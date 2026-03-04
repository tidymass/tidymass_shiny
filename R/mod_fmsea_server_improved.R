#' 优化后的fMSEA服务器端 - 改进的进度显示系统
#' @noRd

# --- 进度管理辅助函数 ---
ProgressManager <- R6::R6Class("ProgressManager",
  public = list(
    step_id = NULL,
    start_time = NULL,
    session = NULL,
    cancelled = FALSE,

    initialize = function(session, step_id) {
      self$session <- session
      self$step_id <- step_id
      self$start_time <- Sys.time()
      self$cancelled <- FALSE
    },

    update_status = function(status, progress = NULL, detail = NULL, error_msg = NULL) {
      if (self$cancelled) return()

      # 计算运行时间
      elapsed_time <- as.numeric(difftime(Sys.time(), self$start_time, units = "secs"))
      time_str <- self$format_time(elapsed_time)

      # 构建时间信息
      time_info <- switch(status,
        "waiting" = "",
        "running" = paste0("Running: ", time_str),
        "completed" = paste0("Completed in ", time_str),
        "error" = paste0("Failed after ", time_str)
      )

      # 发送JavaScript更新
      self$session$sendCustomMessage("updateProgressStatus", list(
        stepId = self$step_id,
        status = status,
        progress = progress,
        detail = detail,
        timeInfo = time_info,
        errorMsg = error_msg
      ))
    },

    cancel = function() {
      self$cancelled <- TRUE
      self$update_status("error", detail = "Cancelled by user", error_msg = "Analysis was cancelled")
    },

    format_time = function(seconds) {
      if (seconds < 60) {
        return(paste0(round(seconds, 1), "s"))
      } else if (seconds < 3600) {
        mins <- floor(seconds / 60)
        secs <- round(seconds %% 60)
        return(paste0(mins, "m ", secs, "s"))
      } else {
        hours <- floor(seconds / 3600)
        mins <- floor((seconds %% 3600) / 60)
        return(paste0(hours, "h ", mins, "m"))
      }
    }
  )
)

# --- 增强的服务器函数 ---
fmsea_server_improved <- function(id, volumes) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 响应式值
    vals <- reactiveValues(
      feature_table = NULL,
      ms1_db = NULL,
      pathway_db = NULL,
      final_result = NULL,
      ranking_table = NULL,
      annotation_table = NULL,
      feature_table_file_path = NULL,
      ms1_db_file_path = NULL,
      pathway_db_file_path = NULL,
      results_rda_file_path = NULL,

      # 进度管理
      step1_progress = NULL,
      step2_progress = NULL,
      analysis_start_time = NULL,
      step1_cancelled = FALSE,
      step2_cancelled = FALSE
    )

    # 添加自定义消息处理器
    session$sendCustomMessage <- function(type, message) {
      session$sendCustomMessage(type, message)
    }

    # --- 辅助函数 ---
    load_rda_data <- function(path) {
      if (is.null(path) || length(path) == 0 || path == "") return(NULL)
      env <- new.env()
      name <- load(path, envir = env)
      return(get(name, envir = env))
    }

    get_relative_path <- function(full_path) {
      if (is.null(full_path) || length(full_path) == 0) return("")
      path_parts <- strsplit(full_path, "/")[[1]]
      if (length(path_parts) >= 2) {
        paste0(".../ ", paste(tail(path_parts, 2), collapse = "/"))
      } else {
        basename(full_path)
      }
    }

    # --- 文件上传处理 ---
    observe({
      shinyFiles::shinyFileChoose(input, "feature_table_file",
                                  roots = volumes, session = session)
      req(input$feature_table_file)

      file_info <- shinyFiles::parseFilePaths(volumes, input$feature_table_file)
      if (nrow(file_info) > 0) {
        full_path <- as.character(file_info$datapath)
        vals$feature_table_file_path <- full_path

        tryCatch({
          vals$feature_table <- load_rda_data(full_path)
          if (!is.null(vals$feature_table)) {
            showNotification(
              paste0("✓ Feature table loaded: ", nrow(vals$feature_table), " features"),
              type = "message", duration = 3
            )
          }
        }, error = function(e) {
          showNotification(
            paste("Failed to load feature table:", e$message),
            type = "error", duration = 5
          )
        })
      }
    })

    output$feature_table_path <- renderText({
      if (!is.null(vals$feature_table_file_path)) {
        paste0("✓ ", get_relative_path(vals$feature_table_file_path))
      } else {
        "No file selected"
      }
    })

    # 类似的处理其他文件上传...

    # --- Step 1: 注释分析 ---
    observeEvent(input$run_step1, {
      # 验证必需的输入
      if (is.null(vals$feature_table) || is.null(vals$ms1_db)) {
        showNotification(
          "Please upload both Feature Table and MS1 Database files first.",
          type = "warning", duration = 5
        )
        return()
      }

      # 重置取消状态
      vals$step1_cancelled <- FALSE

      # 显示取消按钮
      shinyjs::show("cancel_step1")
      shinyjs::hide("run_step1")

      # 创建进度管理器
      vals$step1_progress <- ProgressManager$new(session, "step1")

      # 保存参数到本地变量
      local_column <- as.character(input$column)
      local_db_type <- as.character(input$database_type)
      local_ppm <- as.numeric(input$ms1_match_ppm)
      local_rt_tol <- as.numeric(input$mfc_rt_tol)
      local_isotope <- as.numeric(input$isotope_number)

      # 异步执行分析
      future::future({
        progress_mgr <- vals$step1_progress

        tryCatch({
          # 开始分析
          progress_mgr$update_status("running", 0, "Initializing annotation...")

          if (progress_mgr$cancelled) return(NULL)

          # 步骤 1: 注释特征表
          progress_mgr$update_status("running", 15, "Annotating feature table...")
          annotation_table_final <- featuremsea::annotate_feature_table(
            feature_table = vals$feature_table,
            column = local_column,
            metabolite_database = vals$ms1_db,
            database_type = local_db_type,
            ms1_match_ppm = local_ppm,
            mfc_rt_tol = local_rt_tol,
            isotope_number = local_isotope
          )

          if (progress_mgr$cancelled) return(NULL)

          # 步骤 2: 去除冗余
          progress_mgr$update_status("running", 55, "Removing redundancy...")
          annotation_table_final2 <- featuremsea::remove_redundancy(
            annotation_table = annotation_table_final
          )

          if (progress_mgr$cancelled) return(NULL)

          # 步骤 3: 处理注释表
          progress_mgr$update_status("running", 85, "Processing annotation table...")
          results_step1 <- featuremsea::process_annotation_table(
            annotation_table_final2 = annotation_table_final2,
            database_type = local_db_type
          )

          if (progress_mgr$cancelled) return(NULL)

          # 完成
          progress_mgr$update_status("completed", 100, "Analysis completed successfully!")

          return(list(
            ranking_table = results_step1$ranking_table,
            annotation_table = results_step1$original_score_annotation,
            success = TRUE
          ))

        }, error = function(e) {
          progress_mgr$update_status("error", error_msg = paste("Error:", e$message))
          return(list(success = FALSE, error = e$message))
        })

      }) %...>% (function(result) {
        # 处理结果
        if (!is.null(result) && result$success) {
          vals$ranking_table <- result$ranking_table
          vals$annotation_table <- result$annotation_table

          showNotification(
            paste0("Step 1 completed! ", nrow(result$ranking_table), " features annotated."),
            type = "message", duration = 5
          )
        } else if (!is.null(result)) {
          showNotification(
            paste("Step 1 failed:", result$error),
            type = "error", duration = 10
          )
        }

        # 恢复按钮状态
        shinyjs::hide("cancel_step1")
        shinyjs::show("run_step1")
      })
    })

    # --- Step 1 取消处理 ---
    observeEvent(input$cancel_step1, {
      vals$step1_cancelled <- TRUE
      if (!is.null(vals$step1_progress)) {
        vals$step1_progress$cancel()
      }

      shinyjs::hide("cancel_step1")
      shinyjs::show("run_step1")

      showNotification("Step 1 analysis cancelled.", type = "warning", duration = 3)
    })

    # --- Step 2: fMSEA 分析 ---
    observeEvent(input$run_step2, {
      # 验证必需数据
      if (is.null(vals$pathway_db) || is.null(vals$ranking_table) || is.null(vals$annotation_table)) {
        showNotification(
          "Please complete Step 1 and upload Pathway Database first.",
          type = "warning", duration = 5
        )
        return()
      }

      # 重置取消状态
      vals$step2_cancelled <- FALSE
      vals$analysis_start_time <- Sys.time()

      # 显示取消按钮
      shinyjs::show("cancel_step2")
      shinyjs::hide("run_step2")

      # 创建进度管理器
      vals$step2_progress <- ProgressManager$new(session, "step2")

      # 保存参数
      l_db <- vals$pathway_db
      l_anno <- vals$annotation_table
      l_rank <- vals$ranking_table
      l_threads <- as.numeric(input$threads)
      l_db_type <- as.character(input$database_type)
      l_min_compounds <- as.numeric(input$min_compounds)
      l_max_compounds <- as.numeric(input$max_compounds)
      l_perm_num <- as.numeric(input$perm_num)
      l_max_iter_num <- as.numeric(input$max_iter_num)
      l_fdr_thr <- as.numeric(input$fdr_thr)

      # 异步执行分析
      future::future({
        progress_mgr <- vals$step2_progress

        tryCatch({
          # 估算总时间（基于经验公式）
          estimated_time <- (l_perm_num / 100) * (nrow(l_rank) / 1000) * 2 # 粗略估算

          progress_mgr$update_status("running", 10,
            paste0("Initializing fMSEA analysis... (est. ",
                   round(estimated_time/60, 1), " min)"))

          if (progress_mgr$cancelled) return(NULL)

          # 执行分析
          progress_mgr$update_status("running", 30, "Performing enrichment analysis...")

          final_result <- featuremsea::perform_fmsea_analysis(
            pathway_database = l_db,
            annotation_table = l_anno,
            ranking_table = l_rank,
            threads = l_threads,
            min.compounds.num = l_min_compounds,
            max.compounds.num = l_max_compounds,
            id.col = ifelse(l_db_type == "KEGG", "KEGG_ID", "HMDB_ID"),
            perm.num = l_perm_num,
            max.iter.num = l_max_iter_num,
            fdr.thr = l_fdr_thr
          )

          if (progress_mgr$cancelled) return(NULL)

          progress_mgr$update_status("running", 90, "Finalizing results...")

          # 计算统计信息
          n_total <- nrow(final_result@pathway_score)
          n_significant <- nrow(final_result@significant_modules)

          progress_mgr$update_status("completed", 100,
            paste0("Analysis completed! ", n_significant, "/", n_total, " pathways significant."))

          return(list(
            result = final_result,
            stats = list(
              total_pathways = n_total,
              significant_pathways = n_significant
            ),
            success = TRUE
          ))

        }, error = function(e) {
          progress_mgr$update_status("error", error_msg = paste("Error:", e$message))
          return(list(success = FALSE, error = e$message))
        })

      }) %...>% (function(result) {
        if (!is.null(result) && result$success) {
          vals$final_result <- result$result

          # 显示结果概览
          shinyjs::show("results_overview")

          showNotification(
            paste0("fMSEA analysis completed! Found ", result$stats$significant_pathways,
                   " significant pathways out of ", result$stats$total_pathways, " tested."),
            type = "success", duration = 8
          )
        } else if (!is.null(result)) {
          showNotification(
            paste("fMSEA analysis failed:", result$error),
            type = "error", duration = 10
          )
        }

        # 恢复按钮状态
        shinyjs::hide("cancel_step2")
        shinyjs::show("run_step2")
      })
    })

    # --- Step 2 取消处理 ---
    observeEvent(input$cancel_step2, {
      vals$step2_cancelled <- TRUE
      if (!is.null(vals$step2_progress)) {
        vals$step2_progress$cancel()
      }

      shinyjs::hide("cancel_step2")
      shinyjs::show("run_step2")

      showNotification("fMSEA analysis cancelled.", type = "warning", duration = 3)
    })

    # --- 结果概览 ---
    output$total_pathways <- renderText({
      req(vals$final_result)
      nrow(vals$final_result@pathway_score)
    })

    output$significant_pathways <- renderText({
      req(vals$final_result)
      nrow(vals$final_result@significant_modules)
    })

    output$analysis_time <- renderText({
      if (!is.null(vals$analysis_start_time)) {
        elapsed <- as.numeric(difftime(Sys.time(), vals$analysis_start_time, units = "secs"))
        if (elapsed < 60) {
          paste0(round(elapsed, 1), "s")
        } else {
          paste0(round(elapsed/60, 1), "m")
        }
      } else {
        "N/A"
      }
    })

    # --- 结果表格（增强版）---
    output$sig_modules_table <- DT::renderDataTable({
      req(vals$final_result)

      df <- vals$final_result@significant_modules

      # 添加一些格式化
      if ("padj" %in% colnames(df)) {
        df$padj <- round(df$padj, 6)
      }
      if ("pval" %in% colnames(df)) {
        df$pval <- round(df$pval, 6)
      }
      if ("ES" %in% colnames(df)) {
        df$ES <- round(df$ES, 3)
      }

      DT::datatable(
        df,
        selection = 'single',
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          scrollY = "300px",  # 增加高度
          pageLength = 10,    # 显示更多行
          dom = 'Bftip',      # 添加按钮
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(
            list(targets = "_all", className = "dt-center"),
            list(
              targets = c(grep("pathway_name|description", colnames(df)) - 1),
              render = DT::JS(
                "function(data, type, row, meta) {
                  return type === 'display' && data !== null && data.length > 30 ?
                    '<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;
                }"
              )
            )
          )
        ),
        extensions = 'Buttons'
      ) %>%
      DT::formatStyle(
        columns = c("padj"),
        backgroundColor = DT::styleInterval(c(0.001, 0.01, 0.05),
                                           c("#e8f5e8", "#fff3cd", "#f8d7da", "#ffffff"))
      )
    })

    # --- 其余的输出和下载处理器... ---
    # （省略部分代码，但包含所有必要的渲染函数）

  })
}