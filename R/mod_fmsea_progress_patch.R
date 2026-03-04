#' fMSEA模块进度显示优化补丁
#' 这个文件包含了对原有mod_fmsea.R的进度显示优化
#' 可以直接替换原文件中的相应部分

# =============================================================================
# 1. UI部分的改进 - 在原有的accordion_panel中添加更好的状态显示
# =============================================================================

# 替换原有的step1_status部分：
step1_status_ui_improved <- function(ns) {
  div(
    style = "margin-top: 10px;",

    # 状态卡片
    div(
      id = ns("step1_status_card"),
      class = "card",
      style = "border: 1px solid #e9ecef; border-radius: 8px; background: #f8f9fa;",

      div(
        class = "card-body",
        style = "padding: 12px;",

        div(
          style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 8px;",

          # 状态图标和文字
          div(
            style = "display: flex; align-items: center;",
            div(
              id = ns("step1_icon"),
              style = "margin-right: 8px;",
              icon("circle", class = "text-muted")
            ),
            div(
              id = ns("step1_text"),
              style = "font-weight: 500;",
              "Ready to run"
            )
          ),

          # 时间显示
          div(
            id = ns("step1_time"),
            style = "font-size: 0.85em; color: #666;",
            ""
          )
        ),

        # 进度条
        div(
          id = ns("step1_progress"),
          style = "display: none;",
          div(
            class = "progress",
            style = "height: 6px; margin-bottom: 5px;",
            div(
              id = ns("step1_progress_bar"),
              class = "progress-bar progress-bar-striped progress-bar-animated",
              style = "width: 0%;"
            )
          ),
          div(
            id = ns("step1_detail"),
            style = "font-size: 0.8em; color: #666;",
            ""
          )
        )
      )
    )
  )
}

# 类似的step2状态UI
step2_status_ui_improved <- function(ns) {
  div(
    style = "margin-top: 10px;",

    div(
      id = ns("step2_status_card"),
      class = "card",
      style = "border: 1px solid #e9ecef; border-radius: 8px; background: #f8f9fa;",

      div(
        class = "card-body",
        style = "padding: 12px;",

        div(
          style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 8px;",

          div(
            style = "display: flex; align-items: center;",
            div(
              id = ns("step2_icon"),
              style = "margin-right: 8px;",
              icon("circle", class = "text-muted")
            ),
            div(
              id = ns("step2_text"),
              style = "font-weight: 500;",
              "Waiting for Step 1"
            )
          ),

          div(
            id = ns("step2_time"),
            style = "font-size: 0.85em; color: #666;",
            ""
          )
        ),

        div(
          id = ns("step2_progress"),
          style = "display: none;",
          div(
            class = "progress",
            style = "height: 6px; margin-bottom: 5px;",
            div(
              id = ns("step2_progress_bar"),
              class = "progress-bar progress-bar-striped progress-bar-animated bg-success",
              style = "width: 0%;"
            )
          ),
          div(
            id = ns("step2_detail"),
            style = "font-size: 0.8em; color: #666;",
            ""
          )
        )
      )
    )
  )
}

# =============================================================================
# 2. 服务器端的改进进度更新函数
# =============================================================================

# 进度更新辅助函数
update_step_status <- function(session, ns, step, status, progress = NULL, detail = NULL, elapsed_time = NULL) {

  step_icon_id <- paste0("step", step, "_icon")
  step_text_id <- paste0("step", step, "_text")
  step_time_id <- paste0("step", step, "_time")
  step_progress_id <- paste0("step", step, "_progress")
  step_progress_bar_id <- paste0("step", step, "_progress_bar")
  step_detail_id <- paste0("step", step, "_detail")
  step_card_id <- paste0("step", step, "_status_card")

  # 根据状态更新图标和样式
  if (status == "waiting") {
    icon_html <- '<i class="fas fa-circle text-muted"></i>'
    text_content <- "Ready to run"
    card_style <- "border-color: #e9ecef; background: #f8f9fa;"
    time_content <- ""

  } else if (status == "running") {
    icon_html <- '<i class="fas fa-spinner fa-spin text-primary"></i>'
    text_content <- paste0("Running Step ", step, "...")
    card_style <- "border-color: #007bff; background: #e3f2fd;"
    time_content <- ifelse(!is.null(elapsed_time),
                          paste0("Running: ", format_elapsed_time(elapsed_time)), "")

  } else if (status == "completed") {
    icon_html <- '<i class="fas fa-check-circle text-success"></i>'
    text_content <- paste0("Step ", step, " Completed")
    card_style <- "border-color: #28a745; background: #e8f5e8;"
    time_content <- ifelse(!is.null(elapsed_time),
                          paste0("Completed in ", format_elapsed_time(elapsed_time)), "")

  } else if (status == "error") {
    icon_html <- '<i class="fas fa-exclamation-circle text-danger"></i>'
    text_content <- paste0("Step ", step, " Failed")
    card_style <- "border-color: #dc3545; background: #f8d7da;"
    time_content <- ifelse(!is.null(elapsed_time),
                          paste0("Failed after ", format_elapsed_time(elapsed_time)), "")
  }

  # 更新UI元素
  session$sendCustomMessage("updateHTML", list(
    id = step_icon_id,
    html = icon_html
  ))

  session$sendCustomMessage("updateText", list(
    id = step_text_id,
    text = text_content
  ))

  session$sendCustomMessage("updateText", list(
    id = step_time_id,
    text = time_content
  ))

  session$sendCustomMessage("updateStyle", list(
    id = step_card_id,
    style = card_style
  ))

  # 更新进度条
  if (status == "running" && !is.null(progress)) {
    session$sendCustomMessage("showElement", list(id = step_progress_id))
    session$sendCustomMessage("updateProgress", list(
      id = step_progress_bar_id,
      width = paste0(progress, "%")
    ))

    if (!is.null(detail)) {
      session$sendCustomMessage("updateText", list(
        id = step_detail_id,
        text = detail
      ))
    }

  } else {
    session$sendCustomMessage("hideElement", list(id = step_progress_id))
  }
}

# 时间格式化函数
format_elapsed_time <- function(start_time) {
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  if (elapsed < 60) {
    return(paste0(round(elapsed, 1), "s"))
  } else if (elapsed < 3600) {
    mins <- floor(elapsed / 60)
    secs <- round(elapsed %% 60)
    return(paste0(mins, "m ", secs, "s"))
  } else {
    hours <- floor(elapsed / 3600)
    mins <- floor((elapsed %% 3600) / 60)
    return(paste0(hours, "h ", mins, "m"))
  }
}

# =============================================================================
# 3. 改进的Step 1分析函数
# =============================================================================

run_step1_improved <- function(session, ns, vals, input) {
  observeEvent(input$run_step1, {
    req(vals$feature_table, vals$ms1_db)

    # 记录开始时间
    start_time <- Sys.time()

    # 更新状态为运行中
    update_step_status(session, ns, 1, "running", 0, "Initializing...", start_time)

    # 保存参数
    local_column <- as.character(input$column)
    local_db_type <- as.character(input$database_type)
    local_ppm <- as.numeric(input$ms1_match_ppm)
    local_rt_tol <- as.numeric(input$mfc_rt_tol)
    local_isotope <- as.numeric(input$isotope_number)

    # 使用更详细的withProgress
    withProgress(message = 'Feature Annotation Analysis',
                 detail = 'Preparing analysis...',
                 value = 0, {

      tryCatch({
        # 步骤1: 注释特征表
        incProgress(0.1)
        update_step_status(session, ns, 1, "running", 15,
                          "Annotating features with database...", start_time)

        annotation_table_final <- featuremsea::annotate_feature_table(
          feature_table = vals$feature_table,
          column = local_column,
          metabolite_database = vals$ms1_db,
          database_type = local_db_type,
          ms1_match_ppm = local_ppm,
          mfc_rt_tol = local_rt_tol,
          isotope_number = local_isotope
        )

        # 步骤2: 去除冗余
        incProgress(0.4)
        update_step_status(session, ns, 1, "running", 55,
                          "Removing redundant annotations...", start_time)

        annotation_table_final2 <- featuremsea::remove_redundancy(
          annotation_table = annotation_table_final
        )

        # 步骤3: 处理注释表
        incProgress(0.3)
        update_step_status(session, ns, 1, "running", 85,
                          "Processing annotation results...", start_time)

        results_step1 <- featuremsea::process_annotation_table(
          annotation_table_final2 = annotation_table_final2,
          database_type = local_db_type
        )

        # 保存结果
        vals$ranking_table <- results_step1$ranking_table
        vals$annotation_table <- results_step1$original_score_annotation

        # 完成进度
        incProgress(0.2)
        update_step_status(session, ns, 1, "completed", 100, NULL, start_time)

        # 更新Step 2状态为可运行
        update_step_status(session, ns, 2, "waiting")
        session$sendCustomMessage("updateText", list(
          id = "step2_text",
          text = "Ready to run (Step 1 completed)"
        ))

        # 成功通知
        n_features <- nrow(results_step1$ranking_table)
        showNotification(
          paste0("✓ Feature annotation completed successfully! ",
                 n_features, " features processed in ",
                 format_elapsed_time(start_time)),
          type = "message",
          duration = 5
        )

      }, error = function(e) {
        update_step_status(session, ns, 1, "error", NULL, NULL, start_time)

        showNotification(
          paste("❌ Step 1 failed:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
  })
}

# =============================================================================
# 4. 改进的Step 2分析函数
# =============================================================================

run_step2_improved <- function(session, ns, vals, input) {
  observeEvent(input$run_step2, {
    req(vals$pathway_db, vals$ranking_table, vals$annotation_table)

    start_time <- Sys.time()

    # 更新状态
    update_step_status(session, ns, 2, "running", 0, "Initializing fMSEA...", start_time)

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

    # 估算运行时间
    estimated_time <- round((l_perm_num / 1000) * (nrow(l_rank) / 5000) * 60, 1)

    withProgress(message = 'fMSEA Enrichment Analysis',
                 detail = paste0('Estimated time: ~', estimated_time, ' minutes'),
                 value = 0, {

      tryCatch({
        incProgress(0.1)
        update_step_status(session, ns, 2, "running", 20,
                          paste0("Running fMSEA analysis (", l_perm_num, " permutations)..."),
                          start_time)

        vals$final_result <- featuremsea::perform_fmsea_analysis(
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

        incProgress(0.8)
        update_step_status(session, ns, 2, "completed", 100, NULL, start_time)

        # 计算统计信息
        n_total <- nrow(vals$final_result@pathway_score)
        n_significant <- nrow(vals$final_result@significant_modules)

        showNotification(
          paste0("✓ fMSEA analysis completed in ", format_elapsed_time(start_time),
                 "! Found ", n_significant, " significant pathways out of ",
                 n_total, " tested."),
          type = "success",
          duration = 8
        )

      }, error = function(e) {
        update_step_status(session, ns, 2, "error", NULL, NULL, start_time)

        showNotification(
          paste("❌ fMSEA analysis failed:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
  })
}

# =============================================================================
# 5. 必需的JavaScript消息处理器
# =============================================================================

# 添加到UI头部的JavaScript
progress_javascript <- tags$head(
  tags$script(HTML('
    Shiny.addCustomMessageHandler("updateHTML", function(message) {
      var element = document.getElementById(message.id);
      if (element) {
        element.innerHTML = message.html;
      }
    });

    Shiny.addCustomMessageHandler("updateText", function(message) {
      var element = document.getElementById(message.id);
      if (element) {
        element.textContent = message.text;
      }
    });

    Shiny.addCustomMessageHandler("updateStyle", function(message) {
      var element = document.getElementById(message.id);
      if (element) {
        element.style.cssText = message.style;
      }
    });

    Shiny.addCustomMessageHandler("showElement", function(message) {
      var element = document.getElementById(message.id);
      if (element) {
        element.style.display = "block";
      }
    });

    Shiny.addCustomMessageHandler("hideElement", function(message) {
      var element = document.getElementById(message.id);
      if (element) {
        element.style.display = "none";
      }
    });

    Shiny.addCustomMessageHandler("updateProgress", function(message) {
      var element = document.getElementById(message.id);
      if (element) {
        element.style.width = message.width;
      }
    });
  '))
)

# =============================================================================
# 使用说明：
# 1. 在原mod_fmsea.R的UI部分，替换step1_status和step2_status的uiOutput
# 2. 在服务器部分，替换observeEvent(input$run_step1, ...)和observeEvent(input$run_step2, ...)
# 3. 在UI的tagList顶部添加progress_javascript
# =============================================================================