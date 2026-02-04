#' Feature-based Metabolite Set Enrichment Analysis (fMSEA)
#' @import shiny
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @importFrom shinyFiles shinyFilesButton parseFilePaths
#' @noRd
fmsea_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'fMSEA Analysis',
    icon = bsicons::bs_icon("diagram-3"),
    layout_sidebar(
      sidebar = accordion(
        open = c("Data Upload", "Step 1: Annotation", "Step 2: Analysis"),

        # --- 1. Data Upload ---
        accordion_panel(
          title = "Data Upload",
          icon = bsicons::bs_icon("upload"),

          shinyFiles::shinyFilesButton(id = ns('feature_table_file'),
                                       label = 'Feature Table',
                                       title = "Select",
                                       multiple = FALSE,
                                       buttonType = "default"),
          div(textOutput(ns("feature_table_path")),
              style = "font-size: 0.75em; color: #666; margin-top: 5px; margin-bottom: 10px; padding: 5px; background-color: #f8f9fa; border-radius: 3px;"),

          shinyFiles::shinyFilesButton(id = ns('ms1_db_file'),
                                       label = 'MS1 DB',
                                       title = "Select",
                                       multiple = FALSE,
                                       buttonType = "default"),
          div(textOutput(ns("ms1_db_path")),
              style = "font-size: 0.75em; color: #666; margin-top: 5px; margin-bottom: 10px; padding: 5px; background-color: #f8f9fa; border-radius: 3px;"),

          shinyFiles::shinyFilesButton(id = ns('pathway_db_file'),
                                       label = 'Pathway DB',
                                       title = "Select",
                                       multiple = FALSE,
                                       buttonType = "default"),
          div(textOutput(ns("pathway_db_path")),
              style = "font-size: 0.75em; color: #666; margin-top: 5px; margin-bottom: 10px; padding: 5px; background-color: #f8f9fa; border-radius: 3px;"),

          hr(),

          p("Existing Results:", style = "font-size: 0.85em; font-weight: bold; margin-bottom: 5px;"),
          shinyFiles::shinyFilesButton(id = ns('results_rda_file'),
                                       label = 'Load results.rda',
                                       title = "Select RDA",
                                       multiple = FALSE,
                                       buttonType = "info"),
          div(textOutput(ns("results_rda_path")),
              style = "font-size: 0.75em; color: #666; margin-top: 5px; padding: 5px; background-color: #f8f9fa; border-radius: 3px;")
        ),

        # --- 2. Step 1 Parameters ---
        accordion_panel(
          title = "Step 1: Annotation",
          icon = bsicons::bs_icon("1-circle"),

          selectInput(ns("column"), "Column",
                      choices = c("rp", "hilic"),
                      selected = "rp"),

          selectInput(ns("database_type"), "DB Type",
                      choices = c("KEGG", "HMDB"),
                      selected = "KEGG"),

          numericInput(ns("ms1_match_ppm"), "MS1 PPM", value = 15),
          numericInput(ns("mfc_rt_tol"), "RT Tol (s)", value = 10),
          numericInput(ns("isotope_number"), "Isotope No.", value = 3),

          actionButton(ns("run_step1"), "Run Step 1",
                       class = "btn-primary",
                       width = "100%"),

          # 改用 uiOutput 来显示状态信息
          div(style = "margin-top: 10px;",
              uiOutput(ns("step1_status")))
        ),

        # --- 3. Step 2 Parameters ---
        accordion_panel(
          title = "Step 2: fMSEA",
          icon = bsicons::bs_icon("2-circle"),

          numericInput(ns("threads"), "Threads", value = 3, min = 1),
          numericInput(ns("min_compounds"), "Min Compounds", value = 15),
          numericInput(ns("max_compounds"), "Max Compounds", value = 300),
          numericInput(ns("perm_num"), "Permutations", value = 1000),
          numericInput(ns("max_iter_num"), "Max Iterations", value = 1, min = 1, max = 20),
          numericInput(ns("fdr_thr"), "FDR Thr", value = 0.05),

          actionButton(ns("run_step2"), "Run Step 2",
                       class = "btn-success",
                       width = "100%")
        )
      ),

      card(
        full_screen = TRUE,
        card_header(
          div(class = "d-flex justify-content-between align-items-center",
              "Analysis Results",
              downloadButton(ns("download_table"), "Download Table (CSV)",
                             class = "btn-sm"))
        ),
        card_body(
          padding = 0,
          div(
            style = "padding: 10px; border-bottom: 1px solid #eee;",
            h6("Significant Modules (Select to visualize)"),
            DT::dataTableOutput(ns("sig_modules_table"))
          ),
          div(
            style = "padding: 10px;",
            h6("Enrichment Plot"),
            plotOutput(ns("fmsea_plot"), height = "400px"),
            div(
              style = "display: flex; gap: 10px; margin-top: 5px;",
              downloadButton(ns("download_png"), "PNG", class = "btn-sm"),
              downloadButton(ns("download_pdf"), "PDF", class = "btn-sm")
            )
          ),
          accordion(
            open = FALSE,
            accordion_panel("Detailed Summary",
                            verbatimTextOutput(ns("result_summary")))
          )
        )
      )
    )
  )
}

#' fMSEA Server
#' @noRd
fmsea_server <- function(id, volumes) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
      results_rda_file_path = NULL
    )

    load_rda_data <- function(path) {
      if (is.null(path) || length(path) == 0 || path == "") return(NULL)
      env <- new.env()
      name <- load(path, envir = env)
      return(get(name, envir = env))
    }

    # --- 文件路径显示函数 ---
    get_relative_path <- function(full_path) {
      if (is.null(full_path) || length(full_path) == 0) return("")
      # 获取相对路径（从最后两级目录开始）
      path_parts <- strsplit(full_path, "/")[[1]]
      if (length(path_parts) >= 2) {
        paste0(".../ ", paste(tail(path_parts, 2), collapse = "/"))
      } else {
        basename(full_path)
      }
    }

    # --- Feature Table File Handler ---
    observe({
      shinyFiles::shinyFileChoose(input, "feature_table_file",
                                  roots = volumes, session = session)
      req(input$feature_table_file)

      file_info <- shinyFiles::parseFilePaths(volumes, input$feature_table_file)
      if (nrow(file_info) > 0) {
        full_path <- as.character(file_info$datapath)
        vals$feature_table_file_path <- full_path
        vals$feature_table <- load_rda_data(full_path)

        if (!is.null(vals$feature_table)) {
          showNotification("Feature table loaded successfully!",
                           type = "message", duration = 3)
        }
      }
    })

    output$feature_table_path <- renderText({
      if (!is.null(vals$feature_table_file_path)) {
        paste0("✓ ", get_relative_path(vals$feature_table_file_path))
      } else {
        "No file selected"
      }
    })

    # --- MS1 DB File Handler ---
    observe({
      shinyFiles::shinyFileChoose(input, "ms1_db_file",
                                  roots = volumes, session = session)
      req(input$ms1_db_file)

      file_info <- shinyFiles::parseFilePaths(volumes, input$ms1_db_file)
      if (nrow(file_info) > 0) {
        full_path <- as.character(file_info$datapath)
        vals$ms1_db_file_path <- full_path
        vals$ms1_db <- load_rda_data(full_path)

        if (!is.null(vals$ms1_db)) {
          showNotification("MS1 database loaded successfully!",
                           type = "message", duration = 3)
        }
      }
    })

    output$ms1_db_path <- renderText({
      if (!is.null(vals$ms1_db_file_path)) {
        paste0("✓ ", get_relative_path(vals$ms1_db_file_path))
      } else {
        "No file selected"
      }
    })

    # --- Pathway DB File Handler ---
    observe({
      shinyFiles::shinyFileChoose(input, "pathway_db_file",
                                  roots = volumes, session = session)
      req(input$pathway_db_file)

      file_info <- shinyFiles::parseFilePaths(volumes, input$pathway_db_file)
      if (nrow(file_info) > 0) {
        full_path <- as.character(file_info$datapath)
        vals$pathway_db_file_path <- full_path
        vals$pathway_db <- load_rda_data(full_path)

        if (!is.null(vals$pathway_db)) {
          showNotification("Pathway database loaded successfully!",
                           type = "message", duration = 3)
        }
      }
    })

    output$pathway_db_path <- renderText({
      if (!is.null(vals$pathway_db_file_path)) {
        paste0("✓ ", get_relative_path(vals$pathway_db_file_path))
      } else {
        "No file selected"
      }
    })

    # --- Results RDA File Handler ---
    observe({
      shinyFiles::shinyFileChoose(input, "results_rda_file",
                                  roots = volumes, session = session)
      req(input$results_rda_file)

      file_info <- shinyFiles::parseFilePaths(volumes, input$results_rda_file)
      if (nrow(file_info) > 0) {
        full_path <- as.character(file_info$datapath)
        vals$results_rda_file_path <- full_path
        vals$final_result <- load_rda_data(full_path)

        if (!is.null(vals$final_result)) {
          showNotification("Results loaded successfully!",
                           type = "message", duration = 3)
        }
      }
    })

    output$results_rda_path <- renderText({
      if (!is.null(vals$results_rda_file_path)) {
        paste0("✓ ", get_relative_path(vals$results_rda_file_path))
      } else {
        "No file selected"
      }
    })

    # --- Step 1 状态显示 ---
    output$step1_status <- renderUI({
      if (!is.null(vals$ranking_table) && !is.null(vals$annotation_table)) {
        div(
          style = "padding: 10px; background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px; color: #155724;",
          icon("check-circle"),
          " Step 1 completed successfully!"
        )
      } else {
        NULL
      }
    })

    # --- Analysis: Step 1 (带进度条) ---
    observeEvent(input$run_step1, {
      req(vals$feature_table, vals$ms1_db)

      # 参数快照
      local_column <- as.character(input$column)
      local_db_type <- as.character(input$database_type)
      local_ppm <- as.numeric(input$ms1_match_ppm)
      local_rt_tol <- as.numeric(input$mfc_rt_tol)
      local_isotope <- as.numeric(input$isotope_number)

      # 使用 withProgress 显示进度条
      withProgress(message = 'Running Step 1: Annotation', value = 0, {

        tryCatch({
          # 步骤 1: 注释特征表
          incProgress(0.1, detail = "Annotating feature table...")
          annotation_table_final <- featuremsea::annotate_feature_table(
            feature_table = vals$feature_table,
            column = local_column,
            metabolite_database = vals$ms1_db,
            database_type = local_db_type,
            ms1_match_ppm = local_ppm,
            mfc_rt_tol = local_rt_tol,
            isotope_number = local_isotope
          )

          # 步骤 2: 去除冗余
          incProgress(0.4, detail = "Removing redundancy...")
          annotation_table_final2 <- featuremsea::remove_redundancy(
            annotation_table = annotation_table_final
          )

          # 步骤 3: 处理注释表
          incProgress(0.3, detail = "Processing annotation table...")
          results_step1 <- featuremsea::process_annotation_table(
            annotation_table_final2 = annotation_table_final2,
            database_type = local_db_type
          )

          vals$ranking_table <- results_step1$ranking_table
          vals$annotation_table <- results_step1$original_score_annotation

          incProgress(0.2, detail = "Complete!")

          # 成功提示
          showNotification(
            "Step 1 completed successfully! Ready for fMSEA analysis.",
            type = "message",
            duration = 5
          )

        }, error = function(e) {
          showNotification(
            paste("Step 1 Error:", e$message),
            type = "error",
            duration = 10
          )
        })
      })
    })

    # --- Analysis: Step 2 ---
    observeEvent(input$run_step2, {
      req(vals$pathway_db, vals$ranking_table, vals$annotation_table)

      # 关键修复：在 withProgress 之前将所有响应式值保存到本地变量
      # 这样避免在多线程环境中访问响应式上下文
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

      withProgress(message = 'Running Step 2: fMSEA Analysis', value = 0, {
        tryCatch({
          incProgress(0.2, detail = "Initializing analysis...")

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

          incProgress(0.8, detail = "Analysis complete!")

          showNotification(
            "fMSEA analysis completed successfully!",
            type = "message",
            duration = 5
          )

        }, error = function(e) {
          showNotification(
            paste("Step 2 Error:", e$message),
            type = "error",
            duration = 10
          )
        })
      })
    })

    # --- Table & Plot Interaction ---
    output$sig_modules_table <- DT::renderDataTable({
      req(vals$final_result)

      df <- vals$final_result@significant_modules

      DT::datatable(
        df,
        selection = 'single',
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          scrollY = "200px",
          pageLength = 5,
          dom = 'tp',
          columnDefs = list(
            list(
              targets = "_all",
              render = DT::JS(
                "function(data, type, row, meta) {
                  return type === 'display' && data !== null && data.length > 20 ?
                    '' + data.substr(0, 20) + '...' : data;
                }"
              )
            )
          )
        )
      )
    })

    current_plot <- reactive({
      req(vals$final_result, input$sig_modules_table_rows_selected)

      idx <- input$sig_modules_table_rows_selected
      target_id <- vals$final_result@significant_modules$pathway_id[idx]

      featuremsea::plot_fmsea_plot(vals$final_result, target_id)
    })

    output$fmsea_plot <- renderPlot({
      current_plot()
    })

    # --- Download Handlers ---
    output$download_table <- downloadHandler(
      filename = function() {
        paste0("fMSEA_Results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(vals$final_result@significant_modules, file, row.names = FALSE)
      }
    )

    output$download_png <- downloadHandler(
      filename = function() {
        paste0("fMSEA_Plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = current_plot(),
                        device = "png", width = 8, height = 6)
      }
    )

    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("fMSEA_Plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = current_plot(),
                        device = "pdf", width = 8, height = 6)
      }
    )

    output$result_summary <- renderPrint({
      req(vals$final_result)
      print(vals$final_result)
    })
  })
}
