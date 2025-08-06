#' KEGG Pathway Database UI
#'
#' @param id Module ID for Shiny.
#' @import shiny
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyalert useShinyalert
#' @importFrom progress progress_bar
#' @importFrom purrr map
#' @importFrom KEGGREST keggGet
#' @noRd
kegg_pathway_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'KEGG Pathway Database Construction',
    icon = bs_icon("diagram-3"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Project Status",
          icon = bs_icon("info-circle"),
          uiOutput(ns("project_status"))  # 显示项目状态
        ),
        accordion_panel(
          title = "Parameters",
          icon = bs_icon("gear"),
          textInput(
            ns("organism"),
            label = tooltip(
              trigger = list("Organism Code", bs_icon("info-circle")),
              "Enter the KEGG organism code (e.g., hsa for human)"
            ),
            value = "hsa"
          ),
          tags$p(
            "Find organism codes at ",
            tags$a(
              href = "https://www.genome.jp/kegg/tables/br08606.html",
              target = "_blank",
              "KEGG Organism Codes"
            ),
            " (opens in a new tab)"
          ),
          numericInput(
            ns("sleep"),
            label = tooltip(
              trigger = list("Sleep Time (sec)", bs_icon("info-circle")),
              "Pause time between KEGG API requests"
            ),
            value = 1,
            min = 0.1,
            max = 10,
            step = 0.1
          ),
          actionButton(
            ns("download_pathway"),
            "Download Pathway Database",
            icon = icon("download"),
            class = "btn-success"
          )
        )
      ),
      page_fluid(
        nav_panel(
          title = "Summary of pathway database",
          icon = bs_icon("terminal"),
          verbatimTextOutput(ns("status_log")),
          uiOutput(ns("download_ui"))  # 动态显示下载按钮
        )
      )
    )
  )
}

#' KEGG Pathway Database Server
#'
#' @param id Module ID for Shiny.
#' @param prj_init Project initialization object containing:
#' \itemize{
#'   \item wd - Working directory path
#' }
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyalert shinyalert
#' @importFrom progress progress_bar
#' @importFrom purrr map
#' @importFrom KEGGREST keggGet
#' @importFrom massdatabase request_kegg_pathway_info convert_kegg2metpath read_kegg_pathway
#' @noRd
kegg_pathway_server <- function(id, prj_init) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    values <- reactiveValues(
      log = character(),
      final_db_path = NULL  # 存储最终生成的数据库路径
    )

    # 项目初始化状态检查
    project_initialized <- reactive({
      !is.null(prj_init$wd) && dir.exists(prj_init$wd)
    })

    # 显示项目状态
    output$project_status <- renderUI({
      if (!project_initialized()) {
        tags$div(
          class = "alert alert-danger",
          bsicons::bs_icon("exclamation-triangle"),
          " Project not initialized. Please initialize project first."
        )
      } else {
        # 显示相对路径（去除项目根目录）
        relative_path <- gsub(paste0(prj_init$wd, "/?"), "", file.path(prj_init$wd, "kegg_pathway_database"))

        tags$div(
          class = "alert alert-success",
          bsicons::bs_icon("folder"),
          tags$strong("Project directory: "),
          tags$br(),
          tags$code(relative_path),
          tags$br(),
          tags$strong("Database path: "),
          tags$br(),
          tags$code(file.path(relative_path, "kegg_pathway_database"))
        )
      }
    })

    # 显示下载按钮
    output$download_ui <- renderUI({
      req(values$final_db_path)

      # 获取相对路径用于显示
      relative_path <- gsub(paste0(prj_init$wd, "/?"), "", values$final_db_path)

      tagList(
        tags$div(
          class = "alert alert-info",
          bsicons::bs_icon("info-circle"),
          " Pathway database created: ",
          tags$code(relative_path)
        ),
        downloadButton(
          outputId = ns("download_db"),
          label = "Download Pathway Database",
          icon = icon("download"),
          class = "btn-primary",
          style = "width: 100%;"
        )
      )
    })

    # 处理下载按钮
    output$download_db <- downloadHandler(
      filename = function() {
        basename(values$final_db_path)
      },
      content = function(file) {
        req(values$final_db_path)
        file.copy(values$final_db_path, file)
      }
    )

    # 处理下载和转换
    observeEvent(input$download_pathway, {
      # 检查项目是否初始化
      if (!project_initialized()) {
        shinyalert("Error", "Project not initialized. Please initialize project first.", type = "error")
        return()
      }

      organism <- input$organism
      sleep <- input$sleep
      db_dir <- file.path(prj_init$wd, "kegg_pathway_database")
      db_file <- file.path(db_dir, "kegg_pathway_database")

      # 检查数据库是否存在
      if (file.exists(db_file)) {
        shinyalert(
          title = "Database Exists",
          text = "KEGG pathway database already exists. Re-download?",
          type = "warning",
          showCancelButton = TRUE,
          confirmButtonText = "Yes",
          cancelButtonText = "No",
          callbackR = function(value) {
            if (value) {
              download_kegg_pathway_with_progress(db_dir, db_file, sleep, organism)
            } else {
              process_existing_database(db_dir, organism)
            }
          }
        )
      } else {
        download_kegg_pathway_with_progress(db_dir, db_file, sleep, organism)
      }
    })

    # 带进度条的下载函数
    download_kegg_pathway_with_progress <- function(db_dir, db_file, sleep, organism) {
      withProgress(message = "Downloading KEGG Pathway Database", value = 0, {
        # 创建目录
        dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)

        # 获取KEGG通路ID
        tryCatch({
          kegg_id <- massdatabase::request_kegg_pathway_info(organism = organism)
          values$log <- c(values$log, paste("Found", nrow(kegg_id), "pathways for", organism))

          # 设置进度条
          total_pathways <- nrow(kegg_id)
          pb <- progress::progress_bar$new(
            format = "[:bar] :percent | :eta remaining",
            total = total_pathways
          )

          # 下载通路数据
          kegg_pathway_database <- vector("list", total_pathways)
          for (i in seq_along(kegg_id$KEGG.ID)) {
            tryCatch({
              kegg_pathway_database[[i]] <- KEGGREST::keggGet(dbentries = kegg_id$KEGG.ID[i])[[1]]
              values$log <- c(values$log, paste("Downloaded:", kegg_id$KEGG.ID[i]))
            }, error = function(e) {
              values$log <- c(values$log, paste("Error downloading", kegg_id$KEGG.ID[i], ":", e$message))
            })

            # 更新进度
            pb$tick()
            incProgress(1/total_pathways, detail = paste("Pathway", i, "of", total_pathways))
            Sys.sleep(time = sleep)
          }

          # 保存数据库
          save(kegg_pathway_database, file = db_file)
          values$log <- c(values$log, paste("Database saved to:", db_file))

          # 处理数据库
          process_existing_database(db_dir, organism)
        }, error = function(e) {
          shinyalert("Download Error", paste("Failed to download pathway database:", e$message), type = "error")
          values$log <- c(values$log, paste("Error:", e$message))
        })
      })
    }

    # 读取和转换现有数据库
    process_existing_database <- function(db_dir, organism) {
      withProgress(message = "Processing Pathway Database", value = 0, {
        tryCatch({
          # 读取KEGG通路数据库
          kegg_pathway_database <- massdatabase::read_kegg_pathway(path = db_dir)
          values$log <- c(values$log, "Database loaded successfully.")
          incProgress(0.3)

          # 转换为metpath格式
          kegg_org_pathway <- massdatabase::convert_kegg2metpath(
            data = kegg_pathway_database,
            path = db_dir,
            threads = 5
          )
          values$log <- c(values$log, "Conversion to metpath format completed.")
          incProgress(0.6)

          # 保存为特定生物名称
          save_name <- paste0("kegg_", organism, "_pathway.rda")
          save_path <- file.path(db_dir, save_name)
          save(kegg_org_pathway, file = save_path)
          values$log <- c(values$log, paste("Saved as", save_path))

          # 存储最终路径用于下载
          values$final_db_path <- save_path
          incProgress(0.9)

          # 最终状态
          values$log <- c(values$log, "Processing completed successfully!")
          shinyalert("Success", "Pathway database processed successfully!", type = "success", timer = 5000)
        }, error = function(e) {
          shinyalert("Processing Error", paste("Failed to process pathway database:", e$message), type = "error")
          values$log <- c(values$log, paste("Error:", e$message))
        })
      })
    }

    # 显示日志
    output$status_log <- renderText({
      paste(values$log, collapse = "\n")
    })
  })
}
