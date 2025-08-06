#' Database Construction UI
#'
#' @param id Module ID for Shiny.
#' @import shiny
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjqui jqui_resizable
#' @importFrom shinyWidgets materialSwitch
#' @importFrom DT dataTableOutput
#' @noRd
database_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Metabolite Database Construction',
    icon = bs_icon("database"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Data Input",
          icon = bs_icon("folder"),
          # 文件上传控件
          fileInput(
            inputId = ns('metab_zip'),
            label = 'Upload Database ZIP',
            multiple = FALSE,
            accept = '.zip',
            buttonLabel = "Browse...",
            placeholder = "No file selected"
          ),
          helpText("ZIP must contain: metabolite info CSV and NEG/POS folders with mzXML files"),
          uiOutput(ns("upload_status")),
          accordion_panel(
            title = "Construction Parameters",
            icon = bs_icon("gear"),
            textInput(
              ns("version"),
              label = tooltip(
                trigger = list("Version", bs_icon("info-circle")),
                "Database version format: X.X.X"
              ),
              value = "0.0.1"
            ),
            textInput(
              ns("source"),
              label = tooltip(
                trigger = list("Data Source", bs_icon("info-circle")),
                "Organization providing the data (e.g. Shen Lab)"
              ),
              value = "Shen Lab"
            ),
            textInput(
              ns("creater"),
              label = tooltip(
                trigger = list("Creator", bs_icon("info-circle")),
                "Person who created this database"
              ),
              value = "Xiaotao Shen"
            ),
            textInput(
              ns("email"),
              label = tooltip(
                trigger = list("Contact Email", bs_icon("info-circle")),
                "Contact for technical support"
              ),
              value = "xiaotao.shen@outlook.com"
            ),
            numericInput(
              ns("mz_tol"),
              label = tooltip(
                trigger = list("m/z Tolerance (ppm)", bs_icon("info-circle")),
                "Mass-to-charge ratio matching tolerance"
              ),
              value = 15,
              min = 1
            ),
            numericInput(
              ns("rt_tol"),
              label = tooltip(
                trigger = list("RT Tolerance (sec)", bs_icon("info-circle")),
                "Retention time matching tolerance"
              ),
              value = 30,
              min = 1
            ),
            numericInput(
              ns("threads"),
              label = tooltip(
                trigger = list("CPU Threads", bs_icon("info-circle")),
                "Number of parallel workers"
              ),
              value = 3,
              min = 1
            ),
            radioButtons(
              ns("rt"),
              label = tooltip(
                trigger = list("RT Available", bs_icon("info-circle")),
                "Check if retention time data exists"
              ),
              choices = c("TRUE", "FALSE"),
              selected = "TRUE"
            ),
            actionButton(
              ns("build_db"),
              "Build Database",
              icon = icon("hammer"),
              class = "btn-success"
            )
          )
        )
      ),
      page_fluid(
        nav_panel(
          title = "result",
          icon = bs_icon("terminal"),
          textInput(
            ns("db_name"),
            label = tooltip(
              trigger = list("Database Name", bs_icon("info-circle")),
              "The database name"
            ),
            value = "My_inhouse_ms_db_v1"
          ),
          downloadButton(
            outputId = ns("download_db"),
            label = "Download Database",
            icon = icon("download"),style = "width:20%",
          ),
          dataTableOutput(outputId = ns("db_summary"))
        )
      )
    )
  )
}

#' Database Construction Server
#'
#' @param id Module ID for Shiny.
#' @param prj_init Project initialization object containing:
#' \itemize{
#'   \item wd - Working directory path
#' }
#' @import shiny
#' @importFrom shinyjs toggle runjs useShinyjs
#' @importFrom dplyr select left_join
#' @importFrom massdataset activate_mass_dataset
#' @importFrom metid construct_database
#' @importFrom utils unzip
#' @noRd
database_server <- function(id, prj_init) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    db_values <- reactiveValues(
      database = NULL,
      build_log = NULL,
      metab_info_path = NULL,
      dir_path = NULL,
      validation_passed = FALSE
    )

    # 项目初始化状态检查
    project_initialized <- reactive({
      !is.null(prj_init$wd) && dir.exists(prj_init$wd)
    })

    # 显示上传状态
    output$upload_status <- renderUI({
      # 检查项目是否初始化
      if (!project_initialized()) {
        return(tags$div(
          class = "alert alert-danger",
          bsicons::bs_icon("exclamation-triangle"),
          " Project not initialized. Please initialize project first."
        ))
      }

      if (is.null(input$metab_zip)) {
        tags$div(
          class = "alert alert-info",
          bsicons::bs_icon("info-circle"),
          " Please upload a ZIP file"
        )
      } else {
        tags$div(
          class = "alert alert-success",
          bsicons::bs_icon("check-circle"),
          paste("Uploaded:", input$metab_zip$name)
        )
      }
    })

    # 验证ZIP文件内容
    observeEvent(input$metab_zip, {
      # 检查项目是否初始化
      if (!project_initialized()) {
        shinyalert("Error", "Project not initialized. Please initialize project first.", type = "error")
        return()
      }

      req(input$metab_zip)

      # 设置目标目录 (项目目录下的 metabo_db)
      target_dir <- file.path(prj_init$wd, "metabo_db")

      # 如果目录已存在，先删除旧内容
      if (dir.exists(target_dir)) {
        unlink(target_dir, recursive = TRUE)
      }

      # 创建目标目录
      dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)

      # 解压ZIP文件到目标目录
      zip_path <- input$metab_zip$datapath
      unzip(zip_path, exdir = target_dir)

      # 容错处理：删除MAC系统文件和隐藏文件
      clean_extracted_files(target_dir)

      # 查找实际的数据库目录（处理不同压缩结构）
      extracted_dirs <- list.dirs(target_dir, full.names = TRUE, recursive = FALSE)

      # 情况1：直接包含CSV文件和POS/NEG目录
      if (any(grepl("\\.csv$", list.files(target_dir)))) {
        data_dir <- target_dir
      }
      # 情况2：包含一个子目录里面有CSV和POS/NEG
      else {
        # 获取所有一级子目录
        sub_dirs <- list.dirs(target_dir, full.names = TRUE, recursive = FALSE)

        # 查找包含CSV文件的目录
        data_dir_candidates <- sub_dirs[
          sapply(sub_dirs, function(x) {
            any(grepl("\\.csv$", list.files(x)))
          })
        ]

        if (length(data_dir_candidates) > 0) {
          data_dir <- data_dir_candidates[1]
        } else {
          data_dir <- target_dir
        }
      }

      # 检查必需文件
      files <- list.files(data_dir, recursive = TRUE)
      csv_files <- files[grepl("\\.csv$", files)]
      neg_files <- files[grepl("^NEG/.*\\.mzXML$", files)]
      pos_files <- files[grepl("^POS/.*\\.mzXML$", files)]

      # 验证条件
      has_csv <- length(csv_files) > 0
      has_neg <- length(neg_files) > 0
      has_pos <- length(pos_files) > 0
      valid_neg <- if (has_neg) all(grepl("\\.mzXML$", basename(neg_files))) else TRUE
      valid_pos <- if (has_pos) all(grepl("\\.mzXML$", basename(pos_files))) else TRUE

      # 验证结果
      db_values$validation_passed <- has_csv && (has_neg || has_pos) && valid_neg && valid_pos

      # 显示验证状态
      if (!db_values$validation_passed) {
        output$upload_status <- renderUI({
          error_msg <- "❌ Invalid ZIP structure:"
          if (!has_csv) error_msg <- paste(error_msg, "Missing CSV file")
          if (!has_neg && !has_pos) error_msg <- paste(error_msg, "Missing NEG/POS folders")
          if (!valid_neg) error_msg <- paste(error_msg, "NEG folder contains non-mzXML files")
          if (!valid_pos) error_msg <- paste(error_msg, "POS folder contains non-mzXML files")

          tags$div(
            class = "alert alert-danger",
            bsicons::bs_icon("exclamation-triangle"),
            error_msg
          )
        })
      } else {
        # 保存有效路径
        db_values$data_dir <- data_dir
        db_values$metab_info_path <- csv_files[1]  # 取第一个CSV文件

        output$upload_status <- renderUI({
          success_msg <- "✅ ZIP file validated: "
          if (has_pos) success_msg <- paste0(success_msg, length(pos_files), " POS files, ")
          if (has_neg) success_msg <- paste0(success_msg, length(neg_files), " NEG files, ")
          success_msg <- paste0(success_msg, "1 CSV file")

          tags$div(
            class = "alert alert-success",
            bsicons::bs_icon("check-circle-fill"),
            success_msg
          )
        })
      }
    })

    observeEvent(input$build_db, {
      # 检查项目是否初始化
      if (!project_initialized()) {
        shinyalert("Error", "Project not initialized. Please initialize project first.", type = "error")
        return()
      }

      req(db_values$validation_passed, db_values$data_dir, db_values$metab_info_path)

      db_name <- as.character(input$db_name)

      # 输入验证
      if (is.null(db_name) || db_name == "" || !grepl("^[a-zA-Z0-9_]+$", db_name)) {
        shinyalert("Error", "Invalid database name (alphanumeric/underscores only)", type = "error")
        return()
      }

      # 显示进度对话框
      showModal(modalDialog(
        title = tags$div(tags$i(class = "fa fa-spinner fa-spin"), "Building Database"),
        "This may take several minutes. Please be patient...",
        easyClose = FALSE,
        footer = NULL
      ))

      tryCatch({
        # 构建数据库
        db_values$database <- metid::construct_database(
          path = db_values$data_dir,
          metabolite.info.name = db_values$metab_info_path,
          version = input$version,
          source = input$source,
          creater = input$creater,
          email = input$email,
          mz.tol = as.numeric(input$mz_tol),
          rt.tol = as.numeric(input$rt_tol),
          threads = as.numeric(input$threads),
          rt = as.logical(input$rt)
        )

        # 处理结果
        temp_obj <- db_values$database
        db_values$spectra_info <- temp_obj@spectra.info %>% as.data.frame()
        db_values$build_log <- capture.output(show(db_values$database))
        assign(db_name, db_values$database, envir = .GlobalEnv)

        # 显示结果
        output$db_summary <- renderDataTable_formated(
          condition1 = db_values$spectra_info,
          filename.a = "spectra.info",
          tbl = db_values$spectra_info
        )
        shinyalert("Success", "Database built successfully!", type = "success", timer = 5000)
      }, error = function(e) {
        shinyalert("Error", paste("Build failed:", e$message), type = "error", timer = 5000)
      }, finally = {
        removeModal()
      })
    })

    # 下载处理程序
    output$download_db <- downloadHandler(
      filename = function() {
        paste0(input$db_name, ".rda")
      },
      content = function(file) {
        req(db_values$database)
        db_name <- as.character(input$db_name)
        temp_env <- new.env()
        assign(db_name, db_values$database, envir = temp_env)
        save(list = db_name, file = file, envir = temp_env)
      }
    )
  })
}

# 辅助函数：清理解压的文件
clean_extracted_files <- function(dir_path) {
  tryCatch({
    # 删除MAC系统文件夹
    macosx_dirs <- list.dirs(dir_path, recursive = TRUE)[
      grepl("__MACOSX", list.dirs(dir_path, recursive = TRUE))
    ]
    if (length(macosx_dirs) > 0) {
      unlink(macosx_dirs, recursive = TRUE)
    }

    # 删除隐藏文件（以.开头的文件）
    all_files <- list.files(dir_path, all.files = TRUE, full.names = TRUE, recursive = TRUE)
    hidden_files <- all_files[grepl("/\\.[^/]+$", all_files)]
    if (length(hidden_files) > 0) {
      file.remove(hidden_files)
    }

    # 删除空目录
    empty_dirs <- list.dirs(dir_path, recursive = TRUE)
    empty_dirs <- Filter(function(x) length(list.files(x, all.files = TRUE)) == 0, empty_dirs)
    if (length(empty_dirs) > 0) {
      unlink(empty_dirs, recursive = TRUE)
    }
  }, error = function(e) {
    message("Error cleaning extracted files: ", e$message)
  })
}
