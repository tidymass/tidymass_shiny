#' Metabolite annotation
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjqui jqui_resizable
#' @importFrom shinyFiles shinyDirButton
#' @importFrom shinyWidgets materialSwitch
#' @importFrom DT dataTableOutput
#' @noRd


feature_annotation_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Metabolite annotation',
    icon = bs_icon("envelope-open-heart"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Add MS2 spectra",
          icon = bsicons::bs_icon("menu-app"),
          # 文件来源选择
          radioButtons(
            inputId = ns("ms2_source"),
            label = "Select MS2 source:",
            choices = c("Upload ZIP file" = "upload", "Download from URL" = "url"),
            selected = "upload"
          ),

          # 文件上传区域
          conditionalPanel(
            condition = "input.ms2_source == 'upload'",
            ns = ns,
            fileInput(
              inputId = ns('ms2_zip'),
              label = 'Upload MS2 ZIP file',
              multiple = FALSE,
              accept = '.zip',
              buttonLabel = "Browse...",
              placeholder = "No file selected"
            )
          ),

          # URL 输入区域
          conditionalPanel(
            condition = "input.ms2_source == 'url'",
            ns = ns,
            textInput(
              inputId = ns('ms2_url'),
              label = 'Enter MS2 ZIP URL:',
              placeholder = "https://example.com/ms2_data.zip"
            ),
            helpText("URL must point to a .zip file")
          ),

          # 处理按钮
          actionButton(
            inputId = ns("process_ms2"),
            label = "Process MS2 Files",
            icon = icon("gear"),
            class = "btn-primary",
            width = "100%"
          ),

          # 状态显示
          uiOutput(ns("ms2_status")),

          # 路径显示
          verbatimTextOutput(outputId = ns("MS2_path")),

          # 参数设置
          textInput(
            inputId = ns('column'),label = 'column',value = 'rp'
          ),
          textInput(
            inputId = ns('ms1.ms2.match.rt.tol'),label = 'ms1.ms2.match.rt.tol',value = 15
          ),
          textInput(
            inputId = ns('ms1.ms2.match.mz.tol'),label = 'ms1.ms2.match.mz.tol',value = 30
          ),
          actionButton(inputId = ns("add_ms2"),label = "Add MS2 to Dataset",icon = icon("play"))
        ),
        accordion_panel(
          title = "Annotation parameters",
          icon = bsicons::bs_icon("gear"),
          textInput_div(
            inputId = ns('anno_ms1.match.ppm'),
            label = "ms1.match.ppm ",
            value = 25,
            placeholder = "Only accept number, Precursor match ppm tolerance.",
            title = "Precursor match ppm tolerance."
          ),
          textInput_div(
            inputId = ns('anno_ms2.match.ppm'),
            label = "ms2.match.ppm ",
            value = 30,
            placeholder = "Only accept number, Fragment ion match ppm tolerance.",
            title = "Fragment ion match ppm tolerance."
          ),
          textInput_div(
            inputId = ns('anno_rt.match.tol'),
            label = "rt.match.tol",
            value = 30,
            placeholder = "Only accept number, RT match tolerance.",
            title = "RT match tolerance."
          ),
          textInput_div(
            inputId = ns('anno_candidate.num'),
            label = "candidate.num",
            value = 3,
            placeholder = "Only accept number, The number of candidate.",
            title = "The number of candidate."
          ),
          selectInput_div(
            inputId = ns('anno_column'),
            label = "column",choices = c("rp","hilic"),
            selected = "rp",multiple = FALSE,
            title = "rp: reverse phase \nhilic: HILIC column"
          ),
          textInput_div(
            inputId = ns('anno_threads'),
            label = "threads",
            value = 3,
            placeholder = "Only accept number, The number of threads",
            title = "Number of threads"
          )
        ),
        accordion_panel(
          title = "Optional parameters",
          textInput_div(
            inputId = ns('anno_mz.ppm.thr'),label = "mz.ppm.thr",value = 400,title = "Accurate mass tolerance for m/z error calculation.",placeholder = "numeric"
          ),
          textInput_div(
            inputId = ns('anno_ms2.match.tol'),label = 'ms2.match.tol',value = 0.5,placeholder = "numeric",title = "MS2 match (MS2 similarity) tolerance."
          ),
          textInput_div(
            inputId = ns('anno_fraction.weight'),label = 'fraction.weight',value = 0.3,title = "The weight for matched fragments.",placeholder = "numeric"
          ),
          textInput_div(
            inputId = ns('anno_dp.forward.weight'),label = 'dp.forward.weight',value = 0.6,title = "Forward dot product weight.",placeholder = "numeric"
          ),
          textInput_div(
            inputId = ns('anno_dp.reverse.weight'),label = 'dp.reverse.weight',value = 0.1,title = "Reverse dot product weight.",placeholder = "numeric"
          ),
          hr_head(),
          textInput_div(
            inputId = ns('anno_remove_fragment_intensity_cutoff'),label = 'remove_fragment_intensity_cutoff',value = 0,title = "remove_fragment_intensity_cutoff",placeholder = "numeric"
          ),
          hr_head(),
          textInput_div(
            inputId = ns('anno_ce'),label = 'ce',value = "all",title = "Collision energy. Please confirm the CE values in your database. Default is all",placeholder = "CE model"
          ),
          textInput_div(
            inputId = ns('anno_ms1.match.weight'),label = 'ms1.match.weight',value = 0.25,title = "The weight of MS1 match for total score calculation.",placeholder = "numeric"
          ),
          hr_head(),
          textInput_div(
            inputId = ns('anno_rt.match.weight'),label = 'rt.match.weight',value = 0.25,title = "The weight of RT match for total score calculation.",placeholder = "numeric"
          ),
          textInput_div(
            inputId = ns('anno_ms2.match.weight'),label = 'ms2.match.weight',value = 0.5,title = "The weight of MS2 match for total score calculation.",placeholder = "numeric"
          ),
          textInput_div(
            inputId = ns('anno_total.score.tol'),label = 'total.score.tol',value = 0.5,title = "Total score tolerance. The total score are referring to MS-DIAL.",placeholder = "numeric"
          )
        ),
        accordion_panel(
          title = "Database",
          icon = bsicons::bs_icon("database"),
          selectInput_div(
            inputId = ns('norm_db'),
            label = "Public database",
            choices = c("MoNA","Massbank","HMDB","NULL"),
            selected = c("MoNA","Massbank","HMDB"),
            multiple = TRUE,
            title = "Select database"
          ),
          shinyDirButton(
            id = ns("norm_customized_db"),
            label = "Choose folder",
            title = "Customized database path:",
            buttonType = "default",
            class = NULL,
            icon = bs_icon("folder"),
            multiple = FALSE
          ),
          # 显示选择的路径（添加class确保换行）
          tags$div(
            class = "text-wrap",
            textOutput(outputId = ns("ms_db_folder_selected"))
          ),
          br(),
          actionButton(
            inputId = ns('anno_start'),
            label = "Start annotation",
            icon = icon("play")
          )
        )
      ),
      page_fluid(
        nav_panel(
          title = "Feature annotation",
          htmlOutput(ns("anno_check1_pos")),
          navset_card_tab(
            title = "Annotation table",
            height = 400,
            full_screen = TRUE,
            nav_panel(
              "Positive",
              DT::dataTableOutput(outputId = ns("Annotation_pos"))
            ),
            nav_panel(
              "Negative",
              DT::dataTableOutput(outputId = ns("Annotation_neg"))
            ),
            nav_panel(
              shiny::icon("circle-info"),
              markdown("description of noise remove method.")
            )
          ),
          navset_card_tab(
            title = "Status",
            height = 400,
            full_screen = TRUE,
            nav_panel(
              "Positive",
              verbatimTextOutput(ns("obj_anno.pos"))
            ),
            nav_panel(
              "Negative",
              verbatimTextOutput(ns("obj_anno.neg"))
            )
          )
        )
      )
    )
  )
}



#' Data normalization and integration
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs toggle runjs useShinyjs
#' @importFrom dplyr select left_join
#' @importFrom massdataset activate_mass_dataset
#' @importFrom plotly renderPlotly plotlyOutput
#' @import metid
#' @import massdbbuildin
#' @param id module of server
#' @param volumes shinyFiles volumes
#' @param prj_init use project init variables.
#' @param data_import_rv reactivevalues mass_dataset export
#' @param data_clean_rv reactivevalues p2 dataclean
#' @param data_anno reactivevalues data annotation
#' @noRd


feature_annotation_server <- function(id, volumes, prj_init, data_import_rv, data_clean_rv, data_anno) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 存储MS2路径和处理状态
    ms2_path <- reactiveVal(NULL)
    ms2_processing_status <- reactiveVal("idle")  # idle, processing, success, error

    # 显示MS2处理状态
    output$ms2_status <- renderUI({
      status <- ms2_processing_status()
      path <- ms2_path()

      if (status == "idle") {
        tags$div(
          class = "alert alert-info",
          bsicons::bs_icon("info-circle"),
          " Please process MS2 files first"
        )
      } else if (status == "processing") {
        tags$div(
          class = "alert alert-warning",
          bsicons::bs_icon("hourglass-split"),
          " Processing MS2 files..."
        )
      } else if (status == "success" && !is.null(path)) {
        tags$div(
          class = "alert alert-success",
          bsicons::bs_icon("check-circle-fill"),
          " MS2 files processed successfully"
        )
      } else if (status == "error") {
        tags$div(
          class = "alert alert-danger",
          bsicons::bs_icon("exclamation-triangle-fill"),
          " Error processing MS2 files"
        )
      }
    })

    # 显示MS2路径
    output$MS2_path <- renderText({
      path <- ms2_path()
      if (is.null(path)) {
        "MS2 path not set yet"
      } else {
        path
      }
    })

    # 处理MS2文件
    # 处理MS2文件
    observeEvent(input$process_ms2, {
      tryCatch({
        # 验证项目是否已初始化
        if (is.null(prj_init$wd) || !dir.exists(prj_init$wd)) {
          shinyalert("Error", "Project not initialized. Please initialize project first.", type = "error")
          return()
        }

        # 设置处理状态
        ms2_processing_status("processing")

        # 目标目录
        target_dir <- file.path(prj_init$wd, "MS2")

        # 删除已存在的目录
        if (dir.exists(target_dir)) {
          unlink(target_dir, recursive = TRUE)
        }

        # 创建目录
        dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)

        # 创建临时目录
        temp_dir <- file.path(prj_init$wd, "temp_MS2")
        if (dir.exists(temp_dir)) {
          unlink(temp_dir, recursive = TRUE)
        }
        dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

        # 确定ZIP文件路径
        zip_file <- NULL
        if (input$ms2_source == "upload") {
          req(input$ms2_zip)
          zip_file <- input$ms2_zip$datapath

          # 验证文件类型
          if (!grepl("\\.zip$", input$ms2_zip$name, ignore.case = TRUE)) {
            shinyalert("Error", "Please upload a ZIP file", type = "error")
            ms2_processing_status("error")
            return()
          }
        } else if (input$ms2_source == "url") {
          req(input$ms2_url)
          zip_file <- file.path(temp_dir, "downloaded_ms2.zip")

          # 验证URL格式
          if (!grepl("^https?://", input$ms2_url)) {
            shinyalert("Invalid URL", "URL must start with http:// or https://", type = "error")
            ms2_processing_status("error")
            return()
          }

          withProgress(
            message = 'Downloading MS2 files',
            detail = 'This may take a while...',
            value = 0.3,
            {
              tryCatch({
                download.file(input$ms2_url, zip_file, mode = "wb")
              }, error = function(e) {
                shinyalert("Download Failed", paste("Error details:", e$message), type = "error")
                ms2_processing_status("error")
                return()
              })
            }
          )
        }

        # 解压文件
        withProgress(
          message = 'Extracting ZIP file',
          detail = 'This may take a while...',
          value = 0.6,
          {
            tryCatch({
              unzip(zip_file, exdir = temp_dir)
            }, error = function(e) {
              shinyalert("Extraction Failed", paste("Error details:", e$message), type = "error")
              ms2_processing_status("error")
              return()
            })
          }
        )

        # 删除_MACOSX文件夹（如果存在） - 注意名称可能是"_MACOSX"或"__MACOSX"
        macosx_dir1 <- file.path(temp_dir, "_MACOSX")
        macosx_dir2 <- file.path(temp_dir, "__MACOSX")
        if (dir.exists(macosx_dir1)) {
          unlink(macosx_dir1, recursive = TRUE)
        }
        if (dir.exists(macosx_dir2)) {
          unlink(macosx_dir2, recursive = TRUE)
        }

        # 查找包含 "POS" 和 "NEG" 子目录的文件夹 - 参考MS1处理方式
        pos_dirs <- list.dirs(temp_dir, full.names = TRUE)[
          sapply(list.dirs(temp_dir, full.names = TRUE),
                 function(x) dir.exists(file.path(x, "POS")))
        ]

        neg_dirs <- list.dirs(temp_dir, full.names = TRUE)[
          sapply(list.dirs(temp_dir, full.names = TRUE),
                 function(x) dir.exists(file.path(x, "NEG")))
        ]

        # 移动 "POS" 和 "NEG" 目录到 "MS2"
        if (length(pos_dirs) > 0) {
          for (pos_dir in pos_dirs) {
            # 确保我们移动的是POS目录本身，而不是它的父目录
            if (basename(pos_dir) != "POS") {
              pos_source <- file.path(pos_dir, "POS")
            } else {
              pos_source <- pos_dir
            }

            if (dir.exists(pos_source)) {
              file.rename(pos_source, file.path(target_dir, "POS"))
            }
          }
        }

        if (length(neg_dirs) > 0) {
          for (neg_dir in neg_dirs) {
            # 确保我们移动的是NEG目录本身，而不是它的父目录
            if (basename(neg_dir) != "NEG") {
              neg_source <- file.path(neg_dir, "NEG")
            } else {
              neg_source <- neg_dir
            }

            if (dir.exists(neg_source)) {
              file.rename(neg_source, file.path(target_dir, "NEG"))
            }
          }
        }

        # 验证是否成功移动了POS或NEG目录
        pos_exists <- dir.exists(file.path(target_dir, "POS"))
        neg_exists <- dir.exists(file.path(target_dir, "NEG"))

        if (!pos_exists && !neg_exists) {
          shinyalert("Error", "ZIP must contain POS and/or NEG directories", type = "error")
          ms2_processing_status("error")

          # 提供调试信息
          cat("Debug: Contents of temp_dir:\n")
          print(list.dirs(temp_dir, recursive = FALSE))
          cat("Found POS dirs:", pos_dirs, "\n")
          cat("Found NEG dirs:", neg_dirs, "\n")

          return()
        }

        # 清理临时目录
        unlink(temp_dir, recursive = TRUE)

        # 更新状态
        ms2_path(target_dir)
        ms2_processing_status("success")
        shinyalert("Success", "MS2 files processed successfully", type = "success")

      }, error = function(e) {
        shinyalert("Error", paste("Failed to process MS2 files:", e$message), type = "error")
        ms2_processing_status("error")
      })
    })


    # 自定义数据库路径选择
    observe({
      shinyDirChoose(input = input, id = "norm_customized_db", roots = volumes, session = session)
      if (!is.null(input$norm_customized_db)) {
        db_path <- parseDirPath(roots = volumes, input$norm_customized_db)
        output$ms_db_folder_selected <- renderText(db_path)
      }
    })

    # 工具函数 ----
    check_ion_modes <- function(data_rv, prj) {
      list(
        has_pos = !is.null(data_rv$object_pos_norm) || !is.null(prj$object_positive.init),
        has_neg = !is.null(data_rv$object_neg_norm) || !is.null(prj$object_negative.init)
      )
    }

    # 添加MS2谱图函数 ----
    perform_add_ms2 = function(object, para, polarity, ms2_path) {
      tryCatch({
        # 根据极性确定子目录
        subdir <- if (polarity == "positive") "POS" else "NEG"
        full_path <- file.path(ms2_path, subdir)

        # 验证路径是否存在
        if (!dir.exists(full_path)) {
          shinyalert("Path Error", paste("MS2 directory not found:", full_path), type = "error")
          return(NULL)
        }

        # 添加MS2数据
        res <- object %>%
          mutate_ms2(
            polarity = polarity,
            column = para$column,
            ms1.ms2.match.rt.tol = para$ms1.ms2.match.rt.tol,
            ms1.ms2.match.mz.tol = para$ms1.ms2.match.mz.tol,
            path = full_path
          )
        return(res)
      }, error = function(e) {
        shinyalert("Add MS2 Error", paste("Error details:", e$message), type = "error")
        return(NULL)
      })
    }

    # MS2参数
    para_ms2 =  reactive({
      list(
        column = as.character(input$column),
        ms1.ms2.match.rt.tol = as.numeric(input$ms1.ms2.match.rt.tol),
        ms1.ms2.match.mz.tol = as.numeric(input$ms1.ms2.match.mz.tol)
      )
    })

    # 添加MS2谱图事件 ----
    observeEvent(input$add_ms2, {
      tryCatch({
        # 获取离子模式状态
        modes <- check_ion_modes(data_clean_rv, prj_init)

        # 检查是否有数据
        if (!modes$has_pos && !modes$has_neg) {
          shinyalert("Data Not Loaded", "No positive/negative ion mode data found. Upload data first.", type = "error")
          return()
        }

        # 检查MS2路径是否设置
        if (is.null(ms2_path())) {
          shinyalert("MS2 Path Not Set", "Please process MS2 files first", type = "error")
          return()
        }

        # 获取参数
        para <- para_ms2()

        # 进度条设置
        steps <- character()
        if (modes$has_pos) steps <- c(steps, "Processing positive mode")
        if (modes$has_neg) steps <- c(steps, "Processing negative mode")
        steps <- c(steps, "Saving results")
        total_steps <- length(steps)

        withProgress(message = "Adding MS2 Spectra...", value = 0, {
          # 处理正离子模式
          if (modes$has_pos) {
            incProgress(1/total_steps, detail = steps[1])
            data_anno$object_pos <- perform_add_ms2(
              data_anno$object_pos,
              para,
              "positive",
              ms2_path = ms2_path()
            )

            # 保存结果
            if (!is.null(data_anno$object_pos)) {
              object_pos_ms2 <- data_anno$object_pos
              save(
                object_pos_ms2,
                file = file.path(prj_init$mass_dataset_dir, "06.object_pos_ms2.rda")
              )
            }
          }

          # 处理负离子模式
          if (modes$has_neg) {
            incProgress(1/total_steps, detail = steps[2])
            data_anno$object_neg <- perform_add_ms2(
              data_anno$object_neg,
              para,
              "negative",
              ms2_path = ms2_path()
            )

            # 保存结果
            if (!is.null(data_anno$object_neg)) {
              object_neg_ms2 <- data_anno$object_neg
              save(
                object_neg_ms2,
                file = file.path(prj_init$mass_dataset_dir, "06.object_neg_ms2.rda")
              )
            }
          }

          # 更新状态
          incProgress(1/total_steps, detail = steps[3])
          data_anno$ms2_status <- TRUE
          shinyalert(
            title = "Success",
            text = "MS2 spectra have been successfully added to the dataset",
            type = "success"
          )
        })

      }, error = function(e) {
        shinyalert("Add MS2 Failed", paste("Error details:", e$message), type = "error")
      })
    })

    # 注释参数 ----
    run_annotation <- function(object, para, polarity, database) {
      tryCatch({
        annotate_metabolites_mass_dataset(
          object = object,
          polarity = polarity,
          database = database,
          ms1.match.ppm = para$ms1.match.ppm,
          ms2.match.ppm = para$ms2.match.ppm,
          rt.match.tol = para$rt.match.tol,
          candidate.num = para$candidate.num,
          column = para$column,
          threads = para$threads,
          mz.ppm.thr = para$mz.ppm.thr,
          ms2.match.tol = para$ms2.match.tol,
          fraction.weight = para$fraction.weight,
          dp.forward.weight = para$dp.forward.weight,
          dp.reverse.weight = para$dp.reverse.weight,
          remove_fragment_intensity_cutoff = para$remove_fragment_intensity_cutoff,
          ce = para$ce,
          ms1.match.weight = para$ms1.match.weight,
          rt.match.weight = para$rt.match.weight,
          ms2.match.weight = para$ms2.match.weight,
          total.score.tol = para$total.score.tol
        )
      }, error = function(e) {
        shinyalert("Annotation Error", paste("Error in", polarity, "mode:", e$message), type = "error")
        return(NULL)
      })
    }

    # 检查MS2数据是否存在
    check_ms2 = function(object){
      if (is.null(object) || length(object@ms2_data) == 0) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    }

    # 注释参数
    para_anno = reactive({
      list(
        ms1.match.ppm = as.numeric(input$anno_ms1.match.ppm),
        ms2.match.ppm = as.numeric(input$anno_ms2.match.ppm),
        rt.match.tol = as.numeric(input$anno_rt.match.tol),
        candidate.num = as.numeric(input$anno_candidate.num),
        column = as.character(input$anno_column),
        threads = as.numeric(input$anno_threads),
        norm_db = as.character(input$norm_db),
        mz.ppm.thr = as.numeric(input$anno_mz.ppm.thr),
        ms2.match.tol = as.numeric(input$anno_ms2.match.tol),
        fraction.weight = as.numeric(input$anno_fraction.weight),
        dp.forward.weight = as.numeric(input$anno_dp.forward.weight),
        dp.reverse.weight = as.numeric(input$anno_dp.reverse.weight),
        remove_fragment_intensity_cutoff = as.numeric(input$anno_remove_fragment_intensity_cutoff),
        ce = as.character(input$anno_ce),
        rt.match.weight = as.numeric(input$anno_rt.match.weight),
        ms2.match.weight = as.numeric(input$anno_ms2.match.weight),
        ms1.match.weight = as.numeric(input$anno_ms1.match.weight),
        total.score.tol = as.numeric(input$anno_total.score.tol)
      )
    })

    # 执行注释事件 ----
    observeEvent(input$anno_start, {
      tryCatch({
        shinyjs::disable("anno_start")  # 防止重复点击

        # 获取离子模式状态
        modes <- check_ion_modes(data_clean_rv, prj_init)

        # 检查是否有数据
        if (!modes$has_pos && !modes$has_neg) {
          shinyalert("Data Not Loaded", "No positive/negative ion mode data found. Upload data first.", type = "error")
          return()
        }

        # 检查MS2状态
        if (modes$has_pos && !check_ms2(data_anno$object_pos)) {
          showNotification("Warning: No MS2 data found in positive mode. Annotation will use MS1 only.",
                           type = "warning", duration = 10)
        }
        if (modes$has_neg && !check_ms2(data_anno$object_neg)) {
          showNotification("Warning: No MS2 data found in negative mode. Annotation will use MS1 only.",
                           type = "warning", duration = 10)
        }

        # 获取参数
        para <- para_anno()

        # 准备数据库
        shinyalert(
          title = "Preparing Databases",
          text = "Loading and preparing databases for annotation...",
          type = "info",
          timer = 3000,
          showConfirmButton = FALSE
        )

        # 内置数据库
        data_anno$buildin_db <- list(
          MoNA = mona_ms2,
          Massbank = massbank_ms2,
          HMDB = hmdb_ms2
        )

        # 选择的数据库
        selected_dbs <- para$norm_db
        if ("NULL" %in% selected_dbs) {
          selected_dbs <- selected_dbs[selected_dbs != "NULL"]
        }

        if (length(selected_dbs) == 0) {
          data_anno$buildin_db <- NULL
        } else {
          data_anno$buildin_db <- data_anno$buildin_db[selected_dbs]
        }

        # 自定义数据库
        custom_db_path <- parseDirPath(volumes, input$norm_customized_db)
        if (length(custom_db_path) > 0) {
          custom_db_files <- list.files(custom_db_path, pattern = "\\.rda$", full.names = TRUE)

          if (length(custom_db_files) > 0) {
            data_anno$cuz_db <- lapply(custom_db_files, function(file) {
              load(file)
              get(ls()[1])  # 获取加载的对象
            })
            names(data_anno$cuz_db) <- tools::file_path_sans_ext(basename(custom_db_files))
          } else {
            data_anno$cuz_db <- NULL
          }
        } else {
          data_anno$cuz_db <- NULL
        }

        # 合并数据库
        if (!is.null(data_anno$buildin_db) && !is.null(data_anno$cuz_db)) {
          data_anno$db <- c(data_anno$buildin_db, data_anno$cuz_db)
        } else if (!is.null(data_anno$buildin_db)) {
          data_anno$db <- data_anno$buildin_db
        } else if (!is.null(data_anno$cuz_db)) {
          data_anno$db <- data_anno$cuz_db
        } else {
          shinyalert("No Databases", "Please select at least one database", type = "error")
          return()
        }

        # 保存数据库信息
        db_names <- names(data_anno$db)
        dir.create(file.path(prj_init$wd, "Annotation_Databases"), showWarnings = FALSE)
        save(data_anno$db, file = file.path(prj_init$wd, "Annotation_Databases", "used_databases.rda"))

        # 执行注释
        total_dbs <- length(data_anno$db)
        withProgress(message = "Annotating Metabolites...", value = 0, {
          # 正离子模式注释
          if (modes$has_pos) {
            current_object <- data_anno$object_pos
            for (i in seq_along(data_anno$db)) {
              db_name <- names(data_anno$db)[i]
              incProgress(1/(total_dbs * 2), detail = paste("POS:", db_name))

              current_object <- run_annotation(
                object = current_object,
                para = para,
                polarity = "positive",
                database = data_anno$db[[i]]
              )

              if (is.null(current_object)) break  # 如果出错则停止
            }
            data_anno$object_pos_anno <- current_object
          }

          # 负离子模式注释
          if (modes$has_neg) {
            current_object <- data_anno$object_neg
            for (i in seq_along(data_anno$db)) {
              db_name <- names(data_anno$db)[i]
              incProgress(1/(total_dbs * 2), detail = paste("NEG:", db_name))

              current_object <- run_annotation(
                object = current_object,
                para = para,
                polarity = "negative",
                database = data_anno$db[[i]]
              )

              if (is.null(current_object)) break  # 如果出错则停止
            }
            data_anno$object_neg_anno <- current_object
          }

          # 保存结果
          incProgress(0.1, detail = "Saving results")
          if (modes$has_pos && !is.null(data_anno$object_pos_anno)) {
            object_pos_anno <- data_anno$object_pos_anno
            save(
              object_pos_anno,
              file = file.path(prj_init$mass_dataset_dir, "07.object_pos_anno.rda")
            )
            data_clean_rv$object_pos_anno <- object_pos_anno
          }

          if (modes$has_neg && !is.null(data_anno$object_neg_anno)) {
            object_neg_anno <- data_anno$object_neg_anno
            save(
              object_neg_anno,
              file = file.path(prj_init$mass_dataset_dir, "07.object_neg_anno.rda")
            )
            data_clean_rv$object_neg_anno <- object_neg_anno
          }
        })

        # 更新UI
        output$obj_anno.pos <- renderPrint({
          if (modes$has_pos && !is.null(data_anno$object_pos_anno)) {
            cat("Positive Mode Annotation Summary:\n")
            cat("--------------------------------\n")
            cat("Features:", nrow(data_anno$object_pos_anno), "\n")
            cat("Annotated:", sum(!is.na(data_anno$object_pos_anno@annotation$annotation)), "\n")
            cat("Databases:", paste(names(data_anno$db), collapse = ", "), "\n")
          } else {
            cat("No positive mode data available\n")
          }
        })

        output$obj_anno.neg <- renderPrint({
          if (modes$has_neg && !is.null(data_anno$object_neg_anno)) {
            cat("Negative Mode Annotation Summary:\n")
            cat("--------------------------------\n")
            cat("Features:", nrow(data_anno$object_neg_anno), "\n")
            cat("Annotated:", sum(!is.na(data_anno$object_neg_anno@annotation$annotation)), "\n")
            cat("Databases:", paste(names(data_anno$db), collapse = ", "), "\n")
          } else {
            cat("No negative mode data available\n")
          }
        })

        # 显示注释表格
        output$Annotation_pos <- DT::renderDataTable({
          if (modes$has_pos && !is.null(data_anno$object_pos_anno)) {
            data_anno$object_pos_anno %>%
              extract_annotation_table() %>%
              DT::datatable(options = list(
                pageLength = 10,
                scrollX = TRUE,
                autoWidth = TRUE
              ))
          }
        })

        output$Annotation_neg <- DT::renderDataTable({
          if (modes$has_neg && !is.null(data_anno$object_neg_anno)) {
            data_anno$object_neg_anno %>%
              extract_annotation_table() %>%
              DT::datatable(options = list(
                pageLength = 10,
                scrollX = TRUE,
                autoWidth = TRUE
              ))
          }
        })

        # 显示数据库信息
        output$anno_check1_pos <- renderUI({
          db_list <- paste(names(data_anno$db), collapse = ", ")
          HTML(paste0(
            '<div class="alert alert-success">',
            '<strong>Annotation Completed!</strong> ',
            'Used databases: <span style="color:blue;">', db_list, '</span>',
            '</div>'
          ))
        })

        shinyalert("Success", "Metabolite annotation completed!", type = "success")

      }, error = function(e) {
        shinyalert("Annotation Failed", paste("Error details:", e$message), type = "error")
      }) %finally% {
        shinyjs::enable("anno_start")  # 重新启用按钮
      }
    })
  })
}

