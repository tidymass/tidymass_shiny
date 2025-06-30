#' project init UI
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyFiles shinyDirButton shinyFilesButton
#' @importFrom DT dataTableOutput
#' @noRd

project_init_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Initialize project',
    icon = bsicons::bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Working directory",
          icon = bsicons::bs_icon("menu-app"),

          fileInput(
            inputId = ns('SampleInfo'),
            label = 'Sample Information',
            multiple = FALSE,
            accept = '.csv'
          ),

          p('Make sure your file structure is consistent with the following image',style = "color: #7a8788;font-size: 12px; font-style:Italic"),
          img(src = "https://shawnmagic-1257599720.cos.ap-chengdu.myqcloud.com/picgo20240319180805.png",width = "100%"),

          selectInput(
            inputId = ns("sample_id_raw"),label = "sample id",choices = c("sample_id",'injection.order',"class","group"),
            selected = "sample_id",multiple = FALSE
          ),
          selectInput(
            inputId = ns("injection.order_raw"),label = "injection order",choices = c("sample_id",'injection.order',"class","group"),
            selected = "injection.order",multiple = FALSE
          ),
          selectInput(
            inputId = ns("class_raw"),label = "class",choices = c("sample_id",'injection.order',"class","group","batch"),
            selected = "class",multiple = FALSE
          ),
          selectInput(
            inputId = ns("group_raw"),label = "group",choices = c("sample_id",'injection.order',"class","group","batch"),
            selected = "group",multiple = FALSE
          ),
          selectInput(
            inputId = ns("batch_raw"),label = "batch",choices = c("sample_id",'injection.order',"class","group","batch"),
            selected = "batch",multiple = FALSE
          )
        ),
        accordion_panel(
          title = "Resuming task",
          icon = bsicons::bs_icon("repeat"),
          selectInput_div(
            inputId = ns("init_steps"),label = "Choose steps",
            choices = c("none","Remove noisey feature","Remove outlier","impute missing value","Normalization","Annotation","Annotation filtering"),
            selected = "none",multiple = F,
            title = "Choose steps"
          ),
          fileInput(
            inputId = ns('saved_obj_pos'),
            label = 'Positive object',
            multiple = FALSE,
            buttonLabel = "Browse...",
            placeholder = "No file selected",
            accept = '.rda'
          ),
          fileInput(
            inputId = ns('saved_obj_neg'),
            label = 'Negative object',
            multiple = FALSE,
            buttonLabel = "Browse...",
            placeholder = "No file selected",
            accept = '.rda'
          ),
          fileInput(
            inputId = ns('saved_dblist'),
            label = 'Annotation database list',
            multiple = FALSE,
            buttonLabel = "Browse...",
            placeholder = "No file selected",
            accept = '.dblist'
          )
        )
      ),
      page_fluid(
        nav_panel(
          title = "Setting working directory",
          icon = bsicons::bs_icon("power"),
          actionButton(inputId = ns('action_init'),'Initialize project',icon = icon("play"), style = "width: 200px;"),
          tags$h4("Generated working directory:"),
          verbatimTextOutput(ns("generated_wd_path")),
          tags$br(),
          uiOutput(ns("wd_status")),
          tags$br(),
          tags$h3("Summary of input file",style = 'color: black'),
          hr_head(),

          card(
            full_screen = T,
            height = 350,
            card_header(
              "Sample information"
            ),
            DT::dataTableOutput(ns("tbl_sample_info"))
          ),
          tags$h3("Resuming task information",style = 'color: black'),
          hr_head(),
          layout_column_wrap(
            width = 1/2,
            height = 300,
            card(
              full_screen = T,
              height = 350,
              card_header(
                "Positive model"
              ),
              verbatimTextOutput(ns("res_pos_mod"))
            ),
            card(
              full_screen = T,
              height = 350,
              card_header(
                "Negative model"
              ),
              verbatimTextOutput(ns("res_neg_mod"))
            )
          )
        )
      )
    )
  )

}


#' import from raw data of server
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs toggle runjs useShinyjs
#' @importFrom shinyalert shinyalert
#' @importFrom massprocesser process_data
#' @importFrom massdataset mutate_ms2
#' @param id module of server
#' @param volumes shinyFile volumes
#' @param prj_init reactivevalues of project init.
#' @noRd

project_init_server <- function(id, volumes, prj_init) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 添加状态管理变量
    wd_generated <- reactiveVal(FALSE)  # 标记路径是否已生成
    generated_wd <- reactiveVal(NULL)   # 存储生成的路径

    # 生成随机目录的函数
    generate_random_dir <- function() {
      base_dir <- "~/tidymass_results/init"

      # 生成随机字符串（字母+数字，3-10位）
      generate_random_string <- function(length = sample(3:10, 1)) {
        chars <- c(letters, LETTERS, 0:9)
        paste0(sample(chars, length, replace = TRUE), collapse = "")
      }

      # 检查并生成唯一路径
      while (TRUE) {
        rand_string <- generate_random_string()
        new_path <- file.path(base_dir, rand_string)

        # 如果路径不存在，则使用该路径
        if (!dir.exists(new_path)) {
          return(new_path)
        }
      }
    }

    # 显示工作目录状态
    output$wd_status <- renderUI({
      if (wd_generated()) {
        tags$div(
          style = "color: #4CAF50; font-weight: bold;",
          bsicons::bs_icon("check-circle-fill"),
          " Working directory has been initialized. Re-initialization disabled."
        )
      } else {
        tags$div(
          style = "color: #FF9800;",
          bsicons::bs_icon("info-circle"),
          " Ready to initialize project"
        )
      }
    })

    # 显示生成的路径（如果已生成）
    output$generated_wd_path <- renderText({
      if (wd_generated()) {
        generated_wd()
      } else {
        "Working directory will be generated after initialization"
      }
    })

    #> Import sample info from raw file
    sample_info_raw <- reactive({
      file1 <- input$SampleInfo
      if(is.null(file1)){return()}
      read.csv(file = file1$datapath,
               sep=",",header = T,stringsAsFactors = F)
    })

    #> update selectInput contents by read sample infomation table.
    sample_info_col = reactive({
      colnames(sample_info_raw() |> as.data.frame())
    })

    observe({
      if (!is.null(sample_info_col())) {
        updateSelectInput(session, "sample_id_raw", choices = sample_info_col(), selected = sample_info_col()[1])
        updateSelectInput(session, "injection.order_raw", choices = sample_info_col(), selected = sample_info_col()[2])
        updateSelectInput(session, "class_raw", choices = sample_info_col(), selected = sample_info_col()[3])
        updateSelectInput(session, "group_raw", choices = sample_info_col(), selected = sample_info_col()[4])
        updateSelectInput(session, "batch_raw", choices = sample_info_col(), selected = sample_info_col()[5])
      }
    })

    observeEvent(input$action_init, {
      # 如果路径已生成，则阻止重新生成
      if (wd_generated()) {
        shinyalert::shinyalert(
          "Already Initialized",
          "Project has already been initialized. Please refresh the app to start a new project.",
          type = "warning"
        )
        return()
      }

      # 检查样本信息文件
      if(is.null(input$SampleInfo)){
        shinyalert::shinyalert("Error!", "Please upload sample information file", type = "error")
        return()
      }

      tryCatch({
        # 生成随机工作目录
        new_wd <- generate_random_dir()
        generated_wd(new_wd)  # 存储生成的路径
        wd_generated(TRUE)    # 标记路径已生成

        # 设置项目参数
        prj_init$wd_path <- new_wd
        prj_init$wd <- new_wd

        # 创建目录结构
        dir.create(prj_init$wd, showWarnings = FALSE, recursive = TRUE)
        prj_init$mass_dataset_dir <- file.path(prj_init$wd, "mass_dataset")
        dir.create(prj_init$mass_dataset_dir, showWarnings = FALSE, recursive = TRUE)
        prj_init$data_export_dir <- file.path(prj_init$wd, "data_export")
        dir.create(prj_init$data_export_dir, showWarnings = FALSE, recursive = TRUE)

        # Sample info file
        prj_init$sample_id_n = as.character(input$sample_id_raw)
        prj_init$injection.order_n = as.character(input$injection.order_raw)
        prj_init$class_n = as.character(input$class_raw)
        prj_init$group_n = as.character(input$group_raw)
        prj_init$batch_n = as.character(input$batch_raw)
        prj_init$sample_info = as.data.frame(sample_info_raw())

        prj_init$sample_info =
          prj_init$sample_info |>
          dplyr::rename(
            "sample_id" = prj_init$sample_id_n,
            "injection.order" = prj_init$injection.order_n,
            "class" = prj_init$class_n,
            "group" = prj_init$group_n,
            "batch" = prj_init$batch_n
          ) |>
          mutate(batch = as.character(batch))

        prj_init$steps = input$init_steps |> as.character()

        # Sample info
        output$tbl_sample_info =
          renderDataTable_formated(actions = input$action_init,
                                   condition1 = input$SampleInfo,
                                   condition2 = prj_init$sample_info,
                                   tbl = prj_init$sample_info,filename.a = "3.2.Prj_init_sample_info_check")

        # Resuming tasks
        temp_pos_path <- input$saved_obj_pos$datapath
        temp_neg_path <- input$saved_obj_neg$datapath
        saved_dblist <- input$saved_dblist$datapath
        detectedModels <- character(0)
        error_messages <- list()

        # Validate positive file
        if (!is.null(temp_pos_path)) {
          validation_result <- validate_file(temp_pos_path, "positive","uploaded positive file")
          if (!validation_result$success) {
            error_messages <- c(error_messages, validation_result$message)
          } else {
            obj_name_pos = load(temp_pos_path)
            prj_init$object_positive.init <- get(obj_name_pos)
            detectedModels <- c(detectedModels, "positive")
          }
        }

        # Validate negative file
        if (!is.null(temp_neg_path)) {
          validation_result <- validate_file(temp_neg_path, "negative","uploaded negative file")
          if (!validation_result$success) {
            error_messages <- c(error_messages, validation_result$message)
          } else {
            obj_name_neg = load(temp_neg_path)
            prj_init$object_negative.init <- get(obj_name_neg)
            detectedModels <- c(detectedModels, "negative")
          }
        }

        # Validate positive file
        if (prj_init$steps == "Annotation filtering" & !is.null(saved_dblist)) {

          obj_saved_dblist = load(saved_dblist)
          prj_init$dblist <- get(obj_saved_dblist)

        }

        # Error handling
        if (length(error_messages) > 0) {
          error_content <- paste(
            "<div style='max-height: 300px; overflow-y: auto;'>",
            paste("<div style='color: red; margin: 5px 0;'>• ", unlist(error_messages), "</div>", collapse = ""),
            "</div>"
          )
          shinyalert::shinyalert(
            title = "File validation failed.",
            text = HTML(error_content),
            html = TRUE,
            type = "error",
            closeOnEsc = FALSE,
            showCancelButton = FALSE
          )
          return()
        }

        # Auto detect Ion Model
        prj_init$ion_model <- case_when(
          all(c("positive", "negative") %in% detectedModels) ~ "both",
          "positive" %in% detectedModels ~ "positive",
          "negative" %in% detectedModels ~ "negative",
          TRUE ~ "none"
        )

        # Display detection result
        if (prj_init$ion_model != "none") {
          shinyalert::shinyalert(
            title = "Mode detection successful.",
            text = paste0(
              "Detected mode: <b style='color: #2196F3;'>",
              toupper(prj_init$ion_model),
              "</b>\n",
              "Found objects: ",
              ifelse(!is.null(prj_init$object_positive.init),
                     paste0("Positive (", basename(temp_pos_path), ")"), ""),
              ifelse(!is.null(prj_init$object_negative.init),
                     paste0("\nNegative (", basename(temp_neg_path), ")"), "")
            ),
            html = TRUE,
            type = "success",
            animation = "pop"
          )
        } else {
          shinyalert::shinyalert(
            title = "Project initialization successful.",
            text = paste0("Switch to Data Import panel to continue data uploading."),
            html = TRUE,
            type = "success",
            animation = "pop"
          )
        }

        # Status and information of mass datasets
        output$res_pos_mod = check_massdata_info(
          object = prj_init$object_positive.init,
          mode = "positive"
        )

        output$res_neg_mod = check_massdata_info(
          object = prj_init$object_negative.init,
          mode = "negative"
        )

        # 成功提示（覆盖之前的提示）
        shinyalert::shinyalert(
          title = "Project Initialized!",
          text = paste(
            "Working directory created:",
            tags$code(new_wd),
            tags$br(),
            "You can now proceed with the analysis."
          ),
          html = TRUE,
          type = "success"
        )

      }, error = function(e) {
        shinyalert::shinyalert("Initialization Failed",
                               paste("Error:", e$message),
                               type = "error")
      })
    })
  })
}







