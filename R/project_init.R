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
          shinyDirButton(id = ns("prj_wd"),
                         label = "Set working directory" ,
                         title = "Set working directory:",
                         buttonType = "default", class = NULL,
                         icon = bsicons::bs_icon("folder"), multiple = FALSE),
          tags$span(textOutput(outputId = ns("raw_wd_path")), class = "text-wrap"),

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
            choices = c("none","Remove noisey feature","Remove outlier","impute missing value","Normalization","Annotation","Annotation filtering","Merge data","DAM and rest"),
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
          )
        )
      ),
      page_fluid(
        nav_panel(
          title = "Setting working directory",
          icon = bsicons::bs_icon("power"),
          actionButton(inputId = ns('action_init'),'Initialize project',icon = icon("play"), style = "width: 200px;"),
          tags$h3("Summary of input file",style = 'color: black'),
          hr_head(),
          # htmlOutput(outputId = ns("file_check_init")),
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
#' @importFrom shinyFiles shinyDirChoose parseDirPath getVolumes shinyFileChoose
#' @importFrom shinyalert shinyalert
#' @importFrom massprocesser process_data
#' @importFrom massdataset mutate_ms2
#' @param id module of server
#' @param volumes shinyFile volumes
#' @param prj_init reactivevalues of project init.
#' @noRd


project_init_server <- function(id,volumes,prj_init) {
  moduleServer(id, function(input, output, session) {
    #> set working directory and import sample information

    observe({
      shinyDirChoose(input = input,id = "prj_wd", roots =  volumes, session = session)
      if(!is.null(input$prj_wd)){
        # browser()
        ms1_folder_selected<- parseDirPath(roots = volumes, input$prj_wd)
        output$raw_wd_path <- renderText(ms1_folder_selected)

      }})

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
      updateSelectInput(session, "sample_id_raw",choices = sample_info_col(),selected = sample_info_col()[1])
      updateSelectInput(session, "injection.order_raw",choices = sample_info_col(),selected = sample_info_col()[2])
      updateSelectInput(session, "class_raw",choices = sample_info_col(),selected = sample_info_col()[3])
      updateSelectInput(session, "group_raw",choices = sample_info_col(),selected = sample_info_col()[4])
      updateSelectInput(session, "batch_raw",choices = sample_info_col(),selected = sample_info_col()[5])
    })

    observeEvent(
      input$action_init,
      {
        # Check basic setup
        if(is.null(input$prj_wd)){
          shinyalert::shinyalert("Error!", "Please choose working directory", type = "error")
          return()
        }
        if(is.null(input$SampleInfo)){
          shinyalert::shinyalert("Error!", "Please upload sample information file", type = "error")
          return()
        }

        # Parameters
        prj_init$wd_path <- parseDirPath(volumes, input$prj_wd)
        prj_init$wd <- as.character(prj_init$wd_path)

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
        output$res_pos_mod = renderPrint({
          if(is.null(prj_init$object_positive.init)){return()}
          print(prj_init$object_positive.init)
        })
        output$res_neg_mod = renderPrint({
          if(is.null(prj_init$object_negative.init)){return()}
          print(prj_init$object_negative.init)
        })
      }
    )
  })
}






