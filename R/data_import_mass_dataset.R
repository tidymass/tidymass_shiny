#' import from tbl data of UI
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyFiles shinyDirButton
#' @importFrom DT dataTableOutput
#' @noRd


data_import_massdataset_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Start with mass_dataset',
    icon = bs_icon("upload"),
    layout_sidebar(
      sidebar = sidebar(
        fileInput(
          inputId = ns('Pos_obj_mass'),
          label = 'Positive object',
          multiple = FALSE,
          buttonLabel = "Browse...",
          placeholder = "No file selected",
          accept = '.rda'
        ),
        br(),
        fileInput(
          inputId = ns('Neg_obj_mass'),
          label = 'Negative object',
          multiple = FALSE,
          buttonLabel = "Browse...",
          placeholder = "No file selected",
          accept = '.rda'
        ),
        actionButton(ns('action1.3'),'Check input',icon = icon("play")),
      ),
    page_fluid(
      nav_panel(
        title = "File check",
        icon = bsicons::bs_icon("inbox"),
        navset_card_tab(
          height = 350,
          full_screen = T,
          title = "data preview",
          nav_panel(
            title =  "variable information",
            card(
              full_screen = T,
              height = 350,
              DT::dataTableOutput(ns("obj_variable_info")),
            )
          ),
          nav_panel(
            title =  "expression data",
            card(
              full_screen = T,
              height = 350,
              DT::dataTableOutput(ns("obj_expmat"))
            )
          )
        ),
      ),
      nav_panel(
        title = "Import from mass_dataset",
        icon = bsicons::bs_icon("table"),
        layout_column_wrap(
          width = 1/2,
          height = 350,
          card(
            full_screen = T,
            height = 350,
            card_header(
              "Positive"
            ),
            verbatimTextOutput(ns("obj_mass_check.pos_tbl2"))
          ),
          card(
            full_screen = T,
            height = 350,
            card_header(
              "negative"
            ),
            verbatimTextOutput(ns("obj_mass_check.neg_tbl2"))
          )
        )
      )
    )
    ),
  )

}


#' import from tbl data of server
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs toggle runjs useShinyjs
#' @importFrom shinyFiles shinyDirChoose parseDirPath parseFilePaths
#' @importFrom dplyr select mutate case_when pull mutate_if filter inner_join
#' @importFrom stringr str_detect
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom massdataset mutate_ms2 activate_mass_dataset extract_expression_data extract_variable_info
#' @importFrom magrittr %>%
#' @param id module of server
#' @param volumes shinyFiles volumes
#' @param prj_init use project init variables.
#' @param data_import_rv reactivevalues mass_dataset export
#' @param data_export_rv reactivevalues mass_dataset export
#' @noRd

data_import_massdataset_server <- function(id, volumes, prj_init, data_import_rv, data_export_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #> sidebar2 - from massdataset
    observeEvent(input$toggleSidebar, {
      shinyjs::toggle(id = "Sidebar")
    })

    ## 3.5 import from mass_dataset class -------------------------------------------
    #> File check
    para_obj_check <- reactiveValues(
      data = NULL,
      object.pos = NULL,
      object.neg = NULL
    )



    #> Main processing event
    observeEvent(input$action1.3, {
      req(prj_init$wd)  # Ensure working directory exists

      # Initialize variables
      error_messages <- character()
      detected_models <- character()
      alert_content <- ""
      validation_results <- list()

      # Build alert header
      alert_content <- paste0(
        '<div style="text-align: left; font-size: 14px;">',
        '<h4 style="color: #2c3e50; margin-top: 0;">File Validation Report</h4>'
      )
      # Process Positive File
      if (!is.null(input$Pos_obj_mass)) {
        temp_pos_path <- input$Pos_obj_mass$datapath
        pos_val <- validate_file(temp_pos_path, "positive", "Positive model file")
        validation_results$pos <- pos_val

        alert_content <- paste0(
          alert_content,
          '<div style="margin-bottom: 12px;">',
          '<span style="display: inline-block; width: 30px; color: ',
          ifelse(pos_val$success, '#27ae60', '#e74c3c'), ';">',
          ifelse(pos_val$success, '✓', '✗'), '</span>',
          '<span style="font-weight: 500;">Positive File:</span> ',
          basename(input$Pos_obj_mass$name),
          if (!pos_val$success) paste0(
            '<br><span style="color: #e74c3c; font-size: 12px; padding-left: 30px;">',
            pos_val$message, '</span>'
          ),
          '</div>'
        )

        if (pos_val$success) {
          loaded <- load(temp_pos_path)
          para_obj_check$object.pos <- get(loaded)
          detected_models <- c(detected_models, "positive")
        }
      }

      # Process Negative File
      if (!is.null(input$Neg_obj_mass)) {
        temp_neg_path <- input$Neg_obj_mass$datapath
        neg_val <- validate_file(temp_neg_path, "negative", "Negative model file")
        validation_results$neg <- neg_val

        alert_content <- paste0(
          alert_content,
          '<div style="margin-bottom: 12px;">',
          '<span style="display: inline-block; width: 30px; color: ',
          ifelse(neg_val$success, '#27ae60', '#e74c3c'), ';">',
          ifelse(neg_val$success, '✓', '✗'), '</span>',
          '<span style="font-weight: 500;">Negative File:</span> ',
          basename(input$Neg_obj_mass$name),
          if (!neg_val$success) paste0(
            '<br><span style="color: #e74c3c; font-size: 12px; padding-left: 30px;">',
            neg_val$message, '</span>'
          ),
          '</div>'
        )

        if (neg_val$success) {
          loaded <- load(temp_neg_path)
          para_obj_check$object.neg <- get(loaded)
          detected_models <- c(detected_models, "negative")
        }
      }
      # Add validation summary
      alert_content <- paste0(
        alert_content,
        '<hr style="border-color: #ecf0f1; margin: 15px 0;">',
        '<div style="color: #7f8c8d; font-size: 13px;">',
        '<strong>Detected Models:</strong> ',
        if (length(detected_models) > 0) paste(detected_models, collapse = " + ") else "None",
        '<br><strong>Valid Files:</strong> ',
        sum(sapply(validation_results, function(x) x$success)),
        '/',
        length(validation_results),
        '</div></div>'
      )

      # Show validation alert
      shinyalert::shinyalert(
        title = "File Validation Status",
        text = alert_content,
        html = TRUE,
        type = if (length(detected_models)) "success" else "error",
        closeOnEsc = TRUE,
        showConfirmButton = TRUE,
        confirmButtonText = "OK",
        animation = "pop",
        className = "validation-alert"
      )

      # Exit if no valid models
      if (length(detected_models) == 0) return()

      # Process data for valid models
      # Variable information processing
      vari_list <- list()
      if ("positive" %in% detected_models) {
        vari_list$pos <- para_obj_check$object.pos %>% massdataset::extract_variable_info()
      }
      if ("negative" %in% detected_models) {
        vari_list$neg <- para_obj_check$object.neg %>% massdataset::extract_variable_info()
      }
      para_obj_check$vari_tbl <- do.call(rbind, vari_list)

      # Expression data processing
      exp_list <- list()
      if ("positive" %in% detected_models) {
        exp_list$pos <- para_obj_check$object.pos %>%
          massdataset::extract_expression_data() %>%
          tibble::rownames_to_column("variable_id")
      }
      if ("negative" %in% detected_models) {
        exp_list$neg <- para_obj_check$object.neg %>%
          massdataset::extract_expression_data() %>%
          tibble::rownames_to_column("variable_id")
      }
      para_obj_check$exp_tbl <- do.call(rbind, exp_list)

      # Render tables
      output$obj_variable_info <- renderDataTable_formated(
        tbl = para_obj_check$vari_tbl,
        filename.a = "3.5.mass_dataset_import_vari_info"
      )

      output$obj_expmat <- renderDataTable_formated(
        tbl = para_obj_check$exp_tbl,
        filename.a = "3.5.mass_dataset_import_exp_data"
      )

      # Save objects and show results
      result_content <- '<div style="margin-top: 15px; font-size: 14px;">'

      if ("positive" %in% detected_models) {
        data_import_rv$object_pos_raw <- para_obj_check$object.pos
        object_pos_raw <- data_import_rv$object_pos_raw
        save(object_pos_raw,
             file = file.path(prj_init$mass_dataset_dir, "01.object_pos_raw.rda"))
        result_content <- paste0(
          result_content,
          '<div style="margin-bottom: 10px;">',
          '<span style="color: #3498db; width: 120px; display: inline-block;">Positive Results:</span>',
          '<a href="', URLencode(paste0("file://", prj_init$mass_dataset_dir)),
          '" target="_blank" style="color: #27ae60;">View Directory</a>',
          '</div>'
        )
      }

      if ("negative" %in% detected_models) {
        data_import_rv$object_neg_raw <- para_obj_check$object.neg
        object_neg_raw <- data_import_rv$object_neg_raw
        save(object_neg_raw,
             file = file.path(prj_init$mass_dataset_dir, "01.object_neg_raw.rda"))
        result_content <- paste0(
          result_content,
          '<div style="margin-bottom: 10px;">',
          '<span style="color: #3498db; width: 120px; display: inline-block;">Negative Results:</span>',
          '<a href="', URLencode(paste0("file://", prj_init$mass_dataset_dir)),
          '" target="_blank" style="color: #e74c3c;">View Directory</a>',
          '</div>'
        )
      }

      output$obj_mass_check.pos_tbl2 = check_massdata_info(
        object = data_import_rv$object_pos_raw,
        mode = "positive"
      )

      output$obj_mass_check.neg_tbl2 = check_massdata_info(
        object = data_import_rv$object_neg_raw,
        mode = "negative"
      )

      # Show results alert
      shinyalert::shinyalert(
        title = "Processing Results",
        text = paste0(result_content, '</div>'),
        html = TRUE,
        type = "success",
        closeOnEsc = TRUE,
        showConfirmButton = FALSE,
        timer = 3000,
        animation = "slide-from-top"
      )

    })
  })
}
