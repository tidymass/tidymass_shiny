#' database construction
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjqui jqui_resizable
#' @importFrom shinyFiles shinyFilesButton
#' @importFrom shinyWidgets materialSwitch
#' @importFrom DT dataTableOutput
#' @noRd

database_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Database Construction',
    icon = bs_icon("database"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Data Input",
          icon = bs_icon("folder"),
          shinyFilesButton(
            id = ns('metab_info_file'),
            buttonType = "default",
            title = "choose ms1 info file",
            label = "File path",
            class = NULL,, multiple = FALSE,
            icon = bsicons::bs_icon("files")
          ),
          tags$span(textOutput(outputId = ns("metab_info_path")), class = "text-wrap"),
        ),

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
      ),

      page_fluid(
        navset_card_tab(
          height = 500,
          full_screen = TRUE,
          title = "Construction Output",
          nav_panel(
            title = "Build Log",
            icon = bs_icon("terminal"),
            downloadButton(outputId = ns("download_db"),label = "download",icon = icon("download")),
            verbatimTextOutput(ns("build_log"))
          ),
          nav_panel(
            title = "Database Summary",
            icon = bs_icon("table"),
            verbatimTextOutput(ns("db_summary"))
          )
        )
      )
    )
  )
}



#' database construction
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs toggle runjs useShinyjs
#' @importFrom dplyr select left_join
#' @importFrom massdataset activate_mass_dataset
#' @importFrom shinyFiles shinyDirChoose parseDirPath getVolumes shinyFileChoose parseFilePaths
#' @importFrom plotly renderPlotly plotlyOutput
#' @import metpath
#' @param id module of server
#' @param volumes shinyFiles volumes
#' @noRd

database_server <- function(id,volumes) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    db_values <- reactiveValues(
      database = NULL,
      build_log = NULL
    )

    observe({
      shinyFileChoose(input, "metab_info_file", roots = volumes, session = session)
      if(!is.null(input$metab_info_file)){
        # browser()
        temp_file_text <-parseFilePaths(roots = volumes,  input$metab_info_file)
        output$metab_info_path <- renderText(temp_file_text$datapath)
      }})


    observeEvent(input$build_db, {
      req(input$metab_info_file)

      temp_file_text <-parseFilePaths(roots = volumes,  input$metab_info_file)
      metab_info_file_path <- as.character(temp_file_text$datapath)
      db_values$dir_path = dirname(metab_info_file_path)
      db_values$file_name = basename(metab_info_file_path)


      shinybusy::show_modal_progress_line(
        text = "Constructing database...",
        value = 0.2
      )

      tryCatch({

        db_values$database <- metid::construct_database(
          path = db_values$dir_path,
          metabolite.info.name = db_values$file_name,
          version = input$version,
          source = input$source,
          creater = input$creater,
          email = input$email,
          mz.tol = input$mz_tol %>% as.numeric(),
          rt.tol = input$rt_tol %>% as.numeric(),
          threads = input$threads %>% as.numeric(),
          rt = input$rt %>% as.logical()
        )

        db_values$build_log <- capture.output(show(db_values$database))

        shinybusy::update_modal_progress(0.9)
        showNotification("Database built successfully!", type = "message")

      }, error = function(e) {
        db_values$build_log <- paste("Error:", e$message)
        showNotification(paste("Build failed:", e$message), type = "error")
      }, finally = {
        shinybusy::remove_modal_progress()
      })
    })


    output$build_log <- renderText({
      req(db_values$build_log)
      paste(db_values$build_log, collapse = "\n")
    })


    output$download_db <- downloadHandler(
      filename = function() {
        paste0(db_values$dir_path,"/metabolite-database_", input$version, ".rds")
      },
      content = function(file) {
        req(db_values$database)
        save(db_values$database, file)
      }
    )
  })
}
