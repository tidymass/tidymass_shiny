#' Database Construction UI
#'
#' @param id Module ID for Shiny.
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
    title = 'Metabolite Database Construction',
    icon = bs_icon("database"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Data Input",
          icon = bs_icon("folder"),
          shinyFilesButton(
            id = ns('metab_info_file'),
            buttonType = "default",
            title = "Choose MS1 info file",
            label = "File path",
            class = NULL,
            multiple = FALSE,
            icon = bsicons::bs_icon("files")
          ),
          tags$span(textOutput(outputId = ns("metab_info_path")), class = "text-wrap")
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
#' @param volumes ShinyFiles volumes.
#' @import shiny
#' @importFrom shinyjs toggle runjs useShinyjs
#' @importFrom dplyr select left_join
#' @importFrom massdataset activate_mass_dataset
#' @importFrom shinyFiles shinyDirChoose parseDirPath getVolumes shinyFileChoose parseFilePaths
#' @import metpath
#' @noRd
database_server <- function(id, volumes) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    db_values <- reactiveValues(
      database = NULL,
      build_log = NULL
    )

    observe({
      shinyFileChoose(input, "metab_info_file", roots = volumes, session = session)
      if (!is.null(input$metab_info_file)) {
        temp_file_text <- parseFilePaths(roots = volumes, input$metab_info_file)
        output$metab_info_path <- renderText(temp_file_text$datapath)
      }
    })

    observeEvent(input$build_db, {
      req(input$metab_info_file)
      db_name <- as.character(input$db_name)

      # Input validation for db_name
      if (is.null(db_name) || db_name == "" || !grepl("^[a-zA-Z0-9_]+$", db_name)) {
        showNotification("Please provide a valid database name (alphanumeric and underscores only).", type = "error")
        return()
      }

      temp_file_text <- parseFilePaths(roots = volumes, input$metab_info_file)
      metab_info_file_path <- as.character(temp_file_text$datapath)
      db_values$dir_path <- dirname(metab_info_file_path)
      db_values$file_name <- basename(metab_info_file_path)

      # Show modal progress dialog
      showModal(modalDialog(
        title = tags$div(
          tags$i(class = "fa fa-spinner fa-spin"),
          "Constructing Database"
        ),
        "For MS2 level database construction, this may take a long time. Please be patient.",
        easyClose = FALSE,
        footer = NULL
      ))
      Sys.sleep(time = 15)

      tryCatch({
        db_values$database <- metid::construct_database(
          path = db_values$dir_path,
          metabolite.info.name = db_values$file_name,
          version = input$version,
          source = input$source,
          creater = input$creater,
          email = input$email,
          mz.tol = as.numeric(input$mz_tol),
          rt.tol = as.numeric(input$rt_tol),
          threads = as.numeric(input$threads),
          rt = as.logical(input$rt)
        )
        temp_obj = db_values$database
        db_values$sapectra.info = temp_obj@spectra.info %>% as.data.frame()
        db_values$build_log <- capture.output(show(db_values$database))
        assign(db_name, db_values$database, envir = .GlobalEnv)

        output$db_summary <- renderDataTable_formated(
          condition1 = db_values$sapectra.info,
          filename.a = "spectra.info",
          tbl = db_values$sapectra.info
        )
        shinyalert("Success","Database built successfully!", type = "success",timer = 5000)
      }, error = function(e) {
        shinyalert("Error",paste("Build failed:", e$message), type = "error",timer = 5000)
      }, finally = {
        removeModal()
      })
    })

    # Download handler for .RData file
    output$download_db <- downloadHandler(
      filename = function() {
        paste0(input$db_name, ".rda")
      },
      content = function(file) {
        req(db_values$database)
        db_name <- as.character(input$db_name)
        # Create a temporary environment to avoid polluting .GlobalEnv
        temp_env <- new.env()
        assign(db_name, db_values$database, envir = temp_env)
        save(list = db_name, file = file, envir = temp_env)
      }
    )
  })
}
