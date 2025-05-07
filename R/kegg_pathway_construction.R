
#' KEGG Pathway Database UI
#'
#' @param id Module ID for Shiny.
#' @import shiny
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyFiles shinyDirButton
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
          title = "Data Input",
          icon = bs_icon("folder"),
          shinyDirButton(
            id = ns("prj_wd"),
            label = "Set working directory",
            title = "Set working directory:",
            buttonType = "default",
            icon = bsicons::bs_icon("folder"),
            multiple = FALSE
          ),
          tags$span(textOutput(outputId = ns("raw_wd_path")), class = "text-wrap")
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
          verbatimTextOutput(ns("status_log"))
        )
      )
    )
  )
}

#' KEGG Pathway Database Server
#'
#' @param id Module ID for Shiny.
#' @param volumes ShinyFiles volumes.
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyFiles parseDirPath
#' @importFrom shinyalert shinyalert
#' @importFrom progress progress_bar
#' @importFrom purrr map
#' @importFrom KEGGREST keggGet
#' @importFrom massdatabase request_kegg_pathway_info convert_kegg2metpath read_kegg_pathway
#' @noRd
kegg_pathway_server <- function(id, volumes) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    values <- reactiveValues(wd = NULL, log = character())

    # Set working directory
    observe({
      shinyDirChoose(input, "prj_wd", roots = volumes, session = session)
      if (!is.null(input$prj_wd)) {
        wd_path <- parseDirPath(roots = volumes, input$prj_wd)
        output$raw_wd_path <- renderText(wd_path)
        values$wd <- wd_path
      }
    })

    # Handle download and processing
    observeEvent(input$download_pathway, {
      req(values$wd)
      organism <- input$organism
      sleep <- input$sleep
      db_path <- file.path(values$wd, "kegg_pathway_database")

      # Check if database exists
      if (file.exists(db_path)) {
        shinyalert(
          title = "Database Exists",
          text = "KEGG pathway database already exists. Re-download?",
          type = "warning",
          showCancelButton = TRUE,
          confirmButtonText = "Yes",
          cancelButtonText = "No",
          callbackR = function(value) {
            if (value) {
              download_kegg_pathway_with_progress(values$wd, sleep, organism)
            } else {
              process_existing_database(values$wd, organism)
            }
          }
        )
      } else {
        download_kegg_pathway_with_progress(values$wd, sleep, organism)
      }
    })

    # Download function with progress bar
    download_kegg_pathway_with_progress <- function(path, sleep, organism) {
      withProgress(message = "Downloading KEGG Pathway Database", value = 0, {
        dir.create(path, recursive = TRUE, showWarnings = FALSE)
        kegg_id <- massdatabase::request_kegg_pathway_info(organism = organism)
        pb <- progress::progress_bar$new(total = nrow(kegg_id))

        kegg_pathway_database <- seq_along(kegg_id$KEGG.ID) %>%
          purrr::map(function(i) {
            pb$tick()
            incProgress(1/nrow(kegg_id))
            Sys.sleep(time = sleep)
            KEGGREST::keggGet(dbentries = kegg_id$KEGG.ID[i])[[1]]
          })

        save(kegg_pathway_database, file = db_path <- file.path(path, "kegg_pathway_database"))
        values$log <- c(values$log, "Download completed.")
        process_existing_database(path, organism)
      })
    }

    # Read and convert existing database
    process_existing_database <- function(path, organism) {
      withProgress(message = "Processing Pathway Database", value = 0, {
        # Read KEGG pathway database
        kegg_pathway_database <- massdatabase::read_kegg_pathway(path = path)
        values$log <- c(values$log, "Database loaded.")
        incProgress(0.5)

        # Convert to metpath format
        kegg_org_pathway <- massdatabase::convert_kegg2metpath(
          data = kegg_pathway_database,
          path = path,
          threads = 5
        )
        values$log <- c(values$log, "Conversion to metpath format completed.")
        incProgress(0.9)

        # Save with organism-specific name
        save_name <- paste0("kegg_", organism, "_pathway.rda")
        save(kegg_org_pathway, file = file.path(path, save_name))
        values$log <- c(values$log, paste("Saved as", file.path(path, save_name)))

      })
    }

    # Display log
    output$status_log <- renderText({
      print(paste(values$log, collapse = "\n"))
    })
  })
}
