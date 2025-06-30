#' Flexible Download Widget UI
#'
#' Creates a fixed panel at the bottom of the page for mass_dataset download
#'
#' @param id Module ID
#' @import shiny
#' @importFrom bsicons bs_icon
#' @noRd

flexible_download_widget_ui <- function(id) {
  ns <- NS(id)
  absolutePanel(
    id = ns("export_footer"),
    class = "card-no-gap",
    draggable = TRUE,
    top = "auto",
    right = 0,
    bottom = 0,
    card(
      card_header(class = "bg-primary text-white", "Download mass_dataset"),
      card_body(
        padding = "10px",
        # Progress indicator
        conditionalPanel(
          condition = paste0("output['", ns("is_processing"), "']"),
          class = "text-center",
          div(
            class = "spinner-border text-primary",
            role = "status",
            tags$span(class = "visually-hidden", "Loading...")
          ),
          tags$p("Preparing download...", class = "text-muted mt-2")
        ),

        # Download button
        conditionalPanel(
          condition = paste0("!output['", ns("is_processing"), "']"),
          div(
            id = ns("download_button"),
            downloadButton(
              ns("download_mass_dataset"),
              "Download All Data",
              icon = icon("download"),
              class = "btn-primary",
              style = "width: 100%;"
            )
          )
        )
      )
    )
  )
}

#' Flexible Download Widget Server
#'
#' Handles the server-side logic for the mass_dataset download
#'
#' @param id Module ID
#' @param prj_init Project initialization object
#' @import shiny
#' @import zip
#' @noRd

flexible_download_widget_server <- function(id, prj_init) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to track processing state
    is_processing <- reactiveVal(FALSE)
    output$is_processing <- reactive(is_processing())
    outputOptions(output, "is_processing", suspendWhenHidden = FALSE)

    # Download mass_dataset folder
    output$download_mass_dataset <- downloadHandler(
      filename = function() {
        paste0("mass_dataset_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        tryCatch({
          is_processing(TRUE)

          # Validate directory exists
          req(prj_init$mass_dataset_dir)
          if (!dir.exists(prj_init$mass_dataset_dir)) {
            stop("mass_dataset directory does not exist")
          }

          # Create temporary ZIP file
          temp_zip <- tempfile(fileext = ".zip")

          # Collect files
          files <- list.files(
            prj_init$mass_dataset_dir,
            full.names = TRUE,
            recursive = TRUE,
            include.dirs = FALSE
          )

          if (length(files) == 0) {
            stop("No files found in mass_dataset directory")
          }

          # Create ZIP
          zip::zipr(
            zipfile = temp_zip,
            files = files,
            root = dirname(prj_init$mass_dataset_dir),
            include_directories = FALSE
          )

          # Copy to download location
          file.copy(temp_zip, file)
        }, error = function(e) {
          showNotification(
            paste("Download failed:", e$message),
            type = "error",
            duration = 10
          )
        }, finally = {
          is_processing(FALSE)
        })
      },
      contentType = "application/zip"
    )
  })
}

