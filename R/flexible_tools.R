#' flexible_tools
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bsicons bs_icon
#' @noRd


flexible_tools_ui = function(id) {
  ns <- NS(id)
  absolutePanel(
    id = "export_footer",
    class = "card-no-gap",
    draggable = TRUE,
    top = "auto",
    right = 0,
    bottom = 0,
    card(
      card_header(class = "bg-dark", "Flexible tools"),
      card_body(
        accordion(
          open = FALSE,
          accordion_panel(
            title = "Data export",
            icon = bs_icon("caret-down-fill"),
            radioButtons(
              inputId = ns("export_format"),
              label = "Data format",
              choices = c("mass_dataset", "readable tables", "both"),
              selected = "both"
            ),
            textInput(
              inputId = ns("export_prefix"),
              label = "Prefix of output filename",
              value = Sys.time()
            ),
            actionButton(inputId = ns("export_button"), label = "Export", icon = icon("file-export"))
          )
        )
      )
    )
  )
}


#' export sever
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs toggle runjs useShinyjs
#' @importFrom dplyr select left_join
#' @importFrom massdataset activate_mass_dataset
#' @importFrom plotly renderPlotly plotlyOutput
#' @param id module of server
#' @param data_export_rv reactivevalues p2 dataclean
#' @param prj_init working data
#' @noRd
#'

flexible_tools_server = function(id,data_export_rv,volumes,prj_init) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(
      input$export_button,
      {
        format = input$export_format %>% as.character()
        file_path = prj_init$wd %>% as.character()
        file_name = input$export_prefix %>% as.character()
        if (is.null(data_export_rv$object_pos)) {return()}
        else {
          export_data_for_shiny(
            object = data_export_rv$object_pos,
            format = format,
            file_path = file_path,
            file_name = file_name
          )
        }
        if (is.null(data_export_rv$object_neg)) {return()}
        else {
          export_data_for_shiny(
            object = data_export_rv$object_neg,
            format = format,
            file_path = file_path,
            file_name = file_name
          )
        }
      }
    )
  }
  )
}
