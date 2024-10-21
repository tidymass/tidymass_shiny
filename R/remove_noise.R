#' remove noisey features
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


remove_noise_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Remove noisy metabolic features',
    icon = bs_icon("eraser"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "MV percentage of variables",
          icon = bsicons::bs_icon("image"),
          radioButtons(
            inputId = ns("variable_mv_type"),
            label = "Sample group",
            choices = c("QC","Subject","All"),
            selected = "QC"
          ),
          selectInput(
            inputId = ns("color_by"),
            label = "color by",choices = c("mz","rt","no"),
            selected = "mz"
          ),
          actionButton(inputId = ns("vis_butt_1"),label = "Show plot",icon = icon("play"))
        ),
        accordion_panel(
          title = "Noise remove parameters",
          icon = bsicons::bs_icon("gear"),
          sliderInput(
            inputId = ns("qc_cut"),
            label = "MV cutoff of QC samples (%)",
            min = 0,
            max = 100,
            value = 20
          ),
          selectInput(
            inputId = ns("cut_index"),
            label = "Sample group index",
            choices = c("class","group","..."),
            selected = "group"
          ),
          sliderInput(
            inputId = ns("sample_cut"),
            label = "MV cutoff",
            min = 0,
            max = 100,
            value = 50
          ),
          actionButton(
            inputId = ns("mv_start"),
            label = "Start",icon = icon("play")
          )
      )),
      page_fluid(
        nav_panel(title = "remove noise",
          navset_card_tab(
            title = "MV percentage plot",
            height = 400,
            full_screen = TRUE,
            nav_panel(
              "Positive",
              plotOutput(ns("mv_percentage_pos"))
            ),
            nav_panel(
              "Negative",
              plotOutput(ns("mv_percentage_neg"))
            ),
            nav_panel(
              shiny::icon("circle-info"),
              markdown("[show_variable_missing_values](https://www.tidymass.org/docs/chapter6/1-data_cleaning/)")
            )
          ),
          navset_card_tab(
            title = "MV percentage summary",
            height = 400,
            full_screen = TRUE,
            nav_panel(
              "Positive",
              DT::dataTableOutput(outputId = ns("vari_info_pos"))
            ),
            nav_panel(
              "Negative",
              DT::dataTableOutput(outputId = ns("vari_info_neg"))
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
              verbatimTextOutput(ns("obj_mv.pos"))
            ),
            nav_panel(
              "Negative",
              verbatimTextOutput(ns("obj_mv.neg"))
            )
          )
        )
      )
    )
  )
}


#' Remove noise of server
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
#' @param volumes shinyFiles volumes
#' @param prj_init use project init variables.
#' @param data_import_rv reactivevalues mass_dataset export
#' @param data_clean_rv reactivevalues p2 dataclean
#' @param data_export_rv reactivevalues mass_dataset export
#' @noRd


remove_noise_server <- function(id,volumes,prj_init,data_import_rv,data_clean_rv,data_export_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    p2_dataclean <- reactiveValues(data = NULL)


  })
}

