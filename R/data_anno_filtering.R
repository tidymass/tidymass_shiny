#' Metabolite annotation filtering
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


annotation_filter_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Annotation filtering',
    icon = bs_icon("basket"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Remove redundancy",
          icon = bsicons::bs_icon("stars"),
          radioButtons(
            inputId = ns("af_multi_anno"),
            label = "Multiple annotations",
            choices = c("keep all", "keep top total score", "keep the first one"),
            selected = "keep the first one"
          ),
          radioButtons(
            inputId = ns("af_redundancy"),
            label = "Remove redundancy",
            choices = c("keep all", "keep the first one"),
            selected = "keep all"
          ),
          radioButtons(
            inputId = ns("annotation_levels"),
            label = "Annotation levels",
            choices = c("keep all", "keep level 1 and 2"),
            selected = "keep all"
          )
        ),
        accordion_panel(
          title = "Adduct for level 3 annotation",
          icon = bsicons::bs_icon("gear"),
          selectInput_div(
            inputId = ns('af_column'),
            label = "column type",
            choices = c("rp", "hilic"),
            selected = "rp",
            multiple = F,
            title = "column type"
          ),
          selectInput_div(
            inputId = ns('af_Adduct_pos'),
            label = "Addcut positive model",
            choices = "(M+H)+",
            selected = "(M+H)+",
            multiple = T,
            title = "Addcut based on column type"
          ),
          selectInput_div(
            inputId = ns('af_Adduct_neg'),
            label = "Addcut negative model",
            choices = "(M-H)-",
            selected = "(M-H)-",
            multiple = T,
            title = "Addcut based on column type"
          ),
        )
      ),
      page_fluid(
        nav_panel(
          title = "Annotation filtering",
          navset_card_tab(
            title = "Annotation table",
            height = 400,
            full_screen = TRUE,
            nav_panel("Positive", DT::dataTableOutput(
              outputId = ns("Annotation_filtering_pos")
            )),
            nav_panel("Negative", DT::dataTableOutput(
              outputId = ns("Annotation_filtering_neg")
            )),
            nav_panel(
              shiny::icon("circle-info"),
              markdown("description of noise remove method.")
            )
          ),
          navset_card_tab(
            title = "Check compound annotation in other online database",
            height = 400,
            full_screen = TRUE,
            nav_panel("links"),
            nav_panel(
              shiny::icon("circle-info"),
              markdown("description of noise remove method.")
            )
          ),
          layout_column_wrap(
            width = 1 / 2,
            height = 350,
            navset_card_tab(
              title = "Chromatogram (Only available at start with MS file.)",
              height = 350,
              full_screen = TRUE,
              nav_panel("Positive", plotOutput(ns(
                "Chromatogram_pos"
              ))),
              nav_panel("Negative", plotOutput(ns(
                "Chromatogram_neg"
              )))
            ),
            navset_card_tab(
              title = "MS/MS",
              height = 350,
              full_screen = TRUE,
              nav_panel("Positive", plotOutput(ns("MS2_pos"))),
              nav_panel("Negative", plotOutput(ns("MS2_neg")))
            ),
          ),
          navset_card_tab(
            title = "Status",
            height = 400,
            full_screen = TRUE,
            nav_panel("Positive", verbatimTextOutput(ns("obj_af.pos"))),
            nav_panel("Negative", verbatimTextOutput(ns("obj_af.neg")))
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
#' @param id module of server
#' @param volumes shinyFiles volumes
#' @param prj_init use project init variables.
#' @param data_import_rv reactivevalues mass_dataset export
#' @param data_clean_rv reactivevalues p2 dataclean
#' @noRd


annotation_filter_server <-
  function(id,
           volumes,
           prj_init,
           data_import_rv,
           data_clean_rv,
           data_anno) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      p2_dataclean <- reactiveValues(data = NULL)
    })
  }
