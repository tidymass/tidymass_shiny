#' Metabolite annotation
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


feature_annotation_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Metabolite annotation',
    icon = bs_icon("envelope-open-heart"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Annotation parameters",
          icon = bsicons::bs_icon("gear"),
          textInput_div(
            inputId = ns('anno_ms1.match.ppm'),
            label = "ms1.match.ppm ",
            value = 25,
            placeholder = "Only accept number, Precursor match ppm tolerance.",
            title = "Precursor match ppm tolerance."
          ),
          textInput_div(
            inputId = ns('anno_ms2.match.ppm'),
            label = "ms2.match.ppm ",
            value = 30,
            placeholder = "Only accept number, Fragment ion match ppm tolerance.",
            title = "Fragment ion match ppm tolerance."
          ),
          textInput_div(
            inputId = ns('anno_rt.match.tol'),
            label = "rt.match.tol",
            value = 30,
            placeholder = "Only accept number, RT match tolerance.",
            title = "RT match tolerance."
          ),
          textInput_div(
            inputId = ns('anno_candidate.num'),
            label = "candidate.num",
            value = 3,
            placeholder = "Only accept number, The number of candidate.",
            title = "The number of candidate."
          ),
          selectInput_div(
            inputId = ns('anno_column'),
            label = "column",choices = c("rp","hilic"),
            selected = "rp",multiple = FALSE,
            title = "rp: reverse phase \nhilic: HILIC column"
          ),
          textInput_div(
            inputId = ns('anno_threads'),
            label = "threads",
            value = 3,
            placeholder = "Only accept number, The number of threads",
            title = "Number of threads"
          )
        ),
        accordion_panel(
          title = "Database",
          icon = bsicons::bs_icon("database"),
          selectInput_div(
            inputId = ns('norm_db'),
            label = "Public database",
            choices = c(
              "MoNA","Massbank","ReSpect","PlaSMA","MetaboBASE","KEGG","KNApSAcK","Ath_Cyc","Orbitrap","HMDB"
            ),selected = c(
              "MoNA","Massbank","HMDB"
            ),multiple = T,
            title = "Select database"
          ),
          shinyDirButton(id = ns("norm_customized_db"), label = "Choose folder",
                         title = "Customized database path:",
                         buttonType = "default", class = NULL,
                         icon = bs_icon("folder"), multiple = FALSE),
          tags$span(textOutput(outputId = ns("ms_db_folder_selected")), class = "text-wrap"),
          actionButton(inputId = ns('anno_start'),label = "Start annotation",icon = icon("play")),
        )
        ),
      page_fluid(
        nav_panel(title = "Feature annotation",
                  navset_card_tab(
                    title = "Annotation table",
                    height = 400,
                    full_screen = TRUE,
                    nav_panel(
                      "Positive",
                      DT::dataTableOutput(outputId = ns("Annotation_pos"))
                    ),
                    nav_panel(
                      "Negative",
                      DT::dataTableOutput(outputId = ns("Annotation_neg"))
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
                      verbatimTextOutput(ns("obj_anno.pos"))
                    ),
                    nav_panel(
                      "Negative",
                      verbatimTextOutput(ns("obj_anno.neg"))
                    )
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
#' @param data_export_rv reactivevalues mass_dataset export
#' @param data_anno reactivevalues data annotation
#' @noRd


feature_annotation_server <- function(id,volumes,prj_init,data_import_rv,data_clean_rv,data_anno,data_export_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    p2_dataclean <- reactiveValues(data = NULL)



  })
}

