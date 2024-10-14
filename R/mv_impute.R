#' Missing value imputation
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


mv_impute_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Missing value imputation',
    icon = bs_icon("person-fill-dash"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Parameters",
          icon = bsicons::bs_icon("gear"),
          selectInput(
            inputId = ns('impute_mv_method'),
            label = "method ",multiple = F,
            choices = c("knn", "rf", "mean", "median", "zero", "minimum", "bpca", "svdImpute",
                        "ppca"),
            selected = 'knn'
          ),
          tags$h4("for knn",style = 'color: #008080'),
          hr_bar(),
          textInput(
            inputId = ns('impute_mv_k'),
            label = "k",
            value = 10
          ),
          sliderInput(
            inputId = ns('impute_mv_rowmax'),
            label = "rowmax",
            min = 0,
            max = 1,
            value = 0.5,step = 0.05
          ),
          sliderInput(
            inputId = ns('impute_mv_colmax'),
            label = "colmax",
            min = 0,
            max = 1,
            value = 0.8,step = 0.05
          ),
          textInput(
            inputId = ns('impute_mv_maxp'),
            label = "maxp",
            value = 1500
          ),
          textInput(
            inputId = ns('impute_mv_rng.seed'),
            label = "rng.seed",
            value = 362436069
          ),
          tags$h4("for missForest (rf)",style = 'color: #008080'),
          hr_bar(),
          textInput(
            inputId = ns('impute_mv_maxiter'),
            label = "maxiter",
            value = 10
          ),
          textInput(
            inputId = ns('impute_mv_ntree'),
            label = "ntree",
            value = 100
          ),
          radioButtons(
            inputId = ns('impute_mv_decreasing'),
            label = "decreasing",choices = c("TRUE","FALSE"),
            selected = "FALSE"
          ),
          tags$h4("for ppca",style = 'color: #008080'),
          hr_bar(),
          textInput(
            inputId = ns('impute_mv_npcs'),
            label = "nPcs",
            value = 2
          ),
          textInput(
            inputId = ns('impute_mv_maxsteps'),
            label = "maxSteps",
            value = 100
          ),
          textInput(
            inputId = ns('impute_mv_threshold'),
            label = "threshold",
            value = 0.0001
          ),
          actionButton(
            inputId = ns("mv_start"),
            label = "Start",icon = icon("play")
          )
        )),
      page_fluid(
        nav_panel(title = "Missing value imputation",
                  navset_card_tab(
                    title = "Expression data preview",
                    height = 400,
                    full_screen = TRUE,
                    nav_panel(
                      "Positive",
                      dataTableOutput(ns("impute_tbl_pos"))
                    ),
                    nav_panel(
                      "Negative",
                      dataTableOutput(ns("impute_tbl_neg"))
                    )
                  ),
                  navset_card_tab(
                    title = "Status",
                    height = 400,
                    full_screen = TRUE,
                    nav_panel(
                      "Positive",
                      verbatimTextOutput(ns("obj_impute.pos"))
                    ),
                    nav_panel(
                      "Negative",
                      verbatimTextOutput(ns("obj_impute.neg"))
                    )
                  )
        )
      )
    )
  )
}


#' Missing value imputation
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


mv_impute_server <- function(id,volumes,prj_init,data_import_rv,data_clean_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    p2_dataclean <- reactiveValues(data = NULL)


  })
}

