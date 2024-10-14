#' Data normalization and integration
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


data_normalize_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Data normalization and integration',
    icon = bs_icon("person-fill-dash"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Normalization parameters",
          icon = bsicons::bs_icon("gear"),
          selectInput(
            inputId = ns('norm_method'),
            label = "method ",multiple = F,
            choices = c("svr", "total", "median", "mean", "pqn", "loess","ppca"),
            selected = 'svr'
          ),
          radioButtons(
            inputId = ns('norm_keep_scale'),
            label = "keep_scale",choices = c("TRUE","FALSE"),selected = "TRUE"
          ),
          radioButtons(
            inputId = ns('norm_optimization'),
            label = "optimization",choices = c("TRUE","FALSE"),selected = "TRUE"
          ),
          radioButtons(
            inputId = ns('norm_pqn_reference'),
            label = "pqn_reference",choices = c("median","mean"),selected = "median"
          ),
          textInput(
            inputId = ns('norm_begin'),
            label = "begin",
            value = 0.5
          ),
          textInput(
            inputId = ns('norm_end'),
            label = "end",
            value = 1
          ),
          textInput(
            inputId = ns('norm_step'),
            label = "step",
            value = 0.2
          ),
          textInput(
            inputId = ns('norm_multiple'),
            label = "multiple",
            value = 1
          ),
          textInput(
            inputId = ns('norm_threads'),
            label = "threads",
            value = 1
          ),
          actionButton(
            inputId = ns("mv_start"),
            label = "Start",icon = icon("play")
          )
        ),
        accordion_panel(
          title = "Visualize parameters",
          icon = bsicons::bs_icon("image"),
          selectInput(
            inputId = ns('pca_col_by'),
            label = "pca colored by",
            choices = "class",selected = "class",multiple = F
          ),
          actionButton(inputId = ns("vis_butt"),label = "Show plot",icon = icon("play")),
          materialSwitch(inputId = ns("Interactive_butt"),label = "Interactive plot", status = "primary")
        )),
      page_fluid(
        nav_panel(title = "remove outlier",
                  navset_card_tab(
                    title = "Expression data",
                    height = 400,
                    full_screen = TRUE,
                    nav_panel(
                      "Positive",
                      DT::dataTableOutput(outputId = ns("norm_expdata_pos"))
                    ),
                    nav_panel(
                      "Negative",
                      DT::dataTableOutput(outputId = ns("norm_expdata_neg"))
                    ),
                    nav_panel(
                      shiny::icon("circle-info"),
                      markdown("description of noise remove method.")
                    )
                  ),

                  layout_column_wrap(
                    width = 1/2,
                    height = 350,
                    navset_card_tab(
                      title = "PCA plot before normalization",
                      height = 350,
                      full_screen = TRUE,
                      nav_panel(
                        "Positive",
                        plotOutput(ns("pca_before_pos"))
                      ),
                      nav_panel(
                        "Negative",
                        plotOutput(ns("pca_before_neg"))
                      )
                    ),
                    navset_card_tab(
                      title = "PCA plot after normalization",
                      height = 350,
                      full_screen = TRUE,
                      nav_panel(
                        "Positive",
                        plotOutput(ns("pca_after_pos"))
                      ),
                      nav_panel(
                        "Negative",
                        plotOutput(ns("pca_after_neg"))
                      )
                    ),
                  ),
                  navset_card_tab(
                    title = "Status",
                    height = 400,
                    full_screen = TRUE,
                    nav_panel(
                      "Positive",
                      verbatimTextOutput(ns("obj_norm.pos"))
                    ),
                    nav_panel(
                      "Negative",
                      verbatimTextOutput(ns("obj_norm.neg"))
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
#' @noRd


data_normalize_server <- function(id,volumes,prj_init,data_import_rv,data_clean_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    p2_dataclean <- reactiveValues(data = NULL)


  })
}

