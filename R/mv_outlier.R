#' remove outlier
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


remove_outlier_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Filter outlier samples',
    icon = bs_icon("funnel"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Visualize parameters",
          icon = bsicons::bs_icon("image"),
          selectInput(
            inputId = ns("color_by"),
            label = "color by",
            choices = c("class","group","batch","..."),
            selected = "group"
          ),
          selectInput(
            inputId = ns("order_by"),
            label = "order by",
            choices = c("injection.order","..."),
            selected = "injection.order"
          ),
          actionButton(inputId = ns("vis_butt_1"),label = "Show plot",icon = icon("play")),
          materialSwitch(inputId = ns("Interactive_butt"),label = "Interactive plot", status = "primary")
        ),
        accordion_panel(
          title = "Outlier deteced parameters",
          icon = bsicons::bs_icon("gear"),
          selectInput(
            inputId = ns("Heterogeneous"),
            label = "Heterogeneous samples",
            choices = c("TRUE","FALSE"),
            selected = "FALSE"
          ),
          selectInput(
            inputId = ns("mv_method"),
            label = "Outlier removal strategy",
            choices = c("By tidymass","By myself"),
            selected = "By tidymass"
          ),
          selectInput(
            inputId = ns("outliers.pos"),
            label = "outliers in positive model",
            choices = c("none"),
            selected = "none"
          ),
          selectInput(
            inputId = ns("outliers.neg"),
            label = "outliers in negative model",
            choices = c("none"),
            selected = "none"
          ),
          actionButton(
            inputId = ns("mv_start"),
            label = "Start",icon = icon("play")
          )
        )),
      page_fluid(
        nav_panel(title = "remove outlier",
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
                      markdown("description of this figure")
                    )
                  ),
                  layout_column_wrap(
                    width = 1/2,
                    height = 350,
                    navset_card_tab(
                      title = "PCA plot",
                      height = 350,
                      full_screen = TRUE,
                      nav_panel(
                        "Positive",
                        plotOutput(ns("pca_pos"))
                      ),
                      nav_panel(
                        "Negative",
                        plotOutput(ns("pca_neg"))
                      ),
                      nav_panel(
                        shiny::icon("circle-info"),
                        markdown("description of this figure")
                      )
                    ),
                    navset_card_tab(
                      title = "Hierarchical clustering",
                      height = 350,
                      full_screen = TRUE,
                      nav_panel(
                        "Positive",
                        plotOutput(ns("hclust_pos"))
                      ),
                      nav_panel(
                        "Negative",
                        plotOutput(ns("hclust_neg"))
                      ),
                      nav_panel(
                        shiny::icon("circle-info"),
                        markdown("description of this figure")
                      )
                    ),
                    ),
                  navset_card_tab(
                    title = "Outlier detection summary",
                    height = 400,
                    full_screen = TRUE,
                    nav_panel(
                      "Positive",
                      DT::dataTableOutput(outputId = ns("outlier_info_pos"))
                    ),
                    nav_panel(
                      "Negative",
                      DT::dataTableOutput(outputId = ns("outlier_info_neg"))
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
                      verbatimTextOutput(ns("obj_outlier.pos"))
                    ),
                    nav_panel(
                      "Negative",
                      verbatimTextOutput(ns("obj_outlier.neg"))
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
#' @noRd


remove_outlier_server <- function(id,volumes,prj_init,data_import_rv,data_clean_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    p2_dataclean <- reactiveValues(data = NULL)


  })
}

