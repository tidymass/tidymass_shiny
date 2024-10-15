#' Metabolite classification
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


feature_class_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'feature classification',
    icon = bs_icon("pie-chart"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Metabolite classification",
          icon = bsicons::bs_icon("gear"),
          radioButtons(
            inputId = ns('class_method'),
            label = 'Use PUG-REST servers',choices = c("TRUE","FALSE"),
            selected = 'FLASE'
          ),
          HTML(
            "<span style='color: #7a8788; font-size: 12px; font-style: italic;'>Notice: It will take a </span>
                          <span style='color: red; font-size: 12px; font-style: italic;'>looooong</span>
                          <span style='color: #7a8788; font-size: 12px; font-style: italic;'>time!</span>"
          ),
          actionButton(inputId = ns('class_start'),"Generate classification table",icon = icon("angles-up")),
        ),
        accordion_panel(
          title = "Visualization parameters",
          icon = bsicons::bs_icon("gear"),
          radioButtons(
            inputId = ns('pie_tag'),
            label = 'choise',choices = c("superclass","class","subclass"),
            selected = 'class'
          ),
          textInput(
            inputId = ns('pie_cut'),
            label = "other group size",
            value = 10
          ),
          actionButton(inputId = ns('class_show'),"Show pie plot",icon = icon("angles-up")),
          materialSwitch(inputId = ns("class_plt_format"),label = "Interactive plot", status = "primary"),
        ),
        accordion_panel(
          title = "Construct ClassyFire annotation database for enrichment",
          icon = bsicons::bs_icon("database-add"),
          actionButton(inputId = ns('class_database_construct'),"start database construction",icon = icon("angles-up")),
        )
      ),
      page_fluid(
        nav_panel(title = "classification",
                  navset_card_tab(
                    title = "Classification table",
                    height = 400,
                    full_screen = TRUE,
                    nav_panel(
                      "Positive",
                      DT::dataTableOutput(outputId = ns("class_tbl_pos"))
                    ),
                    nav_panel(
                      "Negative",
                      DT::dataTableOutput(outputId = ns("class_tbl_neg"))
                    ),
                    nav_panel(
                      shiny::icon("circle-info"),
                      markdown("description of noise remove method.")
                    )
                  ),
                  navset_card_tab(
                    title = "Pie plot",
                    height = 350,
                    full_screen = TRUE,
                    nav_panel(
                      "Positive",
                      plotOutput(ns("class_pie_pos"))
                    ),
                    nav_panel(
                      "Negative",
                      plotOutput(ns("class_pie_neg"))
                    )
                  ),
                  layout_column_wrap(
                    width = 1/2,
                    height = 350,
                    navset_card_tab(
                      title = "Classification enrichment database: Term2Name",
                      height = 350,
                      full_screen = TRUE,
                      nav_panel(
                        "Positive",
                        DT::dataTableOutput(outputId = ns("t2n_pos"))
                      ),
                      nav_panel(
                        "Negative",
                        DT::dataTableOutput(outputId = ns("t2n_neg"))
                      )
                    ),
                    navset_card_tab(
                      title = "Classification enrichment database: Term2Metabolites",
                      height = 350,
                      full_screen = TRUE,
                      nav_panel(
                        "Positive",
                        DT::dataTableOutput(outputId = ns("t2m_pos"))
                      ),
                      nav_panel(
                        "Negative",
                        DT::dataTableOutput(outputId = ns("t2m_neg"))
                      )
                    ),
                  ),
                  navset_card_tab(
                    title = "Status",
                    height = 400,
                    full_screen = TRUE,
                    nav_panel(
                      "Positive",
                      verbatimTextOutput(ns("obj_class.pos"))
                    ),
                    nav_panel(
                      "Negative",
                      verbatimTextOutput(ns("obj_class.neg"))
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


feature_class_server <- function(id,volumes,prj_init,data_import_rv,data_clean_rv,data_anno) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    p2_dataclean <- reactiveValues(data = NULL)


  })
}

