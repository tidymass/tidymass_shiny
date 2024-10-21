#' import from tbl data of UI
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


data_overview_ui <- function(id) {
  ns <- NS(id)
    nav_panel(
        title = 'Overview',
        icon = icon("scale-balanced"),
        layout_sidebar(
          sidebar = sidebar(
            title = "Actions",
            actionButton(inputId = ns('data_clean_start'),label = "Start",icon = icon("play")),
            materialSwitch(inputId = ns("data_clean_plt_format"),label = "Interactive plot", status = "primary"),
            actionButton(inputId = ns('generate_report_raw'),label = "Export report",icon = icon("save")),
          ),
          page_fluid(
            layout_column_wrap(
              width = 1/2,
              height = 350,
              navset_card_tab(
                height = 350,
                full_screen = TRUE,
                title = "Peak distribution",
                sidebar = sidebar(
                  open = 'closed',
                  radioButtons(inputId = ns("Hex_distribution"),
                               label = "Hex",
                               choices = c("TRUE","FALSE"),
                               selected = "TRUE"),
                ),
                nav_panel(
                  "Positive",
                  card_title("Peak distribution plot in positive model"),
                  uiOutput(ns("peak_dis_plot.pos"),fill = T)
                ),
                nav_panel(
                  "Negative",
                  card_title("Peak distribution plot in negative model"),
                  uiOutput(ns("peak_dis_plot.neg"),fill = T)
                )
              ),
              navset_card_tab(
                height = 350,
                full_screen = TRUE,
                title = "Check missing value",
                sidebar = sidebar(
                  open = 'closed',
                  radioButtons(
                    inputId = ns('show_row_names_smv'),
                    label = 'show row names',
                    choices = c("TRUE","FALSE"),
                    selected = 'FALSE'
                  ),
                  radioButtons(
                    inputId = ns('show_column_names_smv'),
                    label = 'show row names',
                    choices = c("TRUE","FALSE"),
                    selected = 'TRUE'
                  ),
                  radioButtons(
                    inputId = ns('percentage_smv'),
                    label = 'percentage',
                    choices = c("TRUE","FALSE"),
                    selected = 'FALSE'
                  ),
                  radioButtons(
                    inputId = ns('only_outlier_samples_smv'),
                    label = 'only outlier samples',
                    choices = c("TRUE","FALSE"),
                    selected = 'FALSE'
                  ),
                  radioButtons(
                    inputId = ns('only_outlier_variables_smv'),
                    label = 'only outlier variables',
                    choices = c("TRUE","FALSE"),
                    selected = 'FALSE'
                  ),
                  radioButtons(
                    inputId = ns('row_names_side_smv'),
                    label = 'row name side',
                    choices = c("left","right"),
                    selected = 'right'
                  ),
                  sliderInput(
                    inputId = ns('sample_na_cutoff_smv'),
                    label = 'sample NA cutoff',
                    min = 1,
                    max = 100,step = 1,
                    value = 50
                  ),
                  sliderInput(
                    inputId = ns('variable_na_cutoff_smv'),
                    label = 'variable NA cutoff',
                    min = 1,
                    max = 100,step = 1,
                    value = 50
                  )
                ),
                nav_panel(
                  "Positive",
                  card_title("Missing value summary in positive model"),
                  plotOutput(ns("mv_plot_all.pos"))
                ),
                nav_panel(
                  "Negative",
                  card_title("Missing value summary in negative model"),
                  plotOutput(ns("mv_plot_all.neg"))

                ),
                nav_panel(
                  shiny::icon("circle-info"),
                  markdown("Interactivecomplexheatmap DO NOT work when shiny version > 1.7.5 [issue](https://github.com/jokergoo/InteractiveComplexHeatmap/issues/114)")
                )
              )),
            layout_column_wrap(
              width = 1/2,
              height = 350,
              navset_card_tab(
                height = 350,
                full_screen = TRUE,
                title = "Missing value in samples",
                sidebar = sidebar(
                  id = ns("summ_mv_sidebar"),
                  open = 'closed',
                  selectInput(
                    inputId = ns("mv_color_by"),label = "color by",
                    choices = c("class","group","..."),selected = "class",
                    multiple = F
                  )
                ),
                nav_panel(
                  "Positive",
                  card_title("MV percentage (sample) in positive model"),
                  uiOutput(ns("summary_mvp.s_plt.pos"),fill = T)

                ),
                nav_panel(
                  "Negative",
                  card_title("MV percentage (sample) in negative model"),
                  uiOutput(ns("summary_mvp.s_plt.neg"),fill = T)

                )
              ),

              navset_card_tab(
                height = 350,
                full_screen = TRUE,
                title = "Missing value in variables",

                sidebar = sidebar(
                  id = ns("summ_mv_sidebar"),
                  open = 'closed',
                  selectInput(
                    inputId = ns("mv_color_by"),label = "color by",
                    choices = c("class","group","..."),selected = "class",
                    multiple = F
                  )
                ),
                nav_panel(
                  "Positive",
                  card_title("MV percentage (variable) in positive model"),
                  uiOutput(ns("summary_mvp.v_plt.pos"),fill = T)

                ),
                nav_panel(
                  "Negative",
                  card_title("MV percentage (variable) in positive model"),
                  uiOutput(ns("summary_mvp.v_plt.neg"),fill = T)

                )
              )),
            layout_column_wrap(
              width = 1/2,
              height = 350,
              navset_card_tab(
                height = 350,
                full_screen = TRUE,
                title = "RSD distribution",
                nav_panel(
                  "Positive",
                  card_title("RSD distribution in positive model"),
                  uiOutput(ns("summary_rsd_plt.pos"),fill = T)

                ),
                nav_panel(
                  "Negative",
                  card_title("RSD distribution in negative model"),
                  uiOutput(ns("summary_rsd_plt.neg"),fill = T)

                )
              ),
              navset_card_tab(
                height = 350,
                full_screen = TRUE,
                title = "Intensity for all the variables",
                sidebar = sidebar(
                  id = ns("summ_Intensity_sidebar"),
                  open = 'closed',
                  selectInput(
                    inputId = ns("boxplot_color_by"),label = "color by",
                    choices = c("class","group","..."),selected = "batch",
                    multiple = F
                  )
                ),
                nav_panel(
                  "Positive",
                  card_title("boxplot in positive model"),
                  uiOutput(ns("summary_box_plt.pos"),fill = T)

                ),
                nav_panel(
                  "Negative",
                  card_title("boxplot in negative model"),
                  uiOutput(ns("summary_box_plt.neg"),fill = T)

                )
              )),
            layout_column_wrap(
              width = 1/2,
              height = 350,
              navset_card_tab(
                height = 350,
                full_screen = TRUE,
                title = "PCA",
                nav_panel(
                  "Positive",
                  card_title("PCA plot in positive model"),
                  uiOutput(ns("summary_pca_plt.pos"),fill = T)

                ),
                nav_panel(
                  "Negative",
                  card_title("PCA plot in negative model"),
                  uiOutput(ns("summary_pca_plt.neg"),fill = T)

                )
              ),
              navset_card_tab(
                height = 350,
                full_screen = TRUE,
                title = "Sample correlation",
                nav_panel(
                  "Positive",
                  card_title("Sample correlation in positive model"),
                  uiOutput(ns("summary_corr_plt.pos"),fill = T)

                ),
                nav_panel(
                  "Negative",
                  card_title("Sample correlation in negative model"),
                  uiOutput(ns("summary_corr_plt.neg"),fill = T)

                )
              )
            )
          )
        )


      )
}


#' import from tbl data of server
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


data_overview_server <- function(id,volumes,prj_init,data_import_rv,data_clean_rv,data_export_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    p2_dataclean <- reactiveValues(data = NULL)

    observe({
      updateSelectInput(session, "mv_color_by",choices = colnames(prj_init$sample_info),selected = colnames(prj_init$sample_info)[3])
      updateSelectInput(session, "mv_order_by",choices = colnames(prj_init$sample_info),selected = colnames(prj_init$sample_info)[2])
    })


    observeEvent(
      input$data_clean_start,
      {
        #> Init object pos and neg
        if(!is.null(data_import_rv$object_pos)) {
          p2_dataclean$object_pos = data_import_rv$object_pos
          p2_dataclean$object_neg = data_import_rv$object_neg
        } else {
          return()
        }


        p2_dataclean$object_neg =
          p2_dataclean$object_neg %>%
          activate_mass_dataset('sample_info') %>%
          dplyr::select(sample_id) %>%
          dplyr::left_join(prj_init$sample_info)

        p2_dataclean$object_pos =
          p2_dataclean$object_pos %>%
          activate_mass_dataset('sample_info') %>%
          dplyr::select(sample_id) %>%
          dplyr::left_join(prj_init$sample_info)


# peak distribution -------------------------------------------------------


        temp_hex = input$Hex_distribution %>% as.logical()
        #> plot.pos

        output$peak_dis_plot.pos <- renderUI({
          plot_type <- input$data_clean_plt_format

          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_peak_dis_plot.pos"))
          } else {
            plotOutput(outputId = ns("plot_peak_dis_plot.pos"))
          }
        })

        output$plot_peak_dis_plot.pos <- renderPlot({
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          p2_dataclean$object_pos %>% massqc::show_mz_rt_plot(hex = temp_hex)
        })


        output$plotly_peak_dis_plot.pos <- renderPlotly({
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          p2_dataclean$object_pos %>% massqc::show_mz_rt_plot(hex = temp_hex) %>% plotly::ggplotly()
        })

        #> plot.neg
        output$peak_dis_plot.neg <- renderUI({
          plot_type <- input$data_clean_plt_format

          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_peak_dis_plot.neg"))
          } else {
            plotOutput(outputId = ns("plot_peak_dis_plot.neg"))
          }
        })

        output$plot_peak_dis_plot.neg <- renderPlot({
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          p2_dataclean$object_neg %>% massqc::show_mz_rt_plot(hex = temp_hex)
        })


        output$plotly_peak_dis_plot.neg <- renderPlotly({
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          p2_dataclean$object_neg %>% massqc::show_mz_rt_plot(hex = temp_hex) %>% plotly::ggplotly()
        })


# plot mv--------------------------------------------------------------------
        show_row_names = as.logical(input$show_row_names_smv)
        show_column_names = as.logical(input$show_column_names_smv)
        percentage = as.logical(input$percentage_smv)
        row_names_side = as.character(input$row_names_side_smv)
        sample_na_cutoff = as.numeric(input$sample_na_cutoff_smv)
        variable_na_cutoff = as.numeric(input$variable_na_cutoff_smv)
        only_outlier_samples = as.logical(input$only_outlier_samples_smv)
        only_outlier_variables = as.logical(input$only_outlier_variables_smv)
        print(show_row_names)

        output$mv_plot_all.pos <- renderPlot({
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          p2_dataclean$object_pos %>% massqc::show_missing_values(
            show_row_names = show_row_names,
            show_column_names = show_column_names,
            percentage = percentage,
            row_names_side = row_names_side,
            sample_na_cutoff = sample_na_cutoff,
            variable_na_cutoff = variable_na_cutoff,
            only_outlier_samples = only_outlier_samples,
            only_outlier_variables = only_outlier_variables
          )
        })

        output$mv_plot_all.neg <- renderPlot({
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          p2_dataclean$object_neg %>% massqc::show_missing_values(
            show_row_names = show_row_names,
            show_column_names = show_column_names,
            percentage = percentage,
            row_names_side = row_names_side,
            sample_na_cutoff = sample_na_cutoff,
            variable_na_cutoff = variable_na_cutoff,
            only_outlier_samples = only_outlier_samples,
            only_outlier_variables = only_outlier_variables
          )
        })

        #> plot.neg
        output$data_clean_mv_plt.neg <- renderUI({
          plot_type <- input$data_clean_plt_format

          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_plot_checkmv.neg"))
          } else {
            plotOutput(outputId = ns("plot_checkmv.neg"))
          }
        })

        output$plot_checkmv.neg <- renderPlot({
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          check_mv(object = p2_dataclean$object_neg,colby = p2_dataclean$colby,orderby = p2_dataclean$orderby,type = 'plot')
        })

        output$plotly_plot_checkmv.neg <- renderPlotly({
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          check_mv(object = p2_dataclean$object_neg,colby = p2_dataclean$colby,orderby = p2_dataclean$orderby,type = 'plotly')
        })
        data_clean_rv$object_neg <- p2_dataclean$object_neg
        data_clean_rv$object_pos <- p2_dataclean$object_pos

      }
    )


  })
}

