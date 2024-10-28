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
                sidebar = accordion(
                  open = 'closed',
                  accordion_panel(
                    title = 'Parameter',
                    radioButtons(inputId = ns("Hex_distribution"),
                                 label = "Hex",
                                 choices = c("TRUE","FALSE"),
                                 selected = "TRUE"),
                  ),
                  accordion_panel(
                    title = 'Download',
                    icon = bs_icon('download'),
                    textInput(
                      inputId = ns("fig1_height"),label = "Height",value = 7
                    ),
                    textInput(
                      inputId = ns("fig1_width"),label = "width",value = 7
                    ),
                    selectInput(
                      inputId = ns("fig1_format"),label = "format",
                      choices = c("jepg","pdf","png"),
                      selected = "pdf",selectize = F
                    ),
                    downloadButton(outputId = ns("fig1_download"),label = "Download",icon = icon("download"))
                  )
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
                sidebar = accordion(open = FALSE,
                  accordion_panel(
                    title = "Parameter",
                    radioButtons(
                      inputId = ns('show_row_names_mv'),
                      label = 'show row names',
                      choices = c("TRUE","FALSE"),
                      selected = 'FALSE'
                    ),
                    radioButtons(
                      inputId = ns('show_column_names_mv'),
                      label = 'show row names',
                      choices = c("TRUE","FALSE"),
                      selected = 'TRUE'
                    ),
                    radioButtons(
                      inputId = ns('percentage_mv'),
                      label = 'percentage',
                      choices = c("TRUE","FALSE"),
                      selected = 'FALSE'
                    ),
                    radioButtons(
                      inputId = ns('only_outlier_samples_mv'),
                      label = 'only outlier samples',
                      choices = c("TRUE","FALSE"),
                      selected = 'FALSE'
                    ),
                    radioButtons(
                      inputId = ns('only_outlier_variables_mv'),
                      label = 'only outlier variables',
                      choices = c("TRUE","FALSE"),
                      selected = 'FALSE'
                    ),
                    radioButtons(
                      inputId = ns('row_names_side_mv'),
                      label = 'row name side',
                      choices = c("left","right"),
                      selected = 'right'
                    ),
                    sliderInput(
                      inputId = ns('sample_na_cutoff_mv'),
                      label = 'sample NA cutoff',
                      min = 1,
                      max = 100,step = 1,
                      value = 50
                    ),
                    sliderInput(
                      inputId = ns('variable_na_cutoff_mv'),
                      label = 'variable NA cutoff',
                      min = 1,
                      max = 100,step = 1,
                      value = 50
                    )
                  ),
                  accordion_panel(
                    title = 'Download',
                    icon = bs_icon('download'),
                    textInput(
                      inputId = ns("fig2_height"),label = "Height",value = 7
                    ),
                    textInput(
                      inputId = ns("fig2_width"),label = "width",value = 7
                    ),
                    selectInput(
                      inputId = ns("fig2_format"),label = "format",
                      choices = c("jepg","pdf","png"),
                      selected = "pdf",selectize = F
                    ),
                    downloadButton(outputId = ns("fig2_download"),label = "Download",icon = icon("download"))
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
                    inputId = ns("color_by_smv"),label = "color by",
                    choices = c("class","group","..."),selected = "class",
                    multiple = F
                  ),
                  selectInput(
                    inputId = ns("order_by_smv"),label = "order by",
                    choices = c("injection.order","class","..."),selected = "class",
                    multiple = F
                  ),
                  radioButtons(
                    inputId = ns('percentage_smv'),label = 'percentage',choices = c("TRUE","FALSE"),selected = "FALSE"
                  ),
                  radioButtons(
                    inputId = ns('show_x_text_smv'),label = 'show x text',choices = c("TRUE","FALSE"),selected = "TRUE"
                  ),
                  radioButtons(
                    inputId = ns('show_x_ticks_smv'),label = 'show x ticks',choices = c("TRUE","FALSE"),selected = "TRUE"
                  ),
                  radioButtons(
                    inputId = ns('desc_smv'),label = 'descend sample order or not',choices = c("TRUE","FALSE"),selected = "FALSE"
                  )
                ),
                nav_panel(
                  "Positive",
                  card_title("MV percentage (sample) in positive model"),
                  uiOutput(ns("smv_plt.pos"),fill = T)

                ),
                nav_panel(
                  "Negative",
                  card_title("MV percentage (sample) in negative model"),
                  uiOutput(ns("smv_plt.neg"),fill = T)

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


# server ------------------------------------------------------------------


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
    ## parameters for color
    observe({
      updateSelectInput(session, "color_by_smv",choices = colnames(prj_init$sample_info),selected = colnames(prj_init$sample_info)[3])
      updateSelectInput(session, "order_by_smv",choices = colnames(prj_init$sample_info),selected = colnames(prj_init$sample_info)[2])
    })
    ##> set plot parameters =============
    plot_para = reactive({
      list(
        ##> fig1
        fig1_hex = input$Hex_distribution %>% as.logical(),
        ##> fig2
        fig2_show_row_names = as.logical(input$show_row_names_mv),
        fig2_show_column_names = as.logical(input$show_column_names_mv),
        fig2_percentage = as.logical(input$percentage_mv),
        fig2_row_names_side = as.character(input$row_names_side_mv),
        fig2_sample_na_cutoff = as.numeric(input$sample_na_cutoff_mv),
        fig2_variable_na_cutoff = as.numeric(input$variable_na_cutoff_mv),
        fig2_only_outlier_samples = as.logical(input$only_outlier_samples_mv),
        fig2_only_outlier_variables = as.logical(input$only_outlier_variables_mv),
        ##> fig3
        fig3_color_by = input$color_by_smv %>% as.character(),
        fig3_order_by = input$order_by_smv %>% as.character(),
        fig3_percentage = input$percentage_smv %>% as.logical(),
        fig3_show_x_text = input$show_x_text_smv %>% as.logical(),
        fig3_show_x_ticks = input$show_x_ticks_smv %>% as.logical(),
        fig3_desc = input$desc_smv %>% as.logical()
        ##> fig4
        ##> fig5
        ##> fig6
        ##> fig7
      )
    })
    ##> download parameters ================
    download_para = reactive({
      list(
        ##> fig1
        ##> fig2
        fig2_width = as.numeric(input$fig2_width),
        fig2_height = as.numeric(input$fig2_height),
        fig2_format = as.character(input$fig2_format),
        ##> fig3
        fig3_width = as.numeric(input$fig3_width),
        fig3_height = as.numeric(input$fig3_height),
        fig3_format = as.character(input$fig3_format),
        ##> fig4
        fig4_width = as.numeric(input$fig4_width),
        fig4_height = as.numeric(input$fig4_height),
        fig4_format = as.character(input$fig4_format),
        ##> fig5
        fig5_width = as.numeric(input$fig5_width),
        fig5_height = as.numeric(input$fig5_height),
        fig5_format = as.character(input$fig5_format),
        ##> fig6
        fig6_width = as.numeric(input$fig6_width),
        fig6_height = as.numeric(input$fig6_height),
        fig6_format = as.character(input$fig6_format)
      )
    })

    ##> draw plot ==================
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
        para = plot_para()
        ###> fig1 ========
        output$peak_dis_plot.pos <- renderUI({
          plot_type <- input$data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_peak_dis_plot.pos"))
          } else {
            plotOutput(outputId = ns("plot_peak_dis_plot.pos"))
          }
        })

        p2_dataclean$fig1.pos =
          p2_dataclean$object_pos %>% massqc::show_mz_rt_plot(hex = para$fig1_hex)


        output$plot_peak_dis_plot.pos <- renderPlot({

          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          p2_dataclean$fig1.pos
        })


        output$plotly_peak_dis_plot.pos <- renderPlotly({

          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          p2_dataclean$fig1.pos %>% plotly::ggplotly()
        })

        #> plot.neg
        p2_dataclean$fig1.neg =
          p2_dataclean$object_neg %>% massqc::show_mz_rt_plot(hex = para$fig1_hex)

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
          p2_dataclean$fig1.neg
        })

        output$plotly_peak_dis_plot.neg <- renderPlotly({

          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          p2_dataclean$fig1.neg %>% plotly::ggplotly()
        })

        ###> fig2 ===================
        output$mv_plot_all.pos <- renderPlot({
          para = plot_para()
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          p2_dataclean$object_pos %>% massqc::show_missing_values(
            show_row_names = para$fig2_show_row_names,
            show_column_names = para$fig2_show_column_names,
            percentage = para$fig2_percentage,
            row_names_side = para$fig2_row_names_side,
            sample_na_cutoff = para$fig2_sample_na_cutoff,
            variable_na_cutoff = para$fig2_variable_na_cutoff,
            only_outlier_samples = para$fig2_only_outlier_samples,
            only_outlier_variables = para$fig2_only_outlier_variables
          )
        })

        output$mv_plot_all.neg <- renderPlot({
          para = plot_para()
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          p2_dataclean$object_neg %>% massqc::show_missing_values(
            show_row_names = para$fig2_show_row_names,
            show_column_names = para$fig2_show_column_names,
            percentage = para$fig2_percentage,
            row_names_side = para$fig2_row_names_side,
            sample_na_cutoff = para$fig2_sample_na_cutoff,
            variable_na_cutoff = para$fig2_variable_na_cutoff,
            only_outlier_samples = para$fig2_only_outlier_samples,
            only_outlier_variables = para$fig2_only_outlier_variables
          )
        })

        ###> fig3 ================
        # positive
        output$smv_plt.pos <- renderUI({
          plot_type <- input$data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_smv_plt.pos"))
          } else {
            plotOutput(outputId = ns("plot_smv_plt.pos"))
          }
        })

        output$plot_smv_plt.pos <- renderPlot({
          para = plot_para()
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          p2_dataclean$object_pos %>% massqc::show_sample_missing_values(
            color_by = para$fig3_color_by,
            order_by = para$fig3_order_by,
            percentage = para$fig3_percentage,
            show_x_text = para$fig3_show_x_text,
            show_x_ticks = para$fig3_show_x_ticks,
            desc = para$fig3_desc
          )
        })

        output$plotly_smv_plt.pos <- renderPlotly({
          para = plot_para()
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          p2_dataclean$object_pos %>% massqc::show_sample_missing_values(
            color_by = para$fig3_color_by,
            order_by = para$fig3_order_by,
            percentage = para$fig3_percentage,
            show_x_text = para$fig3_show_x_text,
            show_x_ticks = para$fig3_show_x_ticks,
            desc = para$fig3_desc
          ) %>% plotly::ggplotly()
        })

        # negative

        output$smv_plt.neg <- renderUI({
          plot_type <- input$data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_smv_plt.neg"))
          } else {
            plotOutput(outputId = ns("plot_smv_plt.neg"))
          }
        })

        output$plot_smv_plt.neg <- renderPlot({
          para = plot_para()
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          p2_dataclean$object_neg %>% massqc::show_sample_missing_values(
            color_by = para$fig3_color_by,
            order_by = para$fig3_order_by,
            percentage = para$fig3_percentage,
            show_x_text = para$fig3_show_x_text,
            show_x_ticks = para$fig3_show_x_ticks,
            desc = para$fig3_desc
          )
        })


        output$plotly_smv_plt.neg <- renderPlotly({
          para = plot_para()
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          p2_dataclean$object_neg %>% massqc::show_sample_missing_values(
            color_by = para$fig3_color_by,
            order_by = para$fig3_order_by,
            percentage = para$fig3_percentage,
            show_x_text = para$fig3_show_x_text,
            show_x_ticks = para$fig3_show_x_ticks,
            desc = para$fig3_desc
          ) %>% plotly::ggplotly()
        })


      }
    )
    ##> download plot ==================
    ###> fig1
    output$fig1_download = downloadHandler(
      filename = function() {
        "01.Peak_distribution"
      },
      content = function(file,format) {
        para = download_para()
        ggsave(
          filename = paste0(file,"_pos.",para$fig1_format),
          plot = p2_dataclean$object_pos %>% massqc::show_mz_rt_plot(hex = para$fig1_hex),
          width = para$fig1_width,
          height = para$fig1_height
        )
        ggsave(
          filename = paste0(file,"_neg.",para$fig1_format),
          plot = p2_dataclean$object_neg %>% massqc::show_mz_rt_plot(hex = para$fig1_hex),
          width = para$fig1_width,
          height = para$fig1_height
        )
      }
    )
    ###> fig2
    output$fig2_download = downloadHandler(
      filename = function() {
        para_1 = download_para()
        paste0("01.Missing_value_summary.",para_1$fig2_format)
      },
      content = function(file) {
        para = plot_para()
        para_1 = download_para()
        if(is.null(p2_dataclean$fig1.neg) & is.null(p2_dataclean$fig1.pos)) {
          return()
        } else if(is.null(p2_dataclean$fig1.neg))
        ggsave(
          filename = file,
          plot = ,
          width = para_1$fig2_width,
          height = para_1$fig2_height
        )
      }
    )
    ###> fig3
    ###> fig4
    ###> fig5
    ###> fig6
  }
  )
  }


##> download


