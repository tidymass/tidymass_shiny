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
#' @importFrom colourpicker colourInput
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
                      choices = c("jpg","pdf","png","tiff"),
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
                      label = 'show column names',
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
                      choices = c("jpg","pdf","png","tiff"),
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
                sidebar = accordion(
                  open = FALSE,
                  accordion_panel(
                    title = 'Parameters',
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
                  accordion_panel(
                    title = 'Download',
                    icon = bs_icon('download'),
                    textInput(
                      inputId = ns("fig3_height"),label = "Height",value = 7
                    ),
                    textInput(
                      inputId = ns("fig3_width"),label = "width",value = 7
                    ),
                    selectInput(
                      inputId = ns("fig3_format"),label = "format",
                      choices = c("jpg","pdf","png","tiff"),
                      selected = "pdf",selectize = F
                    ),
                    downloadButton(outputId = ns("fig3_download"),label = "Download",icon = icon("download"))
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

                sidebar =
                  accordion(
                    open = FALSE,
                    accordion_panel(
                      title = 'Parameters',
                      selectInput(
                        inputId = ns('color_by_vmv'),
                        label = 'color by',
                        choices = c("mz","rt"),
                        selected = 'mz'
                      ),
                      selectInput(
                        inputId = ns('order_by_vmv'),
                        label = 'order by',
                        choices = c('variable_id',"..."),
                        selected = 'variable_id'
                      ),
                      radioButtons(
                        inputId = ns('percentage_vmv'),label = 'percentage',choices = c("TRUE","FALSE"),selected = "FALSE"
                      ),
                      radioButtons(
                        inputId = ns('show_x_text_vmv'),label = 'show x text',choices = c("TRUE","FALSE"),selected = "TRUE"
                      ),
                      radioButtons(
                        inputId = ns('show_x_ticks_vmv'),label = 'show x ticks',choices = c("TRUE","FALSE"),selected = "TRUE"
                      ),
                      radioButtons(
                        inputId = ns('desc_vmv'),label = 'descend variable order or not',choices = c("TRUE","FALSE"),selected = "FALSE"
                      )
                    ),accordion_panel(
                      title = 'Download',
                      icon = bs_icon('download'),
                      textInput(
                        inputId = ns("fig4_height"),label = "Height",value = 7
                      ),
                      textInput(
                        inputId = ns("fig4_width"),label = "width",value = 7
                      ),
                      selectInput(
                        inputId = ns("fig4_format"),label = "format",
                        choices = c("jpg","pdf","png","tiff"),
                        selected = "pdf",selectize = F
                      ),
                      downloadButton(outputId = ns("fig4_download"),label = "Download",icon = icon("download"))
                    )
                  ),
                nav_panel(
                  "Positive",
                  card_title("MV percentage (variable) in positive model"),
                  plotOutput(ns("plot_vmv_plt.pos"),fill = T)
                ),
                nav_panel(
                  "Negative",
                  card_title("MV percentage (variable) in positive model"),
                  plotOutput(ns("plot_vmv_plt.neg"),fill = T)
                )
              )),
            layout_column_wrap(
              width = 1/2,
              height = 350,
              navset_card_tab(
                height = 350,
                full_screen = TRUE,
                title = "RSD distribution",
                sidebar = accordion(
                  open = 'closed',
                  accordion_panel(
                    title = 'Parameter',
                    sliderInput(
                      inputId = ns('rsd_cutoff'),label = 'rsd cutoff',min = 0,max = 100,step = 1,value = 30
                    ),
                    colourpicker::colourInput(inputId = ns("color_rsd"),
                                              label = "color",
                                              value = "red"),
                  ),
                  accordion_panel(
                    title = 'Download',
                    icon = bs_icon('download'),
                    textInput(
                      inputId = ns("fig5_height"),label = "Height",value = 7
                    ),
                    textInput(
                      inputId = ns("fig5_width"),label = "width",value = 7
                    ),
                    selectInput(
                      inputId = ns("fig5_format"),label = "format",
                      choices = c("jpg","pdf","png","tiff"),
                      selected = "pdf",selectize = F
                    ),
                    downloadButton(outputId = ns("fig5_download"),label = "Download",icon = icon("download"))
                  )
                ),
                nav_panel(
                  "Positive",
                  card_title("RSD distribution in positive model"),
                  uiOutput(ns("rsd_plt.pos"),fill = T)

                ),
                nav_panel(
                  "Negative",
                  card_title("RSD distribution in negative model"),
                  uiOutput(ns("rsd_plt.neg"),fill = T)

                )
              ),
              navset_card_tab(
                height = 350,
                full_screen = TRUE,
                title = "Intensity for all the variables",
                sidebar =
                  accordion(
                    open = FALSE,
                    accordion_panel(
                      title = 'Parameters',
                      selectInput(
                        inputId = ns('fig6_color_by'),label = "color by",choices = c("batch",'class'),selected = 'batch',multiple = F
                      ),
                      selectInput(
                        inputId = ns('fig6_fill_by'),label = 'fill by',choices = c("batch",'class'),selected = 'class',multiple = F
                      ),
                      selectInput(
                        inputId = ns('fig6_order_by'),label = 'fill by',choices = c("sample_id",'class'),selected = 'sample_id',multiple = F
                      ),
                      radioButtons(
                        inputId = ns('fig6_point'),label = "point",choices = c('TRUE','FALSE'),selected = 'FALSE'
                      ),
                      sliderInput(
                        inputId = ns('fig6_point_alpha'),label = 'point alpha',min = 0,max = 1,step = 0.1,value = 0.8
                      )
                    ),
                    accordion_panel(
                      title = 'Download',
                      icon = bs_icon('download'),
                      textInput(
                        inputId = ns("fig6_height"),label = "Height",value = 7
                      ),
                      textInput(
                        inputId = ns("fig6_width"),label = "width",value = 7
                      ),
                      selectInput(
                        inputId = ns("fig6_format"),label = "format",
                        choices = c("jpg","pdf","png","tiff"),
                        selected = "pdf",selectize = F
                      ),
                      downloadButton(outputId = ns("fig6_download"),label = "Download",icon = icon("download"))
                    )
                  ),
                nav_panel(
                  "Positive",
                  card_title("boxplot in positive model"),
                  uiOutput(ns("box_plt.pos"),fill = T)
                ),
                nav_panel(
                  "Negative",
                  card_title("boxplot in negative model"),
                  uiOutput(ns("box_plt.neg"),fill = T)
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

    ##> set plot parameters =============
    plot1_para = reactive({
      list(
        ##> fig1
        fig1_hex = input$Hex_distribution %>% as.logical()
      )
    })
    plot2_para = reactive({
      list(
        fig2_show_row_names = as.logical(input$show_row_names_mv),
        fig2_show_column_names = as.logical(input$show_column_names_mv),
        fig2_percentage = as.logical(input$percentage_mv),
        fig2_row_names_side = as.character(input$row_names_side_mv),
        fig2_sample_na_cutoff = as.numeric(input$sample_na_cutoff_mv),
        fig2_variable_na_cutoff = as.numeric(input$variable_na_cutoff_mv),
        fig2_only_outlier_samples = as.logical(input$only_outlier_samples_mv),
        fig2_only_outlier_variables = as.logical(input$only_outlier_variables_mv)
        )
    })
    plot3_para = reactive({
      list(
        fig3_color_by = input$color_by_smv %>% as.character(),
        fig3_order_by = input$order_by_smv %>% as.character(),
        fig3_percentage = input$percentage_smv %>% as.logical(),
        fig3_show_x_text = input$show_x_text_smv %>% as.logical(),
        fig3_show_x_ticks = input$show_x_ticks_smv %>% as.logical(),
        fig3_desc = input$desc_smv %>% as.logical()
      )
    })
    plot4_para = reactive({
      list(
        fig4_color_by = input$color_by_vmv %>% as.character(),
        fig4_order_by = input$order_by_vmv %>% as.character(),
        fig4_percentage = input$percentage_vmv %>% as.logical(),
        fig4_show_x_text = input$show_x_text_vmv %>% as.logical(),
        fig4_show_x_ticks = input$show_x_ticks_vmv %>% as.logical(),
        fig4_desc = input$desc_vmv %>% as.logical()
      )
    })
    plot5_para = reactive({
      list(
        fig5_rsd_cutoff = input$rsd_cutoff %>% as.numeric(),
        fig5_color = input$color_rsd %>% as.character()
      )
    })
    plot6_para = reactive({
      list(
        fig6_color_by = input$fig6_color_by %>% as.character(),
        fig6_order_by = input$fig6_order_by %>% as.character(),
        fig6_fill_by = input$fig6_fill_by %>% as.character(),
        fig6_point = input$fig6_point %>% as.logical(),
        fig6_point_alpha = input$fig6_point_alpha %>% as.numeric()
      )
    })
    ##> download parameters ================
    download_para = reactive({
      list(
        ##> fig1
        fig1_width = as.numeric(input$fig1_width),
        fig1_height = as.numeric(input$fig1_height),
        fig1_format = as.character(input$fig1_format),
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
        ###> fig1 peak distribution ========
        output$peak_dis_plot.pos <- renderUI({
          plot_type <- input$data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_peak_dis_plot.pos"))
          } else {
            plotOutput(outputId = ns("plot_peak_dis_plot.pos"))
          }
        })

        output$plot_peak_dis_plot.pos <- renderPlot({
          para = plot1_para()
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          p2_dataclean$object_pos %>% massqc::show_mz_rt_plot(hex = para$fig1_hex)
        })


        output$plotly_peak_dis_plot.pos <- renderPlotly({
          para = plot1_para()
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          p2_dataclean$object_pos %>% massqc::show_mz_rt_plot(hex = para$fig1_hex) %>% plotly::ggplotly()
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
          para = plot1_para()
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          p2_dataclean$object_neg %>% massqc::show_mz_rt_plot(hex = para$fig1_hex)
        })

        output$plotly_peak_dis_plot.neg <- renderPlotly({
          para = plot1_para()
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          p2_dataclean$object_neg %>% massqc::show_mz_rt_plot(hex = para$fig1_hex) %>% plotly::ggplotly()
        })

        ###> fig2 missing value summary ===================
        output$mv_plot_all.pos <- renderPlot({
          para = plot2_para()
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
          para = plot2_para()
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

        ###> fig3 missing value in samples ================
      observe({
        updateSelectInput(session, "color_by_smv",choices = colnames(p2_dataclean$object_pos@sample_info),selected = "group")
        updateSelectInput(session, "order_by_smv",choices = colnames(p2_dataclean$object_pos@sample_info),selected = "sample_id")
      })
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
          para = plot3_para()
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
          para = plot3_para()
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
          para = plot3_para()
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
          para = plot3_para()
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
        ###> fig4 missing value in variable =======
        temp_variable = p2_dataclean$object_neg %>% extract_variable_info_note()
        observe({
          updateSelectInput(session, "order_by_vmv",choices = temp_variable,selected = "variable_id")
          updateSelectInput(session, "color_by_vmv",choices = temp_variable,selected = "mz")
        })
        # positive
        output$plot_vmv_plt.pos <- renderPlot({
          para = plot4_para()
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          p2_dataclean$object_pos %>% massqc::show_variable_missing_values(
            color_by = para$fig4_color_by,
            order_by = para$fig4_order_by,
            percentage = para$fig4_percentage,
            show_x_text = para$fig4_show_x_text,
            show_x_ticks = para$fig4_show_x_ticks,
            desc = para$fig4_desc
          )
        })
        # negative
        output$plot_vmv_plt.neg <- renderPlot({
          para = plot4_para()
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          p2_dataclean$object_neg %>% massqc::show_variable_missing_values(
            color_by = para$fig4_color_by,
            order_by = para$fig4_order_by,
            percentage = para$fig4_percentage,
            show_x_text = para$fig4_show_x_text,
            show_x_ticks = para$fig4_show_x_ticks,
            desc = para$fig4_desc
          )
        })
        ###> fig5 rsd =======

        # positive
        output$rsd_plt.pos <- renderUI({
          plot_type <- input$data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_rsd_plt.pos"))
          } else {
            plotOutput(outputId = ns("plot_rsd_plt.pos"))
          }
        })
        output$plot_rsd_plt.pos <- renderPlot({
          para = plot5_para()
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          p2_dataclean$object_pos %>%
            dplyr::filter(class == "QC")%>%
            massqc::massqc_cumulative_rsd_plot(
            rsd_cutoff = para$fig5_rsd_cutoff,
            color = para$fig5_color,
            title = 'All of QC sample'
          )
        })
        output$plotly_rsd_plt.pos <- renderPlotly({
          para = plot5_para()
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          p2_dataclean$object_pos %>%
            dplyr::filter(class == "QC")%>%
            massqc::massqc_cumulative_rsd_plot(
              rsd_cutoff = para$fig5_rsd_cutoff,
              color = para$fig5_color,
              title = 'All of QC sample'
            ) %>% plotly::ggplotly()
        })
        # negative
        output$rsd_plt.neg <- renderUI({
          plot_type <- input$data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_rsd_plt.neg"))
          } else {
            plotOutput(outputId = ns("plot_rsd_plt.neg"))
          }
        })
        output$plot_rsd_plt.neg <- renderPlot({
          para = plot5_para()
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          p2_dataclean$object_neg %>%
            dplyr::filter(class == "QC")%>%
            massqc::massqc_cumulative_rsd_plot(
              rsd_cutoff = para$fig5_rsd_cutoff,
              color = para$fig5_color,
              title = 'All of QC sample'
            )
        })
        output$plotly_rsd_plt.neg <- renderPlotly({
          para = plot5_para()
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          p2_dataclean$object_neg %>%
            dplyr::filter(class == "QC")%>%
            massqc::massqc_cumulative_rsd_plot(
              rsd_cutoff = para$fig5_rsd_cutoff,
              color = para$fig5_color,
              title = 'All of QC sample'
            ) %>% plotly::ggplotly()
        })
        ###> fig6 sample boxplot
        observe({
          updateSelectInput(session, "color_by_msb",choices = colnames(p2_dataclean$object_pos@sample_info),selected = "batch")
          updateSelectInput(session, "fill_by_msb",choices = colnames(p2_dataclean$object_pos@sample_info),selected = "class")
          updateSelectInput(session, "order_by_msb",choices = colnames(p2_dataclean$object_pos@sample_info),selected = "sample_id")
        })
        output$box_plt.pos <- renderUI({
          plot_type <- input$data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_box_plt.pos"))
          } else {
            plotOutput(outputId = ns("plot_box_plt.pos"))
          }
        })
        output$plot_box_plt.pos <- renderPlot({
          para = plot6_para()
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          p2_dataclean$object_pos %>%log() %>%
            dplyr::filter(class == "QC")%>%
            massqc::massqc_sample_boxplot(
              color_by = para$fig6_color_by,
              fill_by = para$fig6_fill_by,
              order_by = para$fig6_order_by,
              point = para$fig6_point,
              point_alpha = para$fig6_alpha
            )
        })
        output$plotly_box_plt.pos <- renderPlotly({
          para = plot6_para()
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          p2_dataclean$object_pos %>%log() %>%
            dplyr::filter(class == "QC")%>%
            massqc::massqc_sample_boxplot(
              color_by = para$fig6_color_by,
              fill_by = para$fig6_fill_by,
              order_by = para$fig6_order_by,
              point = para$fig6_point,
              point_alpha = para$fig6_alpha
            ) %>% plotly::ggplotly()
        })
        # negative
        output$box_plt.neg <- renderUI({
          plot_type <- input$data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_box_plt.neg"))
          } else {
            plotOutput(outputId = ns("plot_box_plt.neg"))
          }
        })
        output$plot_box_plt.neg <- renderPlot({
          para = plot6_para()
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          p2_dataclean$object_neg %>%log() %>%
            dplyr::filter(class == "QC")%>%
            massqc::massqc_sample_boxplot(
              color_by = para$fig6_color_by,
              fill_by = para$fig6_fill_by,
              order_by = para$fig6_order_by,
              point = para$fig6_point,
              point_alpha = para$fig6_alpha
            )
        })
        output$plotly_box_plt.neg <- renderPlotly({
          para = plot6_para()
          if(is.null(input$data_clean_start)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          p2_dataclean$object_neg %>% log() %>%
            dplyr::filter(class == "QC")%>%
            massqc::massqc_sample_boxplot(
              color_by = para$fig6_color_by,
              fill_by = para$fig6_fill_by,
              order_by = para$fig6_order_by,
              point = para$fig6_point,
              point_alpha = para$fig6_alpha
            ) %>% plotly::ggplotly()
        })
      }
    )



    ##> download plot ==================
    ###> fig1 =====
    output$fig1_download = downloadHandler(
      filename = function() {
        paste0("01.Peak_distribution.", download_para()$fig1_format)
      },
      content = function(file) {
        # extract parameters
        para <- plot1_para()
        para_d <- download_para()

        # draw condition
        if (!is.null(p2_dataclean$object_pos) & !is.null(p2_dataclean$object_neg)) {
          para_d$fig1_width = para_d$fig1_width * 2
          p1 <- p2_dataclean$object_pos %>% massqc::show_mz_rt_plot(hex = para$fig1_hex)
          p2 <- p2_dataclean$object_neg %>% massqc::show_mz_rt_plot(hex = para$fig1_hex)
          p <- (p1 + ggtitle("Positive")) + (p2 + ggtitle("Negative"))
        } else if (!is.null(p2_dataclean$object_pos)) {
          p <- p2_dataclean$object_pos %>% massqc::show_mz_rt_plot(hex = para$fig1_hex)
        } else {
          p <- p2_dataclean$object_neg %>% massqc::show_mz_rt_plot(hex = para$fig1_hex)
        }

        # save plot
        ggsave(
          filename = file,
          plot = p,
          width = para_d$fig1_width,
          height = para_d$fig1_height,
          device = para_d$fig1_format
        )
      }
    )

    ###> fig2 ====
    output$fig2_download = downloadHandler(
      filename = function() {
        paste0("01.Missing_value_summary.", download_para()$fig2_format)
      },
      content = function(file) {
        # extract parameters
        para <- plot2_para()
        para_d <- download_para()

        # draw condition
        if (!is.null(p2_dataclean$object_pos) & !is.null(p2_dataclean$object_neg)) {
          para_d$fig2_width = para_d$fig2_width * 2
          p1 <- p2_dataclean$object_pos %>% massqc::show_missing_values(
            show_row_names = para$fig2_show_row_names,
            show_column_names = para$fig2_show_column_names,
            percentage = para$fig2_percentage,
            row_names_side = para$fig2_row_names_side,
            sample_na_cutoff = para$fig2_sample_na_cutoff,
            variable_na_cutoff = para$fig2_variable_na_cutoff,
            only_outlier_samples = para$fig2_only_outlier_samples,
            only_outlier_variables = para$fig2_only_outlier_variables,
            return_as_ggplot = TRUE
          )
          p2 <- p2_dataclean$object_neg %>% massqc::show_missing_values(
            show_row_names = para$fig2_show_row_names,
            show_column_names = para$fig2_show_column_names,
            percentage = para$fig2_percentage,
            row_names_side = para$fig2_row_names_side,
            sample_na_cutoff = para$fig2_sample_na_cutoff,
            variable_na_cutoff = para$fig2_variable_na_cutoff,
            only_outlier_samples = para$fig2_only_outlier_samples,
            only_outlier_variables = para$fig2_only_outlier_variables,
            return_as_ggplot = TRUE
          )
          p <- (p1 + ggtitle("Positive")) + (p2 + ggtitle("Negative"))
        } else if (!is.null(p2_dataclean$object_pos)) {
          p <- p2_dataclean$object_pos %>% massqc::show_missing_values(
            show_row_names = para$fig2_show_row_names,
            show_column_names = para$fig2_show_column_names,
            percentage = para$fig2_percentage,
            row_names_side = para$fig2_row_names_side,
            sample_na_cutoff = para$fig2_sample_na_cutoff,
            variable_na_cutoff = para$fig2_variable_na_cutoff,
            only_outlier_samples = para$fig2_only_outlier_samples,
            only_outlier_variables = para$fig2_only_outlier_variables,
            return_as_ggplot = TRUE
          )
        } else {
          p <- p2_dataclean$object_neg %>% massqc::show_missing_values(
            show_row_names = para$fig2_show_row_names,
            show_column_names = para$fig2_show_column_names,
            percentage = para$fig2_percentage,
            row_names_side = para$fig2_row_names_side,
            sample_na_cutoff = para$fig2_sample_na_cutoff,
            variable_na_cutoff = para$fig2_variable_na_cutoff,
            only_outlier_samples = para$fig2_only_outlier_samples,
            only_outlier_variables = para$fig2_only_outlier_variables,
            return_as_ggplot = TRUE
          )
        }

        # save plot

        ggsave(
          filename = file,
          plot = p,
          width = para_d$fig2_width,
          height = para_d$fig2_height,
          device = para_d$fig2_format
        )
      }
    )
    ###> fig3 ======
    output$fig3_download = downloadHandler(
      filename = function() {
        paste0("03.Sample_missing_value.", download_para()$fig3_format)
      },
      content = function(file) {
        # extract parameters
        para <- plot3_para()
        para_d <- download_para()

        # draw condition
        if (!is.null(p2_dataclean$object_pos) & !is.null(p2_dataclean$object_neg)) {
          para_d$fig3_width = para_d$fig3_width * 2
          p1 <- p2_dataclean$object_pos %>% massqc::show_sample_missing_values(
            color_by = para$fig3_color_by,
            order_by = para$fig3_order_by,
            percentage = para$fig3_percentage,
            show_x_text = para$fig3_show_x_text,
            show_x_ticks = para$fig3_show_x_ticks,
            desc = para$fig3_desc
          )
          p2 <- p2_dataclean$object_neg %>% massqc::show_sample_missing_values(
            color_by = para$fig3_color_by,
            order_by = para$fig3_order_by,
            percentage = para$fig3_percentage,
            show_x_text = para$fig3_show_x_text,
            show_x_ticks = para$fig3_show_x_ticks,
            desc = para$fig3_desc
          )
          p <- (p1 + ggtitle("Positive")) + (p2 + ggtitle("Negative"))
        } else if (!is.null(p2_dataclean$object_pos)) {
          p <- p2_dataclean$object_pos %>% massqc::show_sample_missing_values(
            color_by = para$fig3_color_by,
            order_by = para$fig3_order_by,
            percentage = para$fig3_percentage,
            show_x_text = para$fig3_show_x_text,
            show_x_ticks = para$fig3_show_x_ticks,
            desc = para$fig3_desc
          )
        } else {
          p <- p2_dataclean$object_neg %>% massqc::show_sample_missing_values(
            color_by = para$fig3_color_by,
            order_by = para$fig3_order_by,
            percentage = para$fig3_percentage,
            show_x_text = para$fig3_show_x_text,
            show_x_ticks = para$fig3_show_x_ticks,
            desc = para$fig3_desc
          )
        }

        # save plot
        ggsave(
          filename = file,
          plot = p,
          width = para_d$fig3_width,
          height = para_d$fig3_height,
          device = para_d$fig3_format
        )
      }
    )
    ###> fig4 =======

    output$fig4_download = downloadHandler(
      filename = function() {
        paste0("04.Variable_missing_value.", download_para()$fig4_format)
      },
      content = function(file) {
        # extract parameters
        para <- plot4_para()
        para_d <- download_para()

        # draw condition
        if (!is.null(p2_dataclean$object_pos) & !is.null(p2_dataclean$object_neg)) {
          para_d$fig4_width = para_d$fig4_width * 2
          p1 <- p2_dataclean$object_pos %>% massqc::show_variable_missing_values(
            color_by = para$fig4_color_by,
            order_by = para$fig4_order_by,
            percentage = para$fig4_percentage,
            show_x_text = para$fig4_show_x_text,
            show_x_ticks = para$fig4_show_x_ticks,
            desc = para$fig4_desc
          )
          p2 <- p2_dataclean$object_neg %>% massqc::show_variable_missing_values(
            color_by = para$fig4_color_by,
            order_by = para$fig4_order_by,
            percentage = para$fig4_percentage,
            show_x_text = para$fig4_show_x_text,
            show_x_ticks = para$fig4_show_x_ticks,
            desc = para$fig4_desc
          )
          p <- (p1 + ggtitle("Positive")) + (p2 + ggtitle("Negative"))
        } else if (!is.null(p2_dataclean$object_pos)) {
          p <- p2_dataclean$object_pos %>% massqc::show_variable_missing_values(
            color_by = para$fig4_color_by,
            order_by = para$fig4_order_by,
            percentage = para$fig4_percentage,
            show_x_text = para$fig4_show_x_text,
            show_x_ticks = para$fig4_show_x_ticks,
            desc = para$fig4_desc
          )
        } else {
          p <- p2_dataclean$object_neg %>% massqc::show_variable_missing_values(
            color_by = para$fig4_color_by,
            order_by = para$fig4_order_by,
            percentage = para$fig4_percentage,
            show_x_text = para$fig4_show_x_text,
            show_x_ticks = para$fig4_show_x_ticks,
            desc = para$fig4_desc
          )
        }

        # save plot
        ggsave(
          filename = file,
          plot = p,
          width = para_d$fig4_width,
          height = para_d$fig4_height,
          device = para_d$fig4_format
        )
      }
    )
    ###> fig5 ==============
    output$fig5_download = downloadHandler(
    filename = function() {
      paste0("05.rsd_plot.", download_para()$fig5_format)
    },
    content = function(file) {
      # extract parameters
      para <- plot5_para()
      para_d <- download_para()

      # draw condition
      if (!is.null(p2_dataclean$object_pos) & !is.null(p2_dataclean$object_neg)) {
        para_d$fig5_width = para_d$fig5_width * 2
        p1 <- p2_dataclean$object_pos %>%
          dplyr::filter(class == "QC")%>%
          massqc::massqc_cumulative_rsd_plot(
            rsd_cutoff = para$fig5_rsd_cutoff,
            color = para$fig5_color,
            title = 'All of QC sample'
          )
        p2 <- p2_dataclean$object_neg %>%
          dplyr::filter(class == "QC")%>%
          massqc::massqc_cumulative_rsd_plot(
            rsd_cutoff = para$fig5_rsd_cutoff,
            color = para$fig5_color,
            title = 'All of QC sample'
          )
        p <- (p1 + ggtitle("Positive")) + (p2 + ggtitle("Negative"))
      } else if (!is.null(p2_dataclean$object_pos)) {
        p <- p2_dataclean$object_pos %>%
          dplyr::filter(class == "QC")%>%
          massqc::massqc_cumulative_rsd_plot(
            rsd_cutoff = para$fig5_rsd_cutoff,
            color = para$fig5_color,
            title = 'All of QC sample'
          )
      } else {
        p <- p2_dataclean$object_neg %>%
          dplyr::filter(class == "QC")%>%
          massqc::massqc_cumulative_rsd_plot(
            rsd_cutoff = para$fig5_rsd_cutoff,
            color = para$fig5_color,
            title = 'All of QC sample'
          )
      }

      # save plot
      ggsave(
        filename = file,
        plot = p,
        width = para_d$fig5_width,
        height = para_d$fig5_height,
        device = para_d$fig5_format
      )
    }
    )

    ###> fig6 ==============
    output$fig6_download = downloadHandler(
      filename = function() {
        paste0("06.rsd_plot.", download_para()$fig6_format)
      },
      content = function(file) {
        # extract parameters
        para <- plot6_para()
        para_d <- download_para()

        # draw condition
        if (!is.null(p2_dataclean$object_pos) & !is.null(p2_dataclean$object_neg)) {
          para_d$fig6_width = para_d$fig6_width * 2
          p1 <- p2_dataclean$object_pos %>%log() %>%
            dplyr::filter(class == "QC")%>%
            massqc::massqc_sample_boxplot(
              color_by = para$fig6_color_by,
              fill_by = para$fig6_fill_by,
              order_by = para$fig6_order_by,
              point = para$fig6_point,
              point_alpha = para$fig6_alpha
            )
          p2 <- p2_dataclean$object_neg %>%log() %>%
            dplyr::filter(class == "QC")%>%
            massqc::massqc_sample_boxplot(
              color_by = para$fig6_color_by,
              fill_by = para$fig6_fill_by,
              order_by = para$fig6_order_by,
              point = para$fig6_point,
              point_alpha = para$fig6_alpha
            )
          p <- (p1 + ggtitle("Positive")) + (p2 + ggtitle("Negative"))
        } else if (!is.null(p2_dataclean$object_pos)) {
          p <- p2_dataclean$object_pos %>%log() %>%
            dplyr::filter(class == "QC")%>%
            massqc::massqc_sample_boxplot(
              color_by = para$fig6_color_by,
              fill_by = para$fig6_fill_by,
              order_by = para$fig6_order_by,
              point = para$fig6_point,
              point_alpha = para$fig6_alpha
            )
        } else {
          p <- p2_dataclean$object_neg %>%log() %>%
            dplyr::filter(class == "QC")%>%
            massqc::massqc_sample_boxplot(
              color_by = para$fig6_color_by,
              fill_by = para$fig6_fill_by,
              order_by = para$fig6_order_by,
              point = para$fig6_point,
              point_alpha = para$fig6_alpha
            )
        }

        # save plot
        ggsave(
          filename = file,
          plot = p,
          width = para_d$fig6_width,
          height = para_d$fig6_height,
          device = para_d$fig6_format
        )
      }
    )
    ###>
    ###>
  }
  )
  }





