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
      sidebar = accordion(open = T,
        accordion_panel(
          title = "Show plot",
          icon = bsicons::bs_icon("play-fill"),
          actionButton(inputId = ns("vis_butt_1"),label = "Start",icon = icon("play"))
        ),
        accordion_panel(
          title = "Methods",
          icon = bsicons::bs_icon("gear"),
          selectInput(
            inputId = ns("mv_method"),
            label = "Outlier removal strategy",
            choices = c("By tidymass","By myself"),
            selected = "By myself"
          ),
          hr_bar(),
          h5("By tidymass"),
          selectInput(
            inputId = ns("by_witch"),
            label = "remove outlier conditions",
            choices = c("according_to_na","pc_sd","pc_mad","accordint_to_distance"),
            selected = "according_to_na",multiple = T
          ),
          tags$span(textOutput(outputId = ns("outlier_in_pos_1")), class = "text-wrap"),
          hr_head(),
          tags$span(textOutput(outputId = ns("outlier_in_neg_1")), class = "text-wrap"),
          hr_bar(),
          h5("By myself"),
          selectInput(
            inputId = ns("outlier_in_pos"),
            label = "Outliers (positive)",
            choices = c("none"),
            selected = "none",multiple = T
          ),
          selectInput(
            inputId = ns("outlier_in_neg"),
            label = "Outliers (negative)",
            choices = c("none"),
            selected = "none",multiple = T
          ),
          hr_bar(),
          actionButton(
            inputId = ns("mv_start"),
            label = "Start",icon = icon("play")
          ),
        )
        ),

      page_fluid(
        nav_panel(title = "remove outlier",
                  ##> column 1-2 ========
                  layout_column_wrap(
                    width = 1/2,
                    height = 350,
                    navset_card_tab(
                      ##> mv in sample
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
                          ),
                          materialSwitch(inputId = ns("fig1_data_clean_plt_format"),label = "Interactive plot", status = "primary"),
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
                        card_title("MV percentage (sample) in positive model"),
                        uiOutput(ns("smv_plt.pos"),fill = T)

                      ),
                      nav_panel(
                        "Negative",
                        card_title("MV percentage (sample) in negative model"),
                        uiOutput(ns("smv_plt.neg"),fill = T)

                      )
                    ),
                    ##> PCA
                    navset_card_tab(
                      height = 350,
                      full_screen = TRUE,
                      title = "PCA",
                      sidebar = accordion(
                        open = FALSE,
                        accordion_panel(
                          title = 'Parameters',
                          selectInput(
                            inputId = ns('fig2_color_by'), label = 'color by', choices = c('group','class',"..."),selected = 'class',multiple = F
                          ),
                          radioButtons(
                            inputId = ns('fig2_scale'),label = 'scale',choices = c('TRUE','FALSE'),selected = 'FALSE',
                          ),
                          sliderInput(
                            inputId = ns('fig2_point_alpha'),label = 'point alpha',min = 0,max = 1,value = 0.8,step = 0.1
                          ),
                          radioButtons(
                            inputId = ns('fig2_frame'),label = 'frame',choices = c('TRUE',"FALSE"),selected = 'TRUE'
                          ),
                          radioButtons(
                            inputId = ns('fig2_line'),label = 'Add line', choices = c('TRUE','FALSE'),selected = 'TRUE'
                          ),
                        ),
                        accordion_panel(
                          title = '3D Plot',
                          materialSwitch(inputId = ns("fig2_data_clean_plt_format"),label = "Interactive plot", status = "primary"),
                          selectInput(
                            inputId = ns('fig2_color_by_3d'), label = 'color by', choices = c('group','class',"..."),selected = 'class',multiple = F
                          ),
                          radioButtons(
                            inputId = ns('fig2_scale_3d'),label = 'scale',choices = c('TRUE','FALSE'),selected = 'FALSE',
                          ),
                          selectInput(
                            inputId = ns('fig2_x_axis'),label = 'PC(n) for x axis',choices = paste0('PC',1:10),selected = 'PC1',multiple = F
                          ),
                          selectInput(
                            inputId = ns('fig2_y_axis'),label = 'PC(n) for y axis',choices = paste0('PC',1:10),selected = 'PC2',multiple = F
                          ),
                          selectInput(
                            inputId = ns('fig2_z_axis'),label = 'PC(n) for z axis',choices = paste0('PC',1:10),selected = 'PC3',multiple = F
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
                        card_title("PCA plot in positive model"),
                        uiOutput(ns("fig2_pca.pos"),fill = T)
                      ),
                      nav_panel(
                        "Negative",
                        card_title("PCA plot in negative model"),
                        uiOutput(ns("fig2_pca.neg"),fill = T)
                      )
                    )),
                  navset_card_tab(
                    title = "Summary of outlier detection",
                    height = 400,
                    full_screen = TRUE,
                    nav_panel(
                      "Positive",
                      verbatimTextOutput(ns("info_outlier.pos")),
                      hr_bar(),
                      dataTableOutput(ns("tbl_outlier.pos"))
                    ),
                    nav_panel(
                      "Negative",
                      verbatimTextOutput(ns("info_outlier.neg")),
                      hr_bar(),
                      dataTableOutput(ns("tbl_outlier.neg"))
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
#' @param data_export_rv reactivevalues mass_dataset export
#' @noRd


remove_outlier_server <- function(id,volumes,prj_init,data_import_rv,data_clean_rv,data_export_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    p2_dataclean <- reactiveValues(data = NULL)

    analy_para = reactive({
      list(
        mv_method = input$mv_method %>% as.character(),
        by_witch = input$by_witch %>% as.character(),
        outlier_in_pos = input$outlier_in_pos %>% as.character(),
        outlier_in_neg = input$outlier_in_neg %>% as.character()
      )
    })

    ###> plot parameters =========
    plot1_para = reactive({
      list(
        fig1_color_by = input$color_by_smv %>% as.character(),
        fig1_order_by = input$order_by_smv %>% as.character(),
        fig1_percentage = input$percentage_smv %>% as.logical(),
        fig1_show_x_text = input$show_x_text_smv %>% as.logical(),
        fig1_show_x_ticks = input$show_x_ticks_smv %>% as.logical(),
        fig1_desc = input$desc_smv %>% as.logical()
      )
    })

    plot2_para = reactive({
      list(
        fig2_color_by = input$fig2_color_by %>% as.character(),
        fig2_scale = input$fig2_scale %>% as.logical(),
        fig2_point_alpha = input$fig2_point_alpha %>% as.numeric(),
        fig2_frame = input$fig2_frame %>% as.logical(),
        fig2_line = input$fig2_line %>% as.logical(),
        fig2_color_by_3d = input$fig2_color_by_3d %>% as.character(),
        fig2_scale_3d = input$fig2_scale_3d %>% as.logical(),
        fig2_x_axis = input$fig2_x_axis %>% as.character(),
        fig2_y_axis = input$fig2_y_axis %>% as.character(),
        fig2_z_axis = input$fig2_z_axis %>% as.character()
      )
    })
    ###> download parameters =========
    download_para = reactive({
      list(
        ##> fig1
        fig1_width = as.numeric(input$fig1_width),
        fig1_height = as.numeric(input$fig1_height),
        fig1_format = as.character(input$fig1_format),
        ##> fig2
        fig2_width = as.numeric(input$fig2_width),
        fig2_height = as.numeric(input$fig2_height),
        fig2_format = as.character(input$fig2_format)
      )
    })

    observe({
      sample_info_colnames <- NULL
      variable_info_colnames <- NULL

      if (!is.null(prj_init$object_negative.init)) {
        sample_info_colnames <- colnames(prj_init$object_negative.init@sample_info)
      } else if (!is.null(prj_init$object_positive.init)) {
        sample_info_colnames <- colnames(prj_init$object_positive.init@sample_info)
      } else if (!is.null(data_import_rv$object_neg.mv)) {
        sample_info_colnames <- colnames(data_import_rv$object_neg.mv@sample_info)
      } else if (!is.null(data_import_rv$object_pos.mv)) {
        sample_info_colnames <- colnames(data_import_rv$object_pos.mv@sample_info)
      }

      # update
      if (!is.null(sample_info_colnames)) {
        updateSelectInput(session, "color_by_smv", choices = sample_info_colnames, selected = "group")
        updateSelectInput(session, "order_by_smv", choices = sample_info_colnames, selected = "injection.order")
        updateSelectInput(session, "fig2_color_by", choices = sample_info_colnames, selected = "group")
        updateSelectInput(session, "fig2_color_by_3d", choices = sample_info_colnames, selected = "group")
      }

    })

    ##> draw plot ==================
    observeEvent(
      input$vis_butt_1,
      {

        ####> check object ===============
        if(is.null(prj_init$sample_info)) {return()}

        if(!is.null(prj_init$object_negative.init) & !is.null(prj_init$object_positive.init) & prj_init$steps == "Remove outlier"){
          p2_dataclean$object_neg = prj_init$object_negative.init
          p2_dataclean$object_pos = prj_init$object_positive.init
        } else {
          if(is.null(data_import_rv$object_neg.mv)) {return()}
          if(is.null(data_import_rv$object_neg.mv)) {return()}
          p2_dataclean$object_neg = data_import_rv$object_neg.mv
          p2_dataclean$object_pos = data_import_rv$object_neg.mv
        }


        ###> fig1 missing value in samples ================

        # positive
        output$smv_plt.pos <- renderUI({
          plot_type <- input$fig1_data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_smv_plt.pos"))
          } else {
            plotOutput(outputId = ns("plot_smv_plt.pos"))
          }
        })
        output$plot_smv_plt.pos <- renderPlot({
          para = plot1_para()
          if(is.null(input$vis_butt_1)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          if(is.null(data_clean_rv$object_pos.outlier)) {
            temp_obj = p2_dataclean$object_pos
          } else{
            temp_obj = data_clean_rv$object_pos.outlier
          }
          temp_obj %>% massqc::show_sample_missing_values(
            color_by = para$fig1_color_by,
            order_by = para$fig1_order_by,
            percentage = para$fig1_percentage,
            show_x_text = para$fig1_show_x_text,
            show_x_ticks = para$fig1_show_x_ticks,
            desc = para$fig1_desc
          )
        })
        output$plotly_smv_plt.pos <- renderPlotly({
          para = plot1_para()
          if(is.null(input$vis_butt_1)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          if(is.null(data_clean_rv$object_pos.outlier)) {
            temp_obj = p2_dataclean$object_pos
          } else{
            temp_obj = data_clean_rv$object_pos.outlier
          }
          temp_obj %>% massqc::show_sample_missing_values(
            color_by = para$fig1_color_by,
            order_by = para$fig1_order_by,
            percentage = para$fig1_percentage,
            show_x_text = para$fig1_show_x_text,
            show_x_ticks = para$fig1_show_x_ticks,
            desc = para$fig1_desc
          ) %>% plotly::ggplotly()
        })
        # negative
        output$smv_plt.neg <- renderUI({
          plot_type <- input$fig1_data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_smv_plt.neg"))
          } else {
            plotOutput(outputId = ns("plot_smv_plt.neg"))
          }
        })
        output$plot_smv_plt.neg <- renderPlot({
          para = plot1_para()
          if(is.null(input$vis_butt_1)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          if(is.null(data_clean_rv$object_neg.outlier)) {
            temp_obj = p2_dataclean$object_neg
          } else{
            temp_obj = data_clean_rv$object_neg.outlier
          }
          temp_obj %>% massqc::show_sample_missing_values(
            color_by = para$fig1_color_by,
            order_by = para$fig1_order_by,
            percentage = para$fig1_percentage,
            show_x_text = para$fig1_show_x_text,
            show_x_ticks = para$fig1_show_x_ticks,
            desc = para$fig1_desc
          )
        })
        output$plotly_smv_plt.neg <- renderPlotly({
          para = plot1_para()
          if(is.null(input$vis_butt_1)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          if(is.null(data_clean_rv$object_neg.outlier)) {
            temp_obj = p2_dataclean$object_neg
          } else{
            temp_obj = data_clean_rv$object_neg.outlier
          }
          temp_obj %>% massqc::show_sample_missing_values(
            color_by = para$fig1_color_by,
            order_by = para$fig1_order_by,
            percentage = para$fig1_percentage,
            show_x_text = para$fig1_show_x_text,
            show_x_ticks = para$fig1_show_x_ticks,
            desc = para$fig1_desc
          ) %>% plotly::ggplotly()
        })

        ###> fig1 PCA =============
        output$fig2_pca.pos <- renderUI({
          plot_type <- input$fig2_data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_pca.pos"))
          } else {
            plotOutput(outputId = ns("plot_pca.pos"))
          }
        })
        output$plot_pca.pos <- renderPlot({
          para = plot2_para()
          if(is.null(input$vis_butt_1)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          if(is.null(data_clean_rv$object_pos.outlier)) {
            temp_obj = p2_dataclean$object_pos
          } else{
            temp_obj = data_clean_rv$object_pos.outlier
          }
          if(isTRUE(para$fig2_scale)) {
            temp_obj.pos <- temp_obj %>% +1 %>% log(2) %>% scale()
          } else {
            temp_obj.pos <- temp_obj %>% +1 %>% log(2)
          }
          temp_obj.pos %>%
            massqc::massqc_pca(
              color_by = para$fig2_color_by,
              point_alpha = para$fig2_point_alpha,
              frame = para$fig2_frame,
              line = para$fig2_line
            )
        })
        output$plotly_pca.pos <- renderPlotly({
          para = plot2_para()
          if(is.null(input$vis_butt_1)){return()}
          if(is.null(p2_dataclean$object_pos)){return()}
          if(is.null(data_clean_rv$object_pos.outlier)) {
            temp_obj = p2_dataclean$object_pos
          } else{
            temp_obj = data_clean_rv$object_pos.outlier
          }
          if(isTRUE(para$fig2_scale_3d)) {
            temp_obj.pos <- temp_obj %>% +1 %>% log(2) %>% scale()
          } else {
            temp_obj.pos <- temp_obj %>% +1 %>% log(2)
          }
          temp_obj.pos %>%
            massqc_pca_3d(
              color_by = para$fig2_color_by_3d,
              x_axis = para$fig2_x_axis,
              y_axis = para$fig2_y_axis,
              z_axis = para$fig2_z_axis
            )
        })
        # negative
        output$fig2_pca.neg <- renderUI({
          plot_type <- input$fig2_data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_pca.neg"))
          } else {
            plotOutput(outputId = ns("plot_pca.neg"))
          }
        })
        output$plot_pca.neg <- renderPlot({
          para = plot2_para()
          if(is.null(input$vis_butt_1)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          if(is.null(data_clean_rv$object_neg.outlier)) {
            temp_obj = p2_dataclean$object_neg
          } else{
            temp_obj = data_clean_rv$object_neg.outlier
          }
          if(isTRUE(para$fig2_scale)) {
            temp_obj.neg <- temp_obj %>% +1 %>% log(2) %>% scale()
          } else {
            temp_obj.neg <- temp_obj %>% +1 %>% log(2)
          }

          temp_obj.neg %>%
            massqc::massqc_pca(
              color_by = para$fig2_color_by,
              point_alpha = para$fig2_point_alpha,
              frame = para$fig2_frame,
              line = para$fig2_line
            )
        })
        output$plotly_pca.neg <- renderPlotly({
          para = plot2_para()
          if(is.null(input$vis_butt_1)){return()}
          if(is.null(p2_dataclean$object_neg)){return()}
          if(is.null(data_clean_rv$object_neg.outlier)) {
            temp_obj = p2_dataclean$object_neg
          } else{
            temp_obj = data_clean_rv$object_neg.outlier
          }
          if(isTRUE(para$fig2_scale_3d)) {
            temp_obj.neg <- temp_obj %>% +1 %>% log(2) %>% scale()
          } else {
            temp_obj.neg <- temp_obj %>% +1 %>% log(2)
          }

          temp_obj.neg %>%
            massqc_pca_3d(
              color_by = para$fig2_color_by_3d,
              x_axis = para$fig2_x_axis,
              y_axis = para$fig2_y_axis,
              z_axis = para$fig2_z_axis
            )
        })
        ####>  outliers ========================

        outlier_samples.neg <-
          p2_dataclean$object_neg %>%
          `+`(1) %>%
          log(2) %>%
          scale() %>%
          detect_outlier()

        outlier_samples.pos <-
          p2_dataclean$object_pos %>%
          `+`(1) %>%
          log(2) %>%
          scale() %>%
          detect_outlier()

      output$info_outlier.neg = renderPrint({
        print(outlier_samples.neg)
      })

      output$info_outlier.pos = renderPrint({
        print(outlier_samples.pos)
      })
      ####> outlier detailed table
      p2_dataclean$outlier_tbl.neg = extract_outlier_table(outlier_samples.neg)
      p2_dataclean$outlier_tbl.pos = extract_outlier_table(outlier_samples.pos)

      output$tbl_outlier.pos = renderDataTable_formated(
        actions = input$vis_butt_1,
        filename.a = "outlier_summary_pos",
        tbl = p2_dataclean$outlier_tbl.pos
      )
      output$tbl_outlier.neg = renderDataTable_formated(
        actions = input$vis_butt_1,
        filename.a = "outlier_summary_neg",
        tbl = p2_dataclean$outlier_tbl.neg
      )
      }
    )

    ## > remove outlier
    observeEvent(
      input$mv_start,
      {
        print('check point 1')
        if(is.null(p2_dataclean$outlier_tbl.neg)){return()}
        if(is.null(p2_dataclean$outlier_tbl.pos)){return()}
        para = analy_para()
        print('check point 2')
        print(para$mv_method)

        if(para$mv_method == "By tidymass") {
          print('check point 3')
          by_witch = para$by_witch %>% paste0(collapse = "|")
          p2_dataclean$outlier_sid.pos =
            p2_dataclean$outlier_tbl.pos %>%
            rownames_to_column("sample_id") %>%
            pivot_longer(
              !sample_id,values_to = 'judge',names_to = 'condition'
            ) %>%
            dplyr::filter(str_detect(condition,by_witch)) %>%
            group_by(sample_id) %>%
            summarise(is_outlier = all(judge == TRUE)) %>%
            filter(is_outlier) %>%
            ungroup() %>%
            pull(sample_id)
          print('check point 4')
          p2_dataclean$outlier_sid.neg =
            p2_dataclean$outlier_tbl.neg %>%
            rownames_to_column("sample_id") %>%
            pivot_longer(
              !sample_id,values_to = 'judge',names_to = 'condition'
            ) %>%
            dplyr::filter(str_detect(condition,by_witch)) %>%
            group_by(sample_id) %>%
            summarise(is_outlier = all(judge == TRUE)) %>%
            filter(is_outlier) %>%
            ungroup() %>%
            pull(sample_id)
          print('check point 5')

          if(length(p2_dataclean$outlier_sid.pos) > 0) {
            output$outlier_in_pos_1 = renderPrint({
              print(p2_dataclean$outlier_sid.pos)
            })
            print('check point 6')
          } else {
            output$outlier_in_pos_1 = renderPrint({
              print("No outliers!")
            })
            print('check point 7')
          }
          if(length(p2_dataclean$outlier_sid.neg) > 0) {
            output$outlier_in_neg_1 = renderPrint({
              print(p2_dataclean$outlier_sid.neg)
            })
            print('check point 8')
          } else {
            output$outlier_in_neg_1 = renderPrint({
              print("No outliers!")
            })
            print('check point 9')
          }
        }
        if(para$mv_method == "By myself") {
          print('check point 10')
          p2_dataclean$outlier_sid.pos = para$outlier_in_pos
          p2_dataclean$outlier_sid.neg = para$outlier_in_neg
        }
        if(length(p2_dataclean$outlier_sid.pos) > 0 & !"none" %in% p2_dataclean$outlier_sid.pos ) {
          print('check point 11')
          p2_dataclean$object_pos =
            p2_dataclean$object_pos %>% activate_mass_dataset(what = 'sample_info') %>%
            filter(!sample_id %in% p2_dataclean$outlier_sid.pos)
        }
        if(length(p2_dataclean$outlier_sid.neg) > 0 & !"none" %in% p2_dataclean$outlier_sid.neg) {
          print('check point 12')
          p2_dataclean$object_neg =
            p2_dataclean$object_neg %>% activate_mass_dataset(what = 'sample_info') %>%
            filter(!sample_id %in% p2_dataclean$outlier_sid.neg)
        }
        print('check point 13')

        output$obj_outlier.pos = renderPrint({
          print(p2_dataclean$object_pos)
        })
        output$obj_outlier.neg = renderPrint({
          print(p2_dataclean$object_neg)
        })

        data_clean_rv$object_pos.outlier = p2_dataclean$object_pos
        data_clean_rv$object_neg.outlier = p2_dataclean$object_neg

      })

  })
}

