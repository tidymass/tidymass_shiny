#' Merge data and export
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


merge_data_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Merge data',
    icon = bs_icon("basket"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Manner",
          icon = bsicons::bs_icon("stars"),
          selectInput(
            inputId = ns("sample_direction"),
            label = tooltip(
              trigger = list(
                "sample direction",
                bsicons::bs_icon("info-circle")
              ),
              "How to merge samples, should be left, right, inner or full. "
              ),
            choices = c("left", "right", "full", "inner"),
            selected = "inner",multiple = FALSE
          ),
          selectInput(
            inputId = ns("variable_direction"),
            label = "variable direction",
            choices = c("left", "right", "full", "inner"),
            selected = "full",multiple = FALSE
          ),
          selectInput(
            inputId = ns("sample_by"),
            label = "sample by",
            choices = c("sample_id"),
            selected = "sample_id",multiple = TRUE
          ),
          selectInput(
            inputId = ns("variable_by"),
            label = "sample by",
            choices = c("variable_id", "mz", "rt"),
            selected = c("variable_id", "mz", "rt"),multiple = TRUE
          ),
          actionButton(inputId = ns("merge_start"),label = 'Start',icon = icon("play"))
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1/2,
          height = 350,
          navset_card_tab(
            height = 350,
            full_screen = TRUE,
            title = "PCA",
            sidebar = accordion(
              open = FALSE,
              accordion_panel(
                title = 'Parameters',
                selectInput(
                  inputId = ns('fig1_color_by'), label = 'color by', choices = c('group','class',"..."),selected = 'class',multiple = F
                ),
                radioButtons(
                  inputId = ns('fig1_scale'),label = 'scale',choices = c('TRUE','FALSE'),selected = 'FALSE',
                ),
                sliderInput(
                  inputId = ns('fig1_point_alpha'),label = 'point alpha',min = 0,max = 1,value = 0.8,step = 0.1
                ),
                radioButtons(
                  inputId = ns('fig1_frame'),label = 'frame',choices = c('TRUE',"FALSE"),selected = 'TRUE'
                ),
                radioButtons(
                  inputId = ns('fig1_line'),label = 'Add line', choices = c('TRUE','FALSE'),selected = 'TRUE'
                ),
              ),
              accordion_panel(
                title = '3D Plot',
                materialSwitch(inputId = ns("fig1_data_clean_plt_format"),label = "Interactive plot", status = "primary"),
                selectInput(
                  inputId = ns('fig1_color_by_3d'), label = 'color by', choices = c('group','class',"..."),selected = 'class',multiple = F
                ),
                radioButtons(
                  inputId = ns('fig1_scale_3d'),label = 'scale',choices = c('TRUE','FALSE'),selected = 'FALSE',
                ),
                selectInput(
                  inputId = ns('fig1_x_axis'),label = 'PC(n) for x axis',choices = paste0('PC',1:10),selected = 'PC1',multiple = F
                ),
                selectInput(
                  inputId = ns('fig1_y_axis'),label = 'PC(n) for y axis',choices = paste0('PC',1:10),selected = 'PC2',multiple = F
                ),
                selectInput(
                  inputId = ns('fig1_z_axis'),label = 'PC(n) for z axis',choices = paste0('PC',1:10),selected = 'PC3',multiple = F
                )
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
              "PCA",
              card_title("PCA plot in positive model"),
              uiOutput(ns("fig1_pca.pos"),fill = T)
            )
          ),
          navset_card_tab(
            height = 350,
            full_screen = TRUE,
            title = "Sample correlation",
            sidebar =
              accordion(
                open = FALSE,
                accordion_panel(
                  title = 'Parameters',
                  radioButtons(
                    inputId = ns('fig2_class_by'),label = 'class by',choices = c("QC","Subject","All"),selected = "Subject"
                  ),
                  selectInput(
                    inputId = ns('fig2_cor_method'),label = 'correlation method',
                    choices = c("spearman", "kendall", "pearson"),selected = 'spearman', multiple = F
                  ),
                  selectInput(
                    inputId = ns('fig2_method'),label = 'method',choices = c("circle", "square"),selected = 'square',
                    multiple = F
                  ),
                  selectInput(
                    inputId = ns('fig2_type'),label = 'type',choices = c("full", "lower", "upper"),selected = 'full',
                    multiple = F
                  ),
                  colourpicker::colourInput(
                    inputId = ns('fig2_min'),label = 'minimal',value = 'blue'
                  ),
                  colourpicker::colourInput(
                    inputId = ns('fig2_mid'),label = 'middle',value = 'white'
                  ),
                  colourpicker::colourInput(
                    inputId = ns('fig2_max'),label = 'maximum',value = 'red'
                  ),
                  colourpicker::colourInput(
                    inputId = ns('fig2_outlier.color'),label = 'outlier color',value = 'grey'
                  ),
                  selectInput(
                    inputId = ns('fig2_order_by'),label = 'order by',choices = c('sample_id','injection.order'),selected = 'sample_id'
                  ),
                  materialSwitch(inputId = ns("fig2_data_clean_plt_format"),label = "Interactive plot", status = "primary")
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
              "correlation",
              card_title("Sample correlation in positive model"),
              uiOutput(ns("fig2_corr_plt.pos"),fill = T)
            )
          )
        ),
        navset_card_tab(
          title = "Status",
          height = 400,
          full_screen = TRUE,
          nav_panel("Object", verbatimTextOutput(ns("obj_merge"))),
          nav_panel("processing information",
                    actionButton(ns("para_report"),label = "generate processing information report",icon = icon('play')),
                    verbatimTextOutput(ns("process_obj")))
        )
      )
    )
  )
}


#' merge massdataset object
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs toggle runjs useShinyjs
#' @importFrom dplyr select left_join
#' @importFrom massdataset activate_mass_dataset extract_process_info
#' @importFrom plotly renderPlotly plotlyOutput
#' @param id module of server
#' @param volumes shinyFiles volumes
#' @param prj_init use project init variables.
#' @param data_import_rv reactivevalues mass_dataset export
#' @param data_clean_rv reactivevalues p2 dataclean
#' @param p2_af_filter reactivevalues anno filtering
#' @param data_export_rv reactivevalues mass_dataset export
#' @noRd


merge_data_server <-
  function(id,
           volumes,
           prj_init,
           data_import_rv,
           data_clean_rv,
           data_export_rv,
           p2_af_filter) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # parameters --------------------------------------------------------------

      merge_para = reactive({
        list(
          sample_direction = input$sample_direction %>% as.character(),
          variable_direction = input$variable_direction %>% as.character(),
          sample_by = input$sample_by %>% as.character(),
          variable_by = input$variable_by %>% as.character()
        )
      })
      plot1_para = reactive({
        list(
          fig1_color_by = input$fig1_color_by %>% as.character(),
          fig1_scale = input$fig1_scale %>% as.logical(),
          fig1_point_alpha = input$fig1_point_alpha %>% as.numeric(),
          fig1_frame = input$fig1_frame %>% as.logical(),
          fig1_line = input$fig1_line %>% as.logical(),
          fig1_color_by_3d = input$fig1_color_by_3d %>% as.character(),
          fig1_scale_3d = input$fig1_scale_3d %>% as.logical(),
          fig1_x_axis = input$fig1_x_axis %>% as.character(),
          fig1_y_axis = input$fig1_y_axis %>% as.character(),
          fig1_z_axis = input$fig1_z_axis %>% as.character()
        )
      })
      plot2_para = reactive({
        list(
          fig2_class_by = input$fig2_class_by %>% as.character(),
          fig2_cor_method = input$fig2_cor_method %>% as.character(),
          fig2_method = input$fig2_method %>% as.character(),
          fig2_type = input$fig2_type %>% as.character(),
          fig2_min = input$fig2_min %>% as.character(),
          fig2_mid = input$fig2_mid %>% as.character(),
          fig2_max = input$fig2_max %>% as.character(),
          fig2_outlier.color = input$fig2_outlier.color %>% as.character(),
          fig2_order_by = input$fig2_order_by %>% as.character()
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
          fig2_format = as.character(input$fig2_format)
        )
      })

      ## start run ==============================

      observeEvent(
        input$merge_start,
        {
          if(!is.null(prj_init$object_negative.init) & !is.null(prj_init$object_positive.init) & prj_init$steps == "Merge data"){
            p2_af_filter$object_neg.af = prj_init$object_negative.init
            p2_af_filter$object_pos.af = prj_init$object_positive.init
          } else {
            if(is.null(data_clean_rv$object_neg.af)){return()}
            if(is.null(data_clean_rv$object_pos.af)){return()}
            p2_af_filter$object_neg.af = data_clean_rv$object_neg.af
            p2_af_filter$object_pos.af = data_clean_rv$object_pos.af
          }
          ##> import parameters
          merge_para = merge_para()
          print(merge_para)
          print("check point 1")
          object_merge = merge_mass_dataset_fix(
            x = p2_af_filter$object_neg.af,
            y = p2_af_filter$object_pos.af,
            sample_direction = merge_para$sample_direction,
            variable_direction = merge_para$variable_direction,
            sample_by = merge_para$sample_by,
            variable_by = merge_para$variable_by
          ) %>% activate_mass_dataset('sample_info') %>%
            dplyr::filter(class != "QC")
          ###> fig1 PCA =============
          output$fig1_pca.pos <- renderUI({
            plot_type <- input$fig1_data_clean_plt_format
            if (plot_type) {
              plotlyOutput(outputId = ns("plotly_pca.pos"))
            } else {
              plotOutput(outputId = ns("plot_pca.pos"))
            }
          })
          output$plot_pca.pos <- renderPlot({
            para = plot1_para()
            if(is.null(input$merge_start)){return()}
            if(is.null(object_merge)){return()}
            if(isTRUE(para$fig1_scale)) {
              temp_obj <- object_merge %>% +1 %>% log(2) %>% scale()
            } else {
              temp_obj <- object_merge %>% +1 %>% log(2)
            }

            temp_obj %>%
              massqc::massqc_pca(
                color_by = para$fig1_color_by,
                point_alpha = para$fig1_point_alpha,
                frame = para$fig1_frame,
                line = para$fig1_line
              )
          })
          output$plotly_pca.pos <- renderPlotly({
            para = plot1_para()
            if(is.null(input$merge_start)){return()}
            if(is.null(object_merge)){return()}
            if(isTRUE(para$fig1_scale_3d)) {
              temp_obj <- object_merge %>% +1 %>% log(2) %>% scale()
            } else {
              temp_obj <- object_merge %>% +1 %>% log(2)
            }
            temp_obj %>%
              massqc_pca_3d(
                color_by = para$fig1_color_by_3d,
                x_axis = para$fig1_x_axis,
                y_axis = para$fig1_y_axis,
                z_axis = para$fig1_z_axis
              )
          })
          ###> fig2 Correlation =============

          output$fig2_corr_plt.pos <- renderUI({
            plot_type <- input$fig2_data_clean_plt_format
            if (plot_type) {
              plotlyOutput(outputId = ns("plotly_corr_plt.pos"))
            } else {
              plotOutput(outputId = ns("plot_corr_plt.pos"))
            }
          })
          output$plot_corr_plt.pos <- renderPlot({
            para = plot2_para()
            if(is.null(input$merge_start)){return()}
            if(is.null(object_merge)){return()}
            object_merge %>%
              massqc::massqc_sample_correlation(
                cor_method = para$fig2_cor_method,
                method = para$fig2_method,
                type = para$fig2_type,
                colors = c(para$fig2_min,para$fig2_mid,para$fig2_max),
                outline.color = para$fig2_outlier.color,
                order_by = para$fig2_order_by
              )
          })
          output$plotly_corr_plt.pos <- renderPlotly({
            para = plot2_para()
            if(is.null(input$merge_start)){return()}
            if(is.null(object_merge)){return()}

            object_merge %>%
              massqc::massqc_sample_correlation(
                cor_method = para$fig2_cor_method,
                method = para$fig2_method,
                type = para$fig2_type,
                colors = c(para$fig2_min,para$fig2_mid,para$fig2_max),
                outline.color = para$fig2_outlier.color,
                order_by = para$fig2_order_by
              ) %>% plotly::ggplotly()
          })
          ##> status
          output$obj_merge = renderPrint({
            print(object_merge)
          })
          output$process_obj = renderPrint({
            print(object_merge %>% extract_process_info())
          })
          p2_af_filter$object_merge = object_merge
        }
      )
      observeEvent(
        input$para_report,
        {
          dir.create(path = paste0(prj_init$wd,"/Result/Data_Merge/"), showWarnings = FALSE,recursive = T)
          object_merge = p2_af_filter$object_merge
          save(object_merge,file = paste0(prj_init$wd,"/Result/Data_Merge/object_merge.rda"))
          report_parameters(object = object_merge, path = paste0(prj_init$wd,"/Result/Data_Merge/"))
        }
      )
    })
  }
