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


dam_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Differential Accumulated Metabolites (DAM)',
    icon = bs_icon("basket"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Compare group",
          icon = bsicons::bs_icon("stars"),
          actionButton(
            inputId = ns("activarte_compare_group"),
            label = tooltip(
              trigger = list(
                "Wake up object",
                bsicons::bs_icon("info-circle")
              ),
              "We need to click this button to wake up the massdataset object, then find the sample attributes from the sample information, then group the samples according to one of the attributes, and then perform DAM analysis"
            ),
            icon = icon("bolt")
          ),

          selectInput(
            inputId = ns("col_index"),
            label = tooltip(
              trigger = list(
                "Column index",
                bsicons::bs_icon("info-circle")
              ),
              "Which sample attribute is used as the compare grouping basis"
            ),
            choices = c("class", "group"),
            selected = "group",multiple = FALSE
          ),
          selectInput(
            inputId = ns("left"),
            label = "First group",
            choices = "Case",
            selected = "case",multiple = FALSE
          ),
          selectInput(
            inputId = ns("right"),
            label = "Second group",
            choices = "Control",
            selected = "sample_id",multiple = FALSE
          ),
          textInput(
            inputId = ns("log2fc"),
            label = tooltip(
              trigger = list(
                "Log2(FoldChange)",
                bsicons::bs_icon("info-circle")
              ),
              "The cutoff based on log2(FoldChage)"
            ),value = 1
          ),
          sliderInput(
            inputId = ns("pvalue"),
            label = tooltip(
              trigger = list(
                "P-value",
                bsicons::bs_icon("info-circle")
              ),
              "The cutoff based on P-value"
            ),min = 0,max = 1,step = 0.01,value = 0.05
          ),
          sliderInput(
            inputId = ns("FDR"),
            label = tooltip(
              trigger = list(
                "FDR",
                bsicons::bs_icon("info-circle")
              ),
              "The cutoff based on Adjuested P-value"
            ),min = 0,max = 1,step = 0.01,value = 1
          ),
          textInput(
            inputId = ns("VIP"),
            label = tooltip(
              trigger = list(
                "VIP",
                bsicons::bs_icon("info-circle")
              ),
              "The cutoff based on VIP of PLS-DA or OPLS-DA"
            ),value = 1
          ),
          actionButton(inputId = ns("DAM_start"),label = "Start",icon = icon("play"))
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
            title = "volcano",
            sidebar =
              accordion(
                open = FALSE,
                accordion_panel(
                  title = 'Parameters',
                  textInput(
                    ns('fig2_xlim_min'),"x-axis range min", -4
                  ),
                  textInput(
                    ns('fig2_xlim_max'),"x-axis range max", 4
                  ),
                  textInput(
                    ns('fig2_fc_column_name'),"fc_column_name","fc"
                  ),
                  selectInput(
                    ns('fig2_log2_fc'),"log2_fc",choices = c("TRUE","FALSE"),"FALSE"
                  ),
                  textInput(
                    ns('fig2_fc_up_cutoff'),"fold change up cutoff",2
                  ),
                  textInput(
                    ns('fig2_fc_down_cutoff'),"fold change down cutoff",0.5
                  ),
                  textInput(
                    ns('fig2_p_value_column_name'),"p_value_column_name","p_value_adjust"
                  ),
                  textInput(
                    ns('fig2_labs_x'),"labs_x","log2(Fold change)"
                  ),
                  textInput(
                    ns('fig2_labs_y'),"labs_y","-log(p-adjust, 10)"
                  ),
                  textInput(
                    ns('fig2_line_color'),"line_color","red"
                  ),
                  colourpicker::colourInput(
                    inputId = ns('fig2_up_color'),label = 'up_color',value = '#EE0000FF'
                  ),
                  colourpicker::colourInput(
                    inputId = ns('fig2_down_color'),label = 'down_color',value = '#3B4992FF'
                  ),
                  colourpicker::colourInput(
                    inputId = ns('fig2_no_color'),label = 'no_color',value = '#808180FF'
                  ),
                  sliderInput(
                    inputId = ns('fig2_point_size'),label = 'point_size', min = 0,max = 10,value = 2,step = 1
                  ),
                  sliderInput(
                    inputId = ns('fig2_point_alpha'),label = 'point_alpha', min = 0,max = 1,value = 0.5,step = 0.1
                  ),
                  textInput(
                    inputId = ns('fig2_point_size_scale'),label = 'point_size_scale', "log10_p"
                  ),
                  textInput(
                    inputId = ns('fig2_line_type'),label = 'line_type', 1
                  ),
                  selectInput(
                    inputId = ns('fig2_text_for'),label = 'text_for', c("marker", "UP", "DOWM"),c("marker", "UP", "DOWM"),multiple = T
                  ),
                  textInput(
                    inputId = ns('fig2_text_from'),label = 'text_from', "variable_id"
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
              "Volcano Plot",
              card_title("Volcano Plot"),
              uiOutput(ns("fig2_corr_plt.pos"),fill = T)
            )
          )
        ),
        navset_card_tab(
          title = "DAM analysis result",
          height = 400,
          full_screen = TRUE,
          nav_panel("Result overview", dataTableOutput(ns("All_compounds"))),
          nav_panel("DAMs", dataTableOutput(ns("DAMs_tbl")))
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
#' @importFrom massstat mutate_fc volcano_plot mutate_p_value
#' @param id module of server
#' @param volumes shinyFiles volumes
#' @param prj_init use project init variables.
#' @param data_import_rv reactivevalues mass_dataset export
#' @param data_clean_rv reactivevalues p2 dataclean
#' @param p2_af_filter reactivevalues anno filtering
#' @param data_export_rv reactivevalues mass_dataset export
#' @noRd


dam_server <-
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

      dam_para = reactive({
        list(
          col_index = input$col_index %>% as.character(),
          left = input$left %>% as.character(),
          right = input$right %>% as.character(),
          log2fc = input$log2fc %>% as.numeric(),
          pvalue = input$pvalue %>% as.numeric(),
          FDR = input$FDR %>% as.numeric(),
          VIP = input$VIP %>% as.numeric()
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
          fig2_fc_column_name = input$fig2_fc_column_name %>% as.character(),
          fig2_log2_fc = input$fig2_log2_fc %>% as.logical(),
          fig2_p_value_column_name = input$fig2_p_value_column_name %>% as.character(),
          fig2_labs_x = input$fig2_labs_x %>% as.character(),
          fig2_labs_y = input$fig2_labs_y %>% as.character(),
          fig2_line_color = input$fig2_line_color %>% as.character(),
          fig2_up_color = input$fig2_up_color %>% as.character(),
          fig2_down_color = input$fig2_down_color %>% as.character(),
          fig2_no_color = input$fig2_no_color %>% as.character(),
          fig2_point_size = input$fig2_point_size %>% as.numeric(),
          fig2_point_alpha = input$fig2_point_alpha %>% as.numeric(),
          fig2_point_size_scale = input$fig2_point_size_scale %>% as.character(),
          fig2_line_type = input$fig2_line_type %>% as.numeric(),
          fig2_text_for = input$fig2_text_for %>% as.character(),
          fig2_text_from = input$fig2_text_from %>% as.character()
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

      ## activate object ========================
      observeEvent(
        input$activarte_compare_group,
        {
          if(prj_init$steps == "DAM and rest") {
            if (!is.null(prj_init$object_negative.init)) {
              p2_af_filter$object_merge = prj_init$object_negative.init
            } else if (!is.null(prj_init$object_positive.init)) {
              p2_af_filter$object_merge = prj_init$object_positive.init
            } else {
              return()
            }
          } else {
            if (!is.null(data_clean_rv$object_neg.af)) {
              p2_af_filter$object_merge = data_clean_rv$object_neg.af
            } else if (!is.null(data_clean_rv$object_pos.af)) {
              p2_af_filter$object_merge = data_clean_rv$object_pos.af
            } else {
              return()
            }
          }
          p2_af_filter$sample_info <- p2_af_filter$object_merge %>% extract_sample_info()

          # Get column indexes, excluding first three columns
          p2_af_filter$temp_col_idx = colnames(p2_af_filter$sample_info)[-c(1:3)]

          # Only update the select inputs if p2_af_filter$temp_col_idx is not empty
          if (length(p2_af_filter$temp_col_idx) > 0) {
            updateSelectInput(session, "col_index", choices = p2_af_filter$temp_col_idx, selected = "group")
            updateSelectInput(session, "fig1_color_by", choices = p2_af_filter$temp_col_idx, selected = "group")
            updateSelectInput(session, "fig1_color_by_3d", choices = p2_af_filter$temp_col_idx, selected = "group")
          }

          # Fetch dam_para and update group_id
          dam_para = dam_para()
          if (!is.null(dam_para)) {
            p2_af_filter$group_id = dam_para$col_index

            # Ensure group_id is valid
            if (!is.null(p2_af_filter$group_id)) {
              p2_af_filter$sample_groups = p2_af_filter$sample_info %>%
                pull(!!p2_af_filter$group_id)

              # Update select inputs for groups
              if (!is.null(p2_af_filter$sample_groups)) {
                updateSelectInput(session, "left", choices = p2_af_filter$sample_groups)
                updateSelectInput(session, "right", choices = p2_af_filter$sample_groups)
              }
            }
          }

        }
      )

      observeEvent(
        input$DAM_start,
        {
          if(is.null(p2_af_filter$object_merge)){return()}
          if(is.null(p2_af_filter$temp_col_idx)){return()}
          ##> import parameters
          dam_para = dam_para()

          ## Calculate the fold changes.

          control_sample_id =
            p2_af_filter$object_merge %>%
            extract_sample_info() %>%
            dplyr::rename("tags" = dam_para$col_index) %>%
            dplyr::filter(tags == dam_para$left) %>%
            dplyr::pull(sample_id)
          print(control_sample_id)

          case_sample_id =
            p2_af_filter$object_merge %>%
            extract_sample_info() %>%
            dplyr::rename("tags" = dam_para$col_index) %>%
            dplyr::filter(tags == dam_para$right) %>%
            dplyr::pull(sample_id)
          print(case_sample_id)
          object <-
            p2_af_filter$object_merge %>%
            activate_mass_dataset('sample_info') %>%
            dplyr::filter(sample_id %in% c(control_sample_id,case_sample_id))

          object <-
            mutate_fc(object = object,
                      control_sample_id = control_sample_id,
                      case_sample_id = case_sample_id,
                      mean_median = "mean")
          object <-
            mutate_p_value(
              object = object,
              control_sample_id = control_sample_id,
              case_sample_id = case_sample_id,
              method = "t.test",
              p_adjust_methods = "BH"
            )
          object %>% extract_variable_info()
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
            if(is.null(input$DAM_start)){return()}
            if(is.null(object)){return()}
            if(isTRUE(para$fig1_scale)) {
              temp_obj <- object %>% +1 %>% log(2) %>% scale()
            } else {
              temp_obj <- object %>% +1 %>% log(2)
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
            if(is.null(input$DAM_start)){return()}
            if(is.null(object)){return()}
            if(isTRUE(para$fig1_scale_3d)) {
              temp_obj <- object %>% +1 %>% log(2) %>% scale()
            } else {
              temp_obj <- object %>% +1 %>% log(2)
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
            if(is.null(input$DAM_start)){return()}
            if(is.null(object)){return()}
            object %>%
              massstat::volcano_plot(
                fc_column_name = para$fig2_fc_column_name,
                log2_fc = para$fig2_log2_fc,
                p_value_column_name = para$fig2_p_value_column_name,
                labs_x = para$fig2_labs_x,
                labs_y = para$fig2_labs_y,
                # fc_up_cutoff = dam_para$log2fc,
                # fc_down_cutoff = -dam_para$log2fc,
                p_value_cutoff = dam_para$pvalue,
                line_color = para$fig2_line_color,
                up_color = para$fig2_up_color,
                down_color = para$fig2_down_color,
                no_color = para$fig2_no_color,
                point_size = para$fig2_point_size,
                point_alpha = para$fig2_point_alpha,
                point_size_scale = para$fig2_point_size_scale,
                line_type = para$fig2_line_type,
                add_text = FALSE,
                text_for = para$fig2_text_for,
                text_from = para$fig2_text_from
              )
          })
          output$plotly_corr_plt.pos <- renderPlotly({
            para = plot2_para()
            if(is.null(input$DAM_start)){return()}
            if(is.null(object)){return()}

            object %>%
              massstat::volcano_plot(
                fc_column_name = para$fig2_fc_column_name,
                log2_fc = para$fig2_log2_fc,
                p_value_column_name = para$fig2_p_value_column_name,
                labs_x = para$fig2_labs_x,
                labs_y = para$fig2_labs_y,
                # fc_up_cutoff = dam_para$log2fc,
                # fc_down_cutoff = -(dam_para$log2fc),
                p_value_cutoff = dam_para$pvalue,
                line_color = para$fig2_line_color,
                up_color = para$fig2_up_color,
                down_color = para$fig2_down_color,
                no_color = para$fig2_no_color,
                point_size = para$fig2_point_size,
                point_alpha = para$fig2_point_alpha,
                point_size_scale = para$fig2_point_size_scale,
                line_type = para$fig2_line_type,
                add_text = FALSE,
                text_for = para$fig2_text_for,
                text_from = para$fig2_text_from
              )%>% plotly::ggplotly()
          })
        }
      )
    })
  }
