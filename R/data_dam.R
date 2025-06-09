#' DAM analysis
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
          fileInput(
            inputId = ns("saved_obj"),
            label = tooltip(
              trigger = list(
                "File upload (option)",
                bsicons::bs_icon("info-circle")
              ),
              "For resuming tasks, please upload mass_dataset/09.object_clean.rda in the working directory"
            ),
            multiple = FALSE,
            buttonLabel = "Browse...",
            placeholder = "No file selected",
            accept = ".rda"
          ),
          radioButtons(
            inputId = ns("anno_level"),
            label = "Remove unknown metabolites",
            choices = c("TRUE","FALSE"),selected = TRUE
          ),
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
          actionButton(inputId = ns("DAM_start"),label = "Start",icon = icon("play"))
          )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1/2,
          height = 500,
          navset_card_tab(
            height = 500,
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
            height = 500,
            full_screen = TRUE,
            title = "volcano",
            sidebar =
              accordion(
                open = FALSE,
                accordion_panel(
                  title = 'Parameters',
                  textInput(
                    ns('fig2_xlim_min'),"x-axis range min", -9999
                  ),
                  textInput(
                    ns('fig2_xlim_max'),"x-axis range max", 9999
                  ),
                  textInput(
                    ns('fig2_fc_column_name'),"fc_column_name","fc"
                  ),
                  selectInput(
                    ns('fig2_log2_fc'),"log2_fc",choices = c("TRUE","FALSE"),"TRUE"
                  ),
                  radioButtons(
                    inputId = ns('fig2_p_value_column_name'),
                    label = "p_value_column_name",
                    choices = c("p_value_adjust","p_value"),
                    selected = "p_value"
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
                    inputId = ns('fig2_add_text'),label = 'add_text', c("TRUE", "FALSE"),"TRUE",multiple = F
                  ),
                  selectInput(
                    inputId = ns('fig2_text_for'),label = 'text_for', c("marker", "UP", "DOWM"),c("marker", "UP", "DOWM"),multiple = T
                  ),
                  radioButtons(
                    inputId = ns('fig2_text_from'),label = 'text_from',
                    choices = c("variable_id","Compound.name"),selected = "Compound.name"
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
          height = 500,
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
#' @param p3_dam reactivevalues p3 DAM analysis
#' @noRd


dam_server <-
  function(id,
           volumes,
           prj_init,
           data_import_rv,
           data_clean_rv,p3_dam
           ) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # parameters --------------------------------------------------------------

      dam_para = reactive({
        list(
          anno_level = input$anno_level %>% as.logical(),
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
          fig2_xlim_min = input$fig2_xlim_min %>% as.numeric(),
          fig2_xlim_max = input$fig2_xlim_max %>% as.numeric(),
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
          fig2_add_text = input$fig2_add_text %>% as.logical(),
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
          if (!is.null(input$saved_obj)) {
            p3_dam$object_final <- load_rdata(input$saved_obj$datapath)
          } else {
            p3_dam$object_final <- data_clean_rv$object_final
          }

          p3_dam$sample_info <- p3_dam$object_final %>% extract_sample_info()

          # Get column indexes, excluding first three columns
          p3_dam$temp_col_idx = colnames(p3_dam$sample_info)[-c(1:3)]

          # Only update the select inputs if p3_dam$temp_col_idx is not empty
          if (length(p3_dam$temp_col_idx) > 0) {
            updateSelectInput(session, "col_index", choices = p3_dam$temp_col_idx, selected = "group")
            updateSelectInput(session, "fig1_color_by", choices = p3_dam$temp_col_idx, selected = "group")
            updateSelectInput(session, "fig1_color_by_3d", choices = p3_dam$temp_col_idx, selected = "group")
          }

          # Fetch dam_para and update group_id
          dam_para = dam_para()
          if (!is.null(dam_para)) {
            p3_dam$group_id = dam_para$col_index

            # Ensure group_id is valid
            if (!is.null(p3_dam$group_id)) {
              p3_dam$sample_groups = p3_dam$sample_info %>%
                pull(!!p3_dam$group_id)

              # Update select inputs for groups
              if (!is.null(p3_dam$sample_groups)) {
                updateSelectInput(session, "left", choices = p3_dam$sample_groups)
                updateSelectInput(session, "right", choices = p3_dam$sample_groups)
              }
            }
          }

        }
      )

      observeEvent(
        input$DAM_start,
        {
          if(is.null(p3_dam$object_final)){return()}
          if(is.null(p3_dam$temp_col_idx)){return()}
          ##> import parameters
          dam_para = dam_para()
          if(isTRUE(dam_para$anno_level)) {
            p3_dam$object_final =
              p3_dam$object_final %>%
              activate_mass_dataset(what = "annotation_table") %>%
              filter(!is.na(Level)) %>%
              filter(Level == 1 | Level == 2) %>%
              group_by(Compound.name) %>%
              dplyr::filter(Level == min(Level)) %>%
              dplyr::filter(SS == max(SS)) %>%
              dplyr::slice_head(n = 1)
          }

          ## Calculate the fold changes.

          control_sample_id =
            p3_dam$object_final %>%
            extract_sample_info() %>%
            dplyr::rename("tags" = dam_para$col_index) %>%
            dplyr::filter(tags == dam_para$right) %>%
            dplyr::pull(sample_id)

          case_sample_id =
            p3_dam$object_final %>%
            extract_sample_info() %>%
            dplyr::rename("tags" = dam_para$col_index) %>%
            dplyr::filter(tags == dam_para$left) %>%
            dplyr::pull(sample_id)

          problems <- character()
          if (length(control_sample_id) < 3) {
            problems <- c(problems, paste(dam_para$right," sample number: ", length(control_sample_id), " need at least 3 samples"))
          }
          if (length(case_sample_id) < 3) {
            problems <- c(problems, paste(dam_para$left, " sample number: ",length(case_sample_id), "need at least 3 samples"))
          }
          if (length(problems) > 0) {
            shinyalert(title = "Error", text = paste(problems, collapse = ". "), type = "error")
            return()
          }

          object <-
            p3_dam$object_final %>%
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

          # DAM analysis ------------------------------------------------------------
          DAM_tbl =
            object %>% extract_variable_info() %>%
            dplyr::select(variable_id,Compound.name,fc,p_value,p_value_adjust,SS,Total.score,Database,Level) %>%
            group_by(variable_id) %>% slice_head(n = 1) %>%
            dplyr::arrange(p_value,fc,Compound.name)
          DAM_tbl_filtered = DAM_tbl %>%
            dplyr::filter(abs(log2(fc)) >= dam_para$log2fc) %>%
            dplyr::filter(p_value <= dam_para$pvalue) %>%
            dplyr::filter(p_value_adjust <= dam_para$FDR)
          data_clean_rv$object_dam <-
            object %>%
            activate_mass_dataset('variable_info') %>%
            dplyr::group_by(variable_id) %>% slice_head(n = 1) %>%
            dplyr::filter(abs(log2(fc)) >= dam_para$log2fc) %>%
            dplyr::filter(p_value <= dam_para$pvalue) %>%
            dplyr::filter(p_value_adjust <= dam_para$FDR)

          output$All_compounds = renderDataTable_formated(

            actions = input$DAM_start,
            condition1 = DAM_tbl,filename.a = paste0(dam_para$left,"vs",dam_para$right,"_summary.xls"),
            tbl = DAM_tbl
          )

          output$DAMs_tbl = renderDataTable_formated(
            actions = input$DAM_start,
            condition1 = DAM_tbl_filtered,filename.a = paste0(dam_para$left,"vs",dam_para$right,"_summary.xls"),
            tbl = DAM_tbl_filtered
          )

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
          output$fig1_download <- downloadHandler(
            filename = function() {
              dp <- download_para()
              format <- dp$fig1_format
              dam_p <- dam_para()
              paste0(dam_p$left, '_vs_', dam_p$right, '_PCA_plot.', format)
            },
            content = function(file) {
              if (is.null(p3_dam$object_final)) {
                stop("No data available for plotting. Please run the analysis first.")
              }
              dp <- download_para()
              width <- dp$fig1_width
              height <- dp$fig1_height
              format <- dp$fig1_format

              if (format == "png") {
                png(file, width = width, height = height, units = "in", res = 300)
              } else if (format == "jpg") {
                jpeg(file, width = width, height = height, units = "in", res = 300, quality = 100)
              } else if (format == "pdf") {
                pdf(file, width = width, height = height)
              } else if (format == "tiff") {
                tiff(file, width = width, height = height, units = "in", res = 300)
              }

              para <- plot1_para()
              if (isTRUE(para$fig1_scale)) {
                temp_obj <- p3_dam$object_final %>% +1 %>% log(2) %>% scale()
              } else {
                temp_obj <- p3_dam$object_final %>% +1 %>% log(2)
              }
              p <- temp_obj %>%
                massqc::massqc_pca(
                  color_by = para$fig1_color_by,
                  point_alpha = para$fig1_point_alpha,
                  frame = para$fig1_frame,
                  line = para$fig1_line
                )

              print(p)
              dev.off()
            }
          )
          ###> fig2 Voc =============

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
            dam_para = dam_para()
            if(is.null(input$DAM_start)){return()}
            if(is.null(object)){return()}
            temp_voc_p =
            object %>%
              massstat::volcano_plot(
                fc_column_name = para$fig2_fc_column_name,
                log2_fc = para$fig2_log2_fc,
                p_value_column_name = para$fig2_p_value_column_name,
                labs_x = para$fig2_labs_x,
                labs_y = para$fig2_labs_y,
                fc_up_cutoff = 2^(dam_para$log2fc),
                fc_down_cutoff = 2^-(dam_para$log2fc),
                p_value_cutoff = dam_para$pvalue,
                line_color = para$fig2_line_color,
                up_color = para$fig2_up_color,
                down_color = para$fig2_down_color,
                no_color = para$fig2_no_color,
                point_size = para$fig2_point_size,
                point_alpha = para$fig2_point_alpha,
                point_size_scale = para$fig2_point_size_scale,
                line_type = para$fig2_line_type,
                add_text = para$fig2_add_text,
                text_for = para$fig2_text_for,
                text_from = para$fig2_text_from
              )
            if(para$fig2_xlim_max < 1000 & para$fig2_xlim_min > -1000) {
              p = temp_voc_p + xlim(para$fig2_xlim_min,para$fig2_xlim_max)
            } else {
              p = temp_voc_p
            }
            p
          })
          output$plotly_corr_plt.pos <- renderPlotly({
            para = plot2_para()
            dam_para = dam_para()
            if(is.null(input$DAM_start)){return()}
            if(is.null(object)){return()}
            temp_voc_p =
              object %>%
              massstat::volcano_plot(
                fc_column_name = para$fig2_fc_column_name,
                log2_fc = para$fig2_log2_fc,
                p_value_column_name = para$fig2_p_value_column_name,
                labs_x = para$fig2_labs_x,
                labs_y = para$fig2_labs_y,
                fc_up_cutoff = 2^(dam_para$log2fc),
                fc_down_cutoff = 2^-(dam_para$log2fc),
                p_value_cutoff = dam_para$pvalue,
                line_color = para$fig2_line_color,
                up_color = para$fig2_up_color,
                down_color = para$fig2_down_color,
                no_color = para$fig2_no_color,
                point_size = para$fig2_point_size,
                point_alpha = para$fig2_point_alpha,
                point_size_scale = para$fig2_point_size_scale,
                line_type = para$fig2_line_type,
                add_text = para$fig2_add_text,
                text_for = para$fig2_text_for,
                text_from = para$fig2_text_from
              )
            if(para$fig2_xlim_max < 1000 & para$fig2_xlim_min > -1000) {
              p = temp_voc_p + xlim(para$fig2_xlim_min,para$fig2_xlim_max)
            } else {
              p = temp_voc_p
            }
            p %>% plotly::ggplotly()
          })
          output$fig2_download <- downloadHandler(
            filename = function() {
              dp <- download_para()
              format <- dp$fig2_format
              dam_p <- dam_para()
              paste0(dam_p$left, '_vs_', dam_p$right, '_Volcano_plot.', format)
            },
            content = function(file) {
              if (is.null(p3_dam$object_final)) {
                stop("No data available for plotting. Please run the analysis first.")
              }
              dp <- download_para()
              width <- dp$fig2_width
              height <- dp$fig2_height
              format <- dp$fig2_format

              if (format == "png") {
                png(file, width = width, height = height, units = "in", res = 300)
              } else if (format == "jpg") {
                jpeg(file, width = width, height = height, units = "in", res = 300, quality = 100)
              } else if (format == "pdf") {
                pdf(file, width = width, height = height)
              } else if (format == "tiff") {
                tiff(file, width = width, height = height, units = "in", res = 300)
              }
              para = plot2_para()
              dam_para = dam_para()
              if(is.null(input$DAM_start)){return()}
              if(is.null(object)){return()}
              temp_voc_p =
                object %>%
                massstat::volcano_plot(
                  fc_column_name = para$fig2_fc_column_name,
                  log2_fc = para$fig2_log2_fc,
                  p_value_column_name = para$fig2_p_value_column_name,
                  labs_x = para$fig2_labs_x,
                  labs_y = para$fig2_labs_y,
                  fc_up_cutoff = 2^(dam_para$log2fc),
                  fc_down_cutoff = 2^-(dam_para$log2fc),
                  p_value_cutoff = dam_para$pvalue,
                  line_color = para$fig2_line_color,
                  up_color = para$fig2_up_color,
                  down_color = para$fig2_down_color,
                  no_color = para$fig2_no_color,
                  point_size = para$fig2_point_size,
                  point_alpha = para$fig2_point_alpha,
                  point_size_scale = para$fig2_point_size_scale,
                  line_type = para$fig2_line_type,
                  add_text = para$fig2_add_text,
                  text_for = para$fig2_text_for,
                  text_from = para$fig2_text_from
                )
              if(para$fig2_xlim_max < 1000 & para$fig2_xlim_min > -1000) {
                p = temp_voc_p + xlim(para$fig2_xlim_min,para$fig2_xlim_max)
              } else {
                p = temp_voc_p
              }
              print(p)
              dev.off()
            }
          )

        }
      )
    })
  }
