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
    icon = bs_icon("justify"),
    layout_sidebar(
      sidebar = accordion(
        open = c("Normalization method","Action buttoms"),
        accordion_panel(
          title = "Normalization method",
          icon = bsicons::bs_icon("gear"),
          selectInput(
            inputId = ns('method'),
            label = "method ",multiple = F,
            choices = c("svr", "total", "median", "mean", "pqn", "loess","ppca"),
            selected = 'svr'
          )
          ),
        accordion_panel(
          title = "Normalization parameters",
          icon = bsicons::bs_icon("gear"),
          radioButtons(
            inputId = ns('keep_scale'),
            label = "keep_scale",choices = c("TRUE","FALSE"),selected = "TRUE"
          ),
          radioButtons(
            inputId = ns('optimization'),
            label = "optimization",choices = c("TRUE","FALSE"),selected = "TRUE"
          ),
          radioButtons(
            inputId = ns('pqn_reference'),
            label = "pqn_reference",choices = c("median","mean"),selected = "median"
          ),
          textInput(
            inputId = ns('begin'),
            label = "begin",
            value = 0.5
          ),
          textInput(
            inputId = ns('end'),
            label = "end",
            value = 1
          ),
          textInput(
            inputId = ns('step'),
            label = "step",
            value = 0.2
          ),
          textInput(
            inputId = ns('multiple'),
            label = "multiple",
            value = 1
          ),
          textInput(
            inputId = ns('threads'),
            label = "threads",
            value = 1
          ),
        ),
        accordion_panel(
          title = "Action buttoms",
          icon = bsicons::bs_icon("hand-index"),
          actionButton(
            inputId = ns("norm_start"),
            label = "Start analysis",icon = icon("play")
          ),
          br(),
          actionButton(
            inputId = ns("norm_vis"),
            label = "Show plot",icon = icon("image")
          )
        )
        ),
      page_fluid(
        nav_panel(title = "normalization",
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
                        "Positive",
                        uiOutput(ns("pca_before_pos"))
                      ),
                      nav_panel(
                        "Negative",
                        uiOutput(ns("pca_before_neg"))
                      )
                    ),
                    navset_card_tab(
                      title = "PCA plot after normalization",
                      height = 350,
                      full_screen = TRUE,
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
                        uiOutput(ns("pca_after_pos"))
                      ),
                      nav_panel(
                        "Negative",
                        uiOutput(ns("pca_after_neg"))
                      )
                    ),
                  ),

                  layout_column_wrap(
                    width = 1/2,
                    height = 350,
                    navset_card_tab(
                      height = 350,
                      full_screen = TRUE,
                      title = "RSD distribution before normalization",
                      sidebar = accordion(
                        open = 'closed',
                        accordion_panel(
                          title = 'Parameter',
                          sliderInput(
                            inputId = ns('fig3_rsd_cutoff'),label = 'rsd cutoff',min = 0,max = 100,step = 1,value = 30
                          ),
                          colourpicker::colourInput(inputId = ns("fig3_color_rsd"),
                                                    label = "color",
                                                    value = "red"),
                          materialSwitch(inputId = ns("fig3_data_clean_plt_format"),label = "Interactive plot", status = "primary"),
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
                        card_title("Cumulative RSD in QC in positive model"),
                        uiOutput(ns("rsd_before.pos"),fill = T)

                      ),
                      nav_panel(
                        "Negative",
                        card_title("Cumulative RSD in QC in negative model"),
                        uiOutput(ns("rsd_before.neg"),fill = T)

                      )
                    ),

                    navset_card_tab(
                      height = 350,
                      full_screen = TRUE,
                      title = "RSD distribution aftre normalization",
                      sidebar = accordion(
                        open = 'closed',
                        accordion_panel(
                          title = 'Parameter',
                          sliderInput(
                            inputId = ns('fig4_rsd_cutoff'),label = 'rsd cutoff',min = 0,max = 100,step = 1,value = 30
                          ),
                          colourpicker::colourInput(inputId = ns("fig4_color_rsd"),
                                                    label = "color",
                                                    value = "red"),
                          materialSwitch(inputId = ns("fig4_data_clean_plt_format"),label = "Interactive plot", status = "primary"),
                        ),
                        accordion_panel(
                          title = 'Download',
                          icon = bs_icon('download'),
                          textInput(
                            inputId = ns("fig4height"),label = "Height",value = 7
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
                        card_title("Cumulative RSD in QC in positive model"),
                        uiOutput(ns("rsd_after.pos"),fill = T)

                      ),
                      nav_panel(
                        "Negative",
                        card_title("Cumulative RSD in QC in negative model"),
                        uiOutput(ns("rsd_after.neg"),fill = T)

                      )
                    )),

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
#' @importFrom masscleaner normalize_data
#' @importFrom plotly renderPlotly plotlyOutput
#' @param id module of server
#' @param volumes shinyFiles volumes
#' @param prj_init use project init variables.
#' @param data_import_rv reactivevalues mass_dataset export
#' @param data_clean_rv reactivevalues p2 dataclean
#' @param data_export_rv reactivevalues mass_dataset export
#' @noRd


data_normalize_server <- function(id,volumes,prj_init,data_import_rv,data_clean_rv,data_export_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    p2_norm <- reactiveValues(data = NULL)

    # Utility functions ----
    check_ion_modes <- function(data_rv, prj) {
      list(
        has_pos = !is.null(data_rv$object_pos_impute) || !is.null(prj$object_positive.init),
        has_neg = !is.null(data_rv$object_neg_impute) || !is.null(prj$object_negative.init)
      )
    }


    analy_para = reactive({
      list(
        method = input$method %>% as.character(),
        keep_scale = input$keep_scale %>% as.logical(),
        optimization = input$optimization %>% as.logical(),
        pqn_reference = input$pqn_reference %>% as.character(),
        begin = input$begin %>% as.numeric(),
        end = input$end %>% as.numeric(),
        step = input$step %>% as.numeric(),
        multiple = input$multiple %>% as.numeric(),
        threads = input$threads %>% as.numeric()
      )
    })


    ###> plot parameters =========
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

    plot3_para = reactive({
      list(
        fig3_rsd_cutoff = input$fig3_rsd_cutoff %>% as.numeric(),
        fig3_color = input$fig3_color_rsd %>% as.character()
      )
    })

    plot4_para = reactive({
      list(
        fig4_rsd_cutoff = input$fig4_rsd_cutoff %>% as.numeric(),
        fig4_color = input$fig4_color_rsd %>% as.character()
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
        fig2_format = as.character(input$fig2_format),
        ##> fig3
        fig3_width = as.numeric(input$fig3_width),
        fig3_height = as.numeric(input$fig3_height),
        fig3_format = as.character(input$fig3_format),
        ##> fig4
        fig4_width = as.numeric(input$fig4_width),
        fig4_height = as.numeric(input$fig4_height),
        fig4_format = as.character(input$fig4_format)

      )
    })

    # Core processing function ----
    perform_normalization <- function(object, para) {
      tryCatch({
        res <- object %>%
          normalize_data(
            method = para$method,
            keep_scale = para$keep_scale,
            optimization = para$optimization,
            pqn_reference = para$pqn_reference,
            begin = para$begin,
            end = para$end,
            step = para$step,
            multiple = para$multiple,
            threads = para$threads
          )

        if (res %>% extract_sample_info() %>% pull(batch) %>% unique() %>% length() > 1) {
          res <- integrate_data(res, method = "qc_mean")
        }

        res
      }, error = function(e) {
        shinyalert("Normalization Error", paste("Error details:", e$message), type = "error")
        NULL
      })
    }

    ### Dynamic UI Updates ###
    observe({
      tryCatch({
        sources <- list(
          prj_init$object_negative.init,
          prj_init$object_positive.init,
          data_clean_rv$object_pos_impute,
          data_clean_rv$object_neg_impute
        )

        valid_source <- Find(Negate(is.null), sources)
        if(!is.null(valid_source)) {
          col_names <- colnames(valid_source@sample_info)
          sample_ids <- valid_source@sample_info$sample_id
          updateSelectInput(session, "fig2_color_by", choices = col_names, selected = "group")
          updateSelectInput(session, "fig2_color_by_3d", choices = col_names, selected = "group")
          updateSelectInput(session, "fig1_color_by", choices = col_names, selected = "group")
          updateSelectInput(session, "fig1_color_by_3d", choices = col_names, selected = "group")
        }
      }, error = function(e) {
        message("UI update error: ", e$message)
      })
    })

    observeEvent(
      input$norm_start,
      {

        # check data
        modes <- check_ion_modes(data_clean_rv, prj_init)


        if (!modes$has_pos && !modes$has_neg) {
          # No data initialized at all
          shinyalert(
            "Data Not Loaded",
            "No positive/negative ion mode data found. Upload data first.",
            type = "error"
          )
          return()
        }
        # Check if data initialization exists
        if(is.null(data_clean_rv$object_pos_impute) && is.null(data_clean_rv$object_neg_impute)){
          if (!is.null(prj_init$object_negative.init) || !is.null(prj_init$object_positive.init)) {
            # Data initialized but current step is invalid
            if (prj_init$steps != "Normalization") {
              shinyalert(
                "Step Error",
                "Invalid workflow sequence detected.\nPlease restart from the 'NORMALIZATION' step.",
                type = "error"
              )
              return()
            }
          }
        }


        # Load data based on processing step
        if(prj_init$steps == "Normalization") {
          if(modes$has_pos) p2_norm$object_pos <- prj_init$object_positive.init
          if(modes$has_neg) p2_norm$object_neg <- prj_init$object_negative.init
        } else {
          p2_norm$object_pos <- data_clean_rv$object_pos_impute
          p2_norm$object_neg <- data_clean_rv$object_neg_impute
        }
        para <- analy_para()
        # QC sample validation check -------------------------------------------------
        qc_check_required <- c("svr", "pqn", "loess")
        # Check positive mode data
        if (!is.null(p2_norm$object_pos) && para$method %in% qc_check_required) {
          qc_samples_pos <- p2_norm$object_pos %>%
            extract_sample_info() %>%
            dplyr::filter(class == "QC") %>%
            nrow()

          if (qc_samples_pos == 0) {
            shinyalert(
              "QC Samples Required",
              paste("Method", input$method, "requires QC samples but none found in positive mode data"),
              type = "error"
            )
            return()
          }
        }
        # Check negative mode data
        if (!is.null(p2_norm$object_neg) && para$method %in% qc_check_required) {
          qc_samples_neg <- p2_norm$object_neg %>%
            extract_sample_info() %>%
            dplyr::filter(class == "QC") %>%
            nrow()

          if (qc_samples_neg == 0) {
            shinyalert(
              "QC Samples Required",
              paste("Method", input$method, "requires QC samples but none found in negative mode data"),
              type = "error"
            )
            return()
          }
        }

        # Core processing
        withProgress(message = "Processing normalization...", value = 0, {
          para <- analy_para()

          if (modes$has_pos) {
            incProgress(0.3, detail = "Processing positive mode")
            p2_norm$object_pos_norm <- perform_normalization(p2_norm$object_pos, para)
          }

          if (modes$has_neg) {
            incProgress(0.3, detail = "Processing negative mode")
            p2_norm$object_neg_norm <- perform_normalization(p2_norm$object_neg, para)
          }
        })


        # Data persistence
        tryCatch({
          data_clean_rv$object_pos_norm <- p2_norm$object_pos_norm
          data_clean_rv$object_neg_norm <- p2_norm$object_neg_norm
          object_pos_norm <- p2_norm$object_pos_norm
          save(
            object_pos_norm,
            file = file.path(prj_init$mass_dataset_dir, "05.object_pos_norm.rda")
          )
          object_neg_norm <- p2_norm$object_neg_norm
          save(
            p2_norm$object_neg_norm,
            file = file.path(prj_init$mass_dataset_dir, "05.object_neg_norm.rda")
          )
        }, error = function(e) {
          shinyalert("Save Error", paste("Failed to save results:", e$message), type = "error")
        })


        output$norm_expdata_pos = renderDataTable_formated(
          actions = input$norm_start,
          condition1 = p2_norm$object_pos_norm,
          filename.a = "3.6.5.Normalization_Acc_Mat_pos",
          tbl = p2_norm$object_pos_norm %>% extract_expression_data() %>% rownames_to_column("variable_id")
        )
        output$norm_expdata_neg = renderDataTable_formated(
          actions = input$norm_start,
          condition1 = p2_norm$object_neg_norm,
          filename.a = "3.6.5.Normalization_Acc_Mat_neg",
          tbl = p2_norm$object_neg_norm %>% extract_expression_data() %>% rownames_to_column("variable_id")
        )

        # show process
        output$obj_impute.pos  = check_massdata_info(
          object = p2_norm$object_pos_norm,
          mode = "positive"
        )

        output$obj_impute.neg  = check_massdata_info(
          object = p2_norm$object_neg_norm,
          mode = "negative"
        )

        shinyalert(
          title = "Normalization Completed",
          text = paste(
            "Successfully processed:",
            ifelse(modes$has_pos, "\n- Positive mode", ""),
            ifelse(modes$has_neg, "\n- Negative mode", ""),
            "Click Show plot buttom in sidebar to perform data visualiz progress"
          ),
          type = "success"
        )

      }
    )

    observeEvent(
      input$norm_vis,
      {
        ##> fig1 pca before ==============

        output$pca_before_pos <- renderUI({
          plot_type <- input$fig1_data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_pca.pos"))
          } else {
            plotOutput(outputId = ns("plot_pca.pos"))
          }
        })
        output$plot_pca.pos <- renderPlot({
          para = plot1_para()
          if(is.null(input$norm_vis)){return()}
          if(is.null(p2_norm$object_pos)){
            print(gg_message_plot("No data in positive ion mode was detected.") %>% plotly::ggplotly())
          } else {
            if(isTRUE(para$fig1_scale)) {
              temp_obj.pos <- p2_norm$object_pos %>% +1 %>% log(2) %>% scale()
            } else {
              temp_obj.pos <- p2_norm$object_pos %>% +1 %>% log(2)
            }

            temp_obj.pos %>%
              massqc::massqc_pca(
                color_by = para$fig1_color_by,
                point_alpha = para$fig1_point_alpha,
                frame = para$fig1_frame,
                line = para$fig1_line
              )
          }

        })
        output$plotly_pca.pos <- renderPlotly({
          para = plot1_para()
          if(is.null(input$norm_vis)){return()}
          if(is.null(p2_norm$object_pos)){
            print(gg_message_plot("No data in positive ion mode was detected.") %>% plotly::ggplotly())
          } else {
          if(isTRUE(para$fig1_scale_3d)) {
            temp_obj.pos <- p2_norm$object_pos %>% +1 %>% log(2) %>% scale()
          } else {
            temp_obj.pos <- p2_norm$object_pos %>% +1 %>% log(2)
          }
          temp_obj.pos %>%
            massqc_pca_3d(
              color_by = para$fig1_color_by_3d,
              x_axis = para$fig1_x_axis,
              y_axis = para$fig1_y_axis,
              z_axis = para$fig1_z_axis
            )
          }
        })
        # negative
        output$pca_before_neg <- renderUI({
          plot_type <- input$fig1_data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_pca.neg"))
          } else {
            plotOutput(outputId = ns("plot_pca.neg"))
          }
        })
        output$plot_pca.neg <- renderPlot({
          para = plot1_para()
          if(is.null(input$norm_vis)){return()}
          if(is.null(p2_norm$object_pos)){
            print(gg_message_plot("No data in negative ion mode was detected.") %>% plotly::ggplotly())
          } else {

          if(isTRUE(para$fig1_scale)) {
            temp_obj.neg <- p2_norm$object_neg %>% +1 %>% log(2) %>% scale()
          } else {
            temp_obj.neg <- p2_norm$object_neg %>% +1 %>% log(2)
          }

          temp_obj.neg %>%
            massqc::massqc_pca(
              color_by = para$fig1_color_by,
              point_alpha = para$fig1_point_alpha,
              frame = para$fig1_frame,
              line = para$fig1_line
            )
          }
        })
        output$plotly_pca.neg <- renderPlotly({
          para = plot1_para()
          if(is.null(input$norm_vis)){return()}
          if(is.null(p2_norm$object_pos)){
            print(gg_message_plot("No data in negative ion mode was detected.") %>% plotly::ggplotly())
          } else {

          if(isTRUE(para$fig1_scale_3d)) {
            temp_obj.neg <- p2_norm$object_neg %>% +1 %>% log(2) %>% scale()
          } else {
            temp_obj.neg <- p2_norm$object_neg %>% +1 %>% log(2)
          }

          temp_obj.neg %>%
            massqc_pca_3d(
              color_by = para$fig1_color_by_3d,
              x_axis = para$fig1_x_axis,
              y_axis = para$fig1_y_axis,
              z_axis = para$fig1_z_axis
            )
          }
        })

        ##>fig2 pca after ======

        output$pca_after_pos <- renderUI({
          plot_type <- input$fig2_data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_pca2.pos"))
          } else {
            plotOutput(outputId = ns("plot_pca2.pos"))
          }
        })
        output$plot_pca2.pos <- renderPlot({
          para = plot2_para()
          if(is.null(input$norm_start)){return()}
          if(is.null(p2_norm$object_pos_norm)){
            print(gg_message_plot("No data in positive ion mode was detected.") %>% plotly::ggplotly())
          } else {
          if(isTRUE(para$fig2_scale)) {
            temp_obj.pos <- p2_norm$object_pos_norm %>% +1 %>% log(2) %>% scale()
          } else {
            temp_obj.pos <- p2_norm$object_pos_norm %>% +1 %>% log(2)
          }

          temp_obj.pos %>%
            massqc::massqc_pca(
              color_by = para$fig2_color_by,
              point_alpha = para$fig2_point_alpha,
              frame = para$fig2_frame,
              line = para$fig2_line
            )
          }
        })
        output$plotly_pca2.pos <- renderPlotly({
          para = plot2_para()
          if(is.null(input$norm_start)){return()}
          if(is.null(p2_norm$object_pos_norm)){
            print(gg_message_plot("No data in positive ion mode was detected.") %>% plotly::ggplotly())
          } else {
          if(isTRUE(para$fig2_scale_3d)) {
            temp_obj.pos <- p2_norm$object_pos_norm %>% +1 %>% log(2) %>% scale()
          } else {
            temp_obj.pos <- p2_norm$object_pos_norm %>% +1 %>% log(2)
          }
          temp_obj.pos %>%
            massqc_pca_3d(
              color_by = para$fig2_color_by_3d,
              x_axis = para$fig2_x_axis,
              y_axis = para$fig2_y_axis,
              z_axis = para$fig2_z_axis
            )
          }
        })
        # negative
        output$pca_after_neg <- renderUI({
          plot_type <- input$fig2_data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_pca2.neg"))
          } else {
            plotOutput(outputId = ns("plot_pca2.neg"))
          }
        })
        output$plot_pca2.neg <- renderPlot({
          para = plot2_para()
          if(is.null(input$norm_start)){return()}
          if(is.null(p2_norm$object_pos_norm)){
            print(gg_message_plot("No data in negative ion mode was detected.") %>% plotly::ggplotly())
          } else {

          if(isTRUE(para$fig2_scale)) {
            temp_obj.neg <- p2_norm$object_neg_norm %>% +1 %>% log(2) %>% scale()
          } else {
            temp_obj.neg <- p2_norm$object_neg_norm %>% +1 %>% log(2)
          }

          temp_obj.neg %>%
            massqc::massqc_pca(
              color_by = para$fig2_color_by,
              point_alpha = para$fig2_point_alpha,
              frame = para$fig2_frame,
              line = para$fig2_line
            )
          }
        })
        output$plotly_pca2.neg <- renderPlotly({
          para = plot2_para()
          if(is.null(input$norm_start)){return()}
          if(is.null(p2_norm$object_pos_norm)){
            print(gg_message_plot("No data in negative ion mode was detected.") %>% plotly::ggplotly())
          } else {

          if(isTRUE(para$fig2_scale_3d)) {
            temp_obj.neg <- p2_norm$object_neg_norm %>% +1 %>% log(2) %>% scale()
          } else {
            temp_obj.neg <- p2_norm$object_neg_norm %>% +1 %>% log(2)
          }

          temp_obj.neg %>%
            massqc_pca_3d(
              color_by = para$fig2_color_by_3d,
              x_axis = para$fig2_x_axis,
              y_axis = para$fig2_y_axis,
              z_axis = para$fig2_z_axis
            )
          }
        })

        ###> fig3 rsd =======

        # positive
        output$rsd_before.pos <- renderUI({
          plot_type <- input$fig3_data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_rsd_plt.pos"))
          } else {
            plotOutput(outputId = ns("plot_rsd_plt.pos"))
          }
        })
        output$plot_rsd_plt.pos <- renderPlot({
          para = plot3_para()
          if(is.null(input$norm_vis)){return()}
          if(is.null(p2_norm$object_pos)){
            print(gg_message_plot("No data in positive ion mode was detected.") %>% plotly::ggplotly())
          } else {
          p2_norm$object_pos %>%
            activate_mass_dataset("sample_info") %>%
            dplyr::filter(class == "QC") %>%
            massqc::massqc_cumulative_rsd_plot(
              rsd_cutoff = para$fig3_rsd_cutoff,
              color = para$fig3_color,
              title = 'All of QC sample'
            )
          }
        })
        output$plotly_rsd_plt.pos <- renderPlotly({
          para = plot3_para()
          if(is.null(input$norm_vis)){return()}
          if(is.null(p2_norm$object_pos)){
            print(gg_message_plot("No data in positive ion mode was detected.") %>% plotly::ggplotly())
          } else {
          p2_norm$object_pos %>%
            activate_mass_dataset("sample_info") %>%
            dplyr::filter(class == "QC")%>%
            massqc::massqc_cumulative_rsd_plot(
              rsd_cutoff = para$fig3_rsd_cutoff,
              color = para$fig3_color,
              title = 'All of QC sample'
            ) %>% plotly::ggplotly()
          }
        })
        # negative
        output$rsd_before_neg <- renderUI({
          plot_type <- input$fig3_data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_rsd_plt.neg"))
          } else {
            plotOutput(outputId = ns("plot_rsd_plt.neg"))
          }
        })
        output$plot_rsd_plt.neg <- renderPlot({
          para = plot3_para()
          if(is.null(input$norm_vis)){return()}
          if(is.null(p2_norm$object_pos)){
            print(gg_message_plot("No data in negative ion mode was detected.") %>% plotly::ggplotly())
          } else {
          p2_norm$object_neg %>%
            activate_mass_dataset("sample_info") %>%
            dplyr::filter(class == "QC")%>%
            massqc::massqc_cumulative_rsd_plot(
              rsd_cutoff = para$fig3_rsd_cutoff,
              color = para$fig3_color,
              title = 'All of QC sample'
            )
          }
        })
        output$plotly_rsd_plt.neg <- renderPlotly({
          para = plot3_para()
          if(is.null(input$norm_vis)){return()}
          if(is.null(p2_norm$object_neg)){
            print(gg_message_plot("No data in negative ion mode was detected.") %>% plotly::ggplotly())
          } else {
          p2_norm$object_neg %>%
            activate_mass_dataset("sample_info") %>%
            dplyr::filter(class == "QC")%>%
            massqc::massqc_cumulative_rsd_plot(
              rsd_cutoff = para$fig3_rsd_cutoff,
              color = para$fig3_color,
              title = 'All of QC sample'
            ) %>% plotly::ggplotly()
          }
        })

        ###> fig4 rsd after =======

        # positive
        output$rsd_after.pos <- renderUI({
          plot_type <- input$fig4_data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_rsd_plt2.pos"))
          } else {
            plotOutput(outputId = ns("plot_rsd_plt2.pos"))
          }
        })
        output$plot_rsd_plt2.pos <- renderPlot({
          para = plot4_para()
          if(is.null(input$norm_vis)){return()}
          if(is.null(p2_norm$object_pos_norm)){
            print(gg_message_plot("No data in positive ion mode was detected.") %>% plotly::ggplotly())
          } else {
          p2_norm$object_pos_norm %>%
            activate_mass_dataset("sample_info") %>%
            dplyr::filter(class == "QC") %>%
            massqc::massqc_cumulative_rsd_plot(
              rsd_cutoff = para$fig4_rsd_cutoff,
              color = para$fig4_color,
              title = 'All of QC sample'
            )
          }
        })
        output$plotly_rsd_plt2.pos <- renderPlotly({
          para = plot4_para()
          if(is.null(input$norm_vis)){return()}
          if(is.null(p2_norm$object_pos_norm)){
            print(gg_message_plot("No data in positive ion mode was detected.") %>% plotly::ggplotly())
          } else {
          p2_norm$object_pos_norm%>%
            activate_mass_dataset("sample_info") %>%
            dplyr::filter(class == "QC")%>%
            massqc::massqc_cumulative_rsd_plot(
              rsd_cutoff = para$fig4_rsd_cutoff,
              color = para$fig4_color,
              title = 'All of QC sample'
            ) %>% plotly::ggplotly()
          }
        })
        # negative
        output$rsd_after.neg <- renderUI({
          plot_type <- input$fig4_data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_rsd_plt2.neg"))
          } else {
            plotOutput(outputId = ns("plot_rsd_plt2.neg"))
          }
        })
        output$plot_rsd_plt2.neg <- renderPlot({
          para = plot4_para()
          if(is.null(input$norm_vis)){return()}
          if(is.null(p2_norm$object_neg_norm)){
            print(gg_message_plot("No data in negative ion mode was detected.") %>% plotly::ggplotly())
          } else {
          p2_norm$object_neg_norm %>%
            activate_mass_dataset("sample_info") %>%
            dplyr::filter(class == "QC")%>%
            massqc::massqc_cumulative_rsd_plot(
              rsd_cutoff = para$fig4_rsd_cutoff,
              color = para$fig4_color,
              title = 'All of QC sample'
            )
          }
        })
        output$plotly_rsd_plt2.neg <- renderPlotly({
          para = plot4_para()
          if(is.null(input$norm_vis)){return()}
          if(is.null(p2_norm$object_neg_norm)){
            print(gg_message_plot("No data in negative ion mode was detected.") %>% plotly::ggplotly())
          } else {
          p2_norm$object_neg_norm %>%
            activate_mass_dataset("sample_info") %>%
            dplyr::filter(class == "QC")%>%
            massqc::massqc_cumulative_rsd_plot(
              rsd_cutoff = para$fig4_rsd_cutoff,
              color = para$fig4_color,
              title = 'All of QC sample'
            ) %>% plotly::ggplotly()
          }
        })


      }
    )


# download ----------------------------------------------------------------



    ###> fig3 ==============
    output$fig3_download = downloadHandler(
      filename = function() {
        paste0("03.rsd_plot_before_normalization.", download_para()$fig3_format)
      },
      content = function(file) {
        # extract parameters
        para <- plot3_para()
        para_d <- download_para()

        # draw condition
        if (!is.null(p2_dataclean$object_pos) & !is.null(p2_dataclean$object_neg)) {
          para_d$fig3_width = para_d$fig3_width * 2
          p1 <-  p2_norm$object_pos %>%
            activate_mass_dataset("sample_info") %>%
            dplyr::filter(class == "QC") %>%
            massqc::massqc_cumulative_rsd_plot(
              rsd_cutoff = para$fig3_rsd_cutoff,
              color = para$fig3_color,
              title = 'All of QC sample'
            )
          p2 <-  p2_norm$object_neg %>%
            activate_mass_dataset("sample_info") %>%
            dplyr::filter(class == "QC") %>%
            massqc::massqc_cumulative_rsd_plot(
              rsd_cutoff = para$fig3_rsd_cutoff,
              color = para$fig3_color,
              title = 'All of QC sample'
            )
          p <- (p1 + ggtitle("Positive")) + (p2 + ggtitle("Negative"))
        } else if (!is.null(p2_dataclean$object_pos)) {
          p <- p2_norm$object_pos %>%
            activate_mass_dataset("sample_info") %>%
            dplyr::filter(class == "QC") %>%
            massqc::massqc_cumulative_rsd_plot(
              rsd_cutoff = para$fig3_rsd_cutoff,
              color = para$fig3_color,
              title = 'All of QC sample'
            )
        } else {
          p <-  p2_norm$object_neg %>%
            activate_mass_dataset("sample_info") %>%
            dplyr::filter(class == "QC") %>%
            massqc::massqc_cumulative_rsd_plot(
              rsd_cutoff = para$fig3_rsd_cutoff,
              color = para$fig3_color,
              title = 'All of QC sample'
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
    ###> fig4 ==============
    output$fig4_download = downloadHandler(
      filename = function() {
        paste0("04.rsd_plot_after_normalization.", download_para()$fig4_format)
      },
      content = function(file) {
        # extract parameters
        para <- plot3_para()
        para_d <- download_para()

        # draw condition
        if (!is.null(p2_norm$object_pos_norm) & !is.null(p2_norm$object_neg_norm)) {
          para_d$fig4_width = para_d$fig4_width * 2
          p1 <-  p2_norm$object_pos_norm %>%
            activate_mass_dataset("sample_info") %>%
            dplyr::filter(class == "QC") %>%
            massqc::massqc_cumulative_rsd_plot(
              rsd_cutoff = para$fig4_rsd_cutoff,
              color = para$fig4_color,
              title = 'All of QC sample'
            )
          p2 <-  p2_norm$object_neg_norm %>%
            activate_mass_dataset("sample_info") %>%
            dplyr::filter(class == "QC") %>%
            massqc::massqc_cumulative_rsd_plot(
              rsd_cutoff = para$fig4_rsd_cutoff,
              color = para$fig4_color,
              title = 'All of QC sample'
            )
          p <- (p1 + ggtitle("Positive")) + (p2 + ggtitle("Negative"))
        } else if (!is.null(p2_dataclean$object_pos)) {
          p <- p2_norm$object_pos_norm%>%
            activate_mass_dataset("sample_info") %>%
            dplyr::filter(class == "QC") %>%
            massqc::massqc_cumulative_rsd_plot(
              rsd_cutoff = para$fig4_rsd_cutoff,
              color = para$fig4_color,
              title = 'All of QC sample'
            )
        } else {
          p <-  p2_norm$object_neg_norm %>%
            activate_mass_dataset("sample_info") %>%
            dplyr::filter(class == "QC") %>%
            massqc::massqc_cumulative_rsd_plot(
              rsd_cutoff = para$fig4_rsd_cutoff,
              color = para$fig4_color,
              title = 'All of QC sample'
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



  })
}

