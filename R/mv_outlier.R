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

#' Remove Outliers Server
#'
#' Server logic for outlier removal in metabolomics data processing
#' @param input,output,session Shiny module parameters
#' @import shiny
#' @importFrom shinyjs toggle runjs useShinyjs
#' @importFrom dplyr select left_join filter
#' @importFrom massdataset activate_mass_dataset
#' @importFrom plotly renderPlotly plotlyOutput
#' @param id Module namespace ID
#' @param volumes File system volumes configuration
#' @param prj_init Project initialization object
#' @param data_import_rv Reactive values for imported data
#' @param data_clean_rv Reactive values for cleaned data
#' @param data_export_rv Reactive values for exported data
#' @noRd
remove_outlier_server <- function(id, volumes, prj_init, data_import_rv, data_clean_rv, data_export_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    p2_dataclean <- reactiveValues(data = NULL)

    # Helper functions --------------------------------------------------------
    check_ion_modes <- function(data_rv, prj) {
      list(
        has_pos = !is.null(data_rv$object_pos_mv) || !is.null(prj$object_positive.init),
        has_neg = !is.null(data_rv$object_neg_mv) || !is.null(prj$object_negative.init)
      )
    }

    process_for_pca <- function(object, scale = FALSE) {
      processed <- object %>%
        `+`(1) %>%
        log(2)

      if(scale) processed <- scale(processed)
      processed
    }

    detect_outliers <- function(object) {
      object %>%
        `+`(1) %>%
        log(2) %>%
        scale() %>%
        detect_outlier()
    }


    ### Parameter Reactives ###
    analy_para <- reactive({
      list(
        mv_method = as.character(input$mv_method),
        by_witch = as.character(input$by_witch),
        outlier_in_pos = as.character(input$outlier_in_pos),
        outlier_in_neg = as.character(input$outlier_in_neg)
      )
    })

    ### Visualization Parameters ###
    plot1_para <- reactive({
      list(
        fig1_color_by = as.character(input$color_by_smv),
        fig1_order_by = as.character(input$order_by_smv),
        fig1_percentage = as.logical(input$percentage_smv),
        fig1_show_x_text = as.logical(input$show_x_text_smv),
        fig1_show_x_ticks = as.logical(input$show_x_ticks_smv),
        fig1_desc = as.logical(input$desc_smv)
      )
    })

    plot2_para <- reactive({
      list(
        fig2_color_by = as.character(input$fig2_color_by),
        fig2_scale = as.logical(input$fig2_scale),
        fig2_point_alpha = as.numeric(input$fig2_point_alpha),
        fig2_frame = as.logical(input$fig2_frame),
        fig2_line = as.logical(input$fig2_line),
        fig2_color_by_3d = as.character(input$fig2_color_by_3d),
        fig2_scale_3d = as.logical(input$fig2_scale_3d),
        fig2_x_axis = as.character(input$fig2_x_axis),
        fig2_y_axis = as.character(input$fig2_y_axis),
        fig2_z_axis = as.character(input$fig2_z_axis)
      )
    })

    ### Dynamic UI Updates ###
    observe({
      tryCatch({
        sources <- list(
          prj_init$object_negative.init,
          prj_init$object_positive.init,
          data_import_rv$object_neg_mv,
          data_import_rv$object_pos_mv
        )

        valid_source <- Find(Negate(is.null), sources)
        if(!is.null(valid_source)) {
          col_names <- colnames(valid_source@sample_info)
          sample_ids <- valid_source@sample_info$sample_id
          updateSelectInput(session, "color_by_smv", choices = col_names, selected = "group")
          updateSelectInput(session, "order_by_smv", choices = col_names, selected = "injection.order")
          updateSelectInput(session, "fig2_color_by", choices = col_names, selected = "group")
          updateSelectInput(session, "fig2_color_by_3d", choices = col_names, selected = "group")
          updateSelectInput(session, "outlier_in_pos", choices = c("none", sample_ids), selected = "none")
          updateSelectInput(session, "outlier_in_neg", choices = c("none", sample_ids), selected = "none")
        }
      }, error = function(e) {
        message("UI update error: ", e$message)
      })
    })

    ### Core Outlier Detection Logic ###
    observeEvent(input$vis_butt_1, {
      # Validate data presence
      modes <- check_ion_modes(data_import_rv, prj_init)
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
      if(is.null(data_import_rv$object_pos_mv) && is.null(data_import_rv$object_neg_mv)){
        if (!is.null(prj_init$object_negative.init) || !is.null(prj_init$object_positive.init)) {
          # Data initialized but current step is invalid
          if (prj_init$steps != "Remove outlier") {
            shinyalert(
              "Step Error",
              "Invalid workflow sequence detected.\nPlease restart from the 'REMOVE OUTLIER' step.",
              type = "error"
            )
            return()
          }
        }
      }


      # Load data based on processing step
      if(prj_init$steps == "Remove outlier") {
        if(modes$has_pos) p2_dataclean$object_pos <- prj_init$object_positive.init
        if(modes$has_neg) p2_dataclean$object_neg <- prj_init$object_negative.init
      } else {
        p2_dataclean$object_pos <- data_import_rv$object_pos_mv
        p2_dataclean$object_neg <- data_import_rv$object_neg_mv
      }

      ### Missing Value Visualization ###
      # Positive mode
      if(modes$has_pos) {
        output$smv_plt.pos <- renderUI({
          if(input$fig1_data_clean_plt_format) {
            plotlyOutput(ns("plotly_smv_plt.pos"))
          } else {
            plotOutput(ns("plot_smv_plt.pos"))
          }
        })

        output$plot_smv_plt.pos <- renderPlot({
          obj <- data_clean_rv$object_pos_outlier %||% p2_dataclean$object_pos
          massqc::show_sample_missing_values(
            obj,
            color_by = plot1_para()$fig1_color_by,
            order_by = plot1_para()$fig1_order_by,
            percentage = plot1_para()$fig1_percentage,
            show_x_text = plot1_para()$fig1_show_x_text,
            show_x_ticks = plot1_para()$fig1_show_x_ticks,
            desc = plot1_para()$fig1_desc
          )
        })

        output$plotly_smv_plt.pos <- renderPlotly({
          obj <- data_clean_rv$object_pos_outlier %||% p2_dataclean$object_pos
          massqc::show_sample_missing_values(
            obj,
            color_by = plot1_para()$fig1_color_by,
            order_by = plot1_para()$fig1_order_by,
            percentage = plot1_para()$fig1_percentage,
            show_x_text = plot1_para()$fig1_show_x_text,
            show_x_ticks = plot1_para()$fig1_show_x_ticks,
            desc = plot1_para()$fig1_desc
          ) %>% plotly::ggplotly()
        })
      }

      # Negative mode
      if(modes$has_neg) {
        output$smv_plt.neg <- renderUI({
          if(input$fig1_data_clean_plt_format) {
            plotlyOutput(ns("plotly_smv_plt.neg"))
          } else {
            plotOutput(ns("plot_smv_plt.neg"))
          }
        })

        output$plot_smv_plt.neg <- renderPlot({
          obj <- data_clean_rv$object_neg_outlier %||% p2_dataclean$object_neg
          massqc::show_sample_missing_values(
            obj,
            color_by = plot1_para()$fig1_color_by,
            order_by = plot1_para()$fig1_order_by,
            percentage = plot1_para()$fig1_percentage,
            show_x_text = plot1_para()$fig1_show_x_text,
            show_x_ticks = plot1_para()$fig1_show_x_ticks,
            desc = plot1_para()$fig1_desc
          )
        })

        output$plotly_smv_plt.neg <- renderPlotly({
          obj <- data_clean_rv$object_neg_outlier %||% p2_dataclean$object_neg
          massqc::show_sample_missing_values(
            obj,
            color_by = plot1_para()$fig1_color_by,
            order_by = plot1_para()$fig1_order_by,
            percentage = plot1_para()$fig1_percentage,
            show_x_text = plot1_para()$fig1_show_x_text,
            show_x_ticks = plot1_para()$fig1_show_x_ticks,
            desc = plot1_para()$fig1_desc
          ) %>% plotly::ggplotly()
        })
      }

      ### PCA Analysis ###
      # Positive mode PCA
      if(modes$has_pos) {
        output$fig2_pca.pos <- renderUI({
          if(input$fig2_data_clean_plt_format) {
            plotlyOutput(ns("plotly_pca.pos"))
          } else {
            plotOutput(ns("plot_pca.pos"))
          }
        })

        output$plot_pca.pos <- renderPlot({
          obj <- data_clean_rv$object_pos_outlier %||% p2_dataclean$object_pos
          processed_data <- process_for_pca(obj, plot2_para()$fig2_scale)
          massqc::massqc_pca(
            processed_data,
            color_by = plot2_para()$fig2_color_by,
            point_alpha = plot2_para()$fig2_point_alpha,
            frame = plot2_para()$fig2_frame,
            line = plot2_para()$fig2_line
          )
        })

        output$plotly_pca.pos <- renderPlotly({
          obj <- data_clean_rv$object_pos_outlier %||% p2_dataclean$object_pos
          processed_data <- process_for_pca(obj, plot2_para()$fig2_scale_3d)
          massqc_pca_3d(
            processed_data,
            color_by = plot2_para()$fig2_color_by_3d,
            x_axis = plot2_para()$fig2_x_axis,
            y_axis = plot2_para()$fig2_y_axis,
            z_axis = plot2_para()$fig2_z_axis
          )
        })
      }

      # Negative mode PCA
      if(modes$has_neg) {
        output$fig2_pca.neg <- renderUI({
          if(input$fig2_data_clean_plt_format) {
            plotlyOutput(ns("plotly_pca.neg"))
          } else {
            plotOutput(ns("plot_pca.neg"))
          }
        })

        output$plot_pca.neg <- renderPlot({
          obj <- data_clean_rv$object_neg_outlier %||% p2_dataclean$object_neg
          processed_data <- process_for_pca(obj, plot2_para()$fig2_scale)
          massqc::massqc_pca(
            processed_data,
            color_by = plot2_para()$fig2_color_by,
            point_alpha = plot2_para()$fig2_point_alpha,
            frame = plot2_para()$fig2_frame,
            line = plot2_para()$fig2_line
          )
        })

        output$plotly_pca.neg <- renderPlotly({
          obj <- data_clean_rv$object_neg_outlier %||% p2_dataclean$object_neg
          processed_data <- process_for_pca(obj, plot2_para()$fig2_scale_3d)
          massqc_pca_3d(
            processed_data,
            color_by = plot2_para()$fig2_color_by_3d,
            x_axis = plot2_para()$fig2_x_axis,
            y_axis = plot2_para()$fig2_y_axis,
            z_axis = plot2_para()$fig2_z_axis
          )
        })
      }

      ### Outlier Detection ###
      if(modes$has_pos) {
        outlier_samples.pos <- detect_outliers(p2_dataclean$object_pos)
        p2_dataclean$outlier_tbl.pos <- extract_outlier_table(outlier_samples.pos)
        output$info_outlier.pos <- renderPrint(print(outlier_samples.pos))
        output$tbl_outlier.pos <- renderDataTable_formated(
          tbl = p2_dataclean$outlier_tbl.pos,
          filename.a = "outlier_summary_pos"
        )
      }

      if(modes$has_neg) {
        outlier_samples.neg <- detect_outliers(p2_dataclean$object_neg)
        p2_dataclean$outlier_tbl.neg <- extract_outlier_table(outlier_samples.neg)
        output$info_outlier.neg <- renderPrint(print(outlier_samples.neg))
        output$tbl_outlier.neg <- renderDataTable_formated(
          tbl = p2_dataclean$outlier_tbl.neg,
          filename.a = "outlier_summary_neg"
        )
      }
    })

    ### Outlier Removal Execution ###
    observeEvent(input$mv_start, {
      modes <- check_ion_modes(data_import_rv, prj_init)
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
      if(is.null(data_import_rv$object_pos_mv) && is.null(data_import_rv$object_neg_mv)){
        if (!is.null(prj_init$object_negative.init) || !is.null(prj_init$object_positive.init)) {
          # Data initialized but current step is invalid
          if (prj_init$steps != "Remove outlier") {
            shinyalert(
              "Step Error",
              "Invalid workflow sequence detected.\nPlease restart from the 'REMOVE OUTLIER' step.",
              type = "error"
            )
            return()
          }
        }
      }


      # Process positive mode data
      if(modes$has_pos) {
        para = analy_para()
        temp_pos_res = process_outliers(
          object = p2_dataclean$object_pos,
          mv_method = para$mv_method,
          by_witch = para$by_witch,
          outlier_samples = para$outlier_in_pos,
          outlier_table = p2_dataclean$outlier_tbl.pos
        )
        p2_dataclean$object_pos_mv = temp_pos_res[[1]]
        object_pos_outlier = p2_dataclean$object_pos_mv
        save(object_pos_outlier,file = file.path(prj_init$mass_dataset_dir, "03.object_pos_outlier.rda"))

        # Build alert message
        method_type <- ifelse(para$mv_method == "By tidymass",
                              paste0("Auto detection (Criteria: ",paste0(para$by_witch,collapse = " | "),")"),
                              "Manual selection")

        alert_msg <- paste(
          "Positive Mode Outlier Processing",
          "\n\nMethod: ", method_type,
          "\n\nDetected outliers: "
        )

        if(length(temp_pos_res$outlier_ids) > 0) {
          alert_msg <- paste0(
            alert_msg,
            paste(temp_pos_res$outlier_ids, collapse = ", ")
          )
          alert_type <- "warning"
        } else {
          alert_msg <- paste0(alert_msg, "No outliers detected")
          alert_type <- "info"
        }

        # Show shinyalert
        shinyalert(
          title = "Outlier Processing Result",
          text = alert_msg,
          type = alert_type,
          closeOnEsc = TRUE,
          animation = "slide-from-top",
          className = "outlier-alert"
        )
      }

      # Process negative mode data
      if(modes$has_neg) {
        para <- analy_para()
        temp_neg_res <- process_outliers(
          object = p2_dataclean$object_neg,
          mv_method = para$mv_method,
          by_witch = para$by_witch,
          outlier_samples = para$outlier_in_neg,
          outlier_table = p2_dataclean$outlier_tbl.neg
        )

        # Update negative mode data
        p2_dataclean$object_neg_mv <- temp_neg_res[[1]]
        object_neg_outlier <- p2_dataclean$object_neg_mv
        save(object_neg_outlier,file = file.path(prj_init$mass_dataset_dir, "03.object_neg_outlier.rda"))

        # Build alert message
        method_type <- ifelse(para$mv_method == "By tidymass",
                              paste0("Auto detection (Criteria: ",paste0(para$by_witch,collapse = " | "),")"),
                              "Manual selection")
        alert_msg <- paste(
          "Negative Mode Outlier Processing",
          "\n\nMethod: ", method_type,
          "\n\nDetected outliers: "
        )

        if(length(temp_neg_res$outlier_ids) > 0) {
          alert_msg <- paste0(
            alert_msg,
            paste(temp_neg_res$outlier_ids, collapse = ", ")
          )
          alert_type <- "warning"
        } else {
          alert_msg <- paste0(alert_msg, "No outliers detected")
          alert_type <- "info"
        }

        # Show shinyalert
        shinyalert(
          title = "Outlier Processing Result",
          text = alert_msg,
          type = alert_type,
          closeOnEsc = TRUE,
          animation = "slide-from-top",
          className = "outlier-alert"
        )
      }
      # Update results display

      print(class(p2_dataclean$object_pos_mv))



      output$obj_outlier.pos = check_massdata_info(
        object = p2_dataclean$object_pos_mv,
        mode = "positive"
      )

      output$obj_outlier.neg  = check_massdata_info(
        object = p2_dataclean$object_neg_mv,
        mode = "negative"
      )

      if(modes$has_pos) {
        data_clean_rv$object_pos_outlier <- p2_dataclean$object_pos_mv
      }

      if(modes$has_neg) {
        data_clean_rv$object_neg_outlier <- p2_dataclean$object_neg_mv
      }
    })
  })
}
