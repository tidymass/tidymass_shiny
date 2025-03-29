#' remove noisey features
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


remove_noise_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Remove noisy metabolic features',
    icon = bs_icon("eraser"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Noise remove parameters",
          icon = bsicons::bs_icon("gear"),
          sliderInput(
            inputId = ns("qc_cut"),
            label = "MV cutoff of QC samples (%)",
            min = 0,
            max = 100,
            value = 20
          ),
          selectInput(
            inputId = ns("cut_index"),
            label = "Sample group index",
            choices = c("class","group","..."),
            selected = "group"
          ),
          sliderInput(
            inputId = ns("sample_cut"),
            label = "MV cutoff (%)",
            min = 0,
            max = 100,
            value = 50
          ),
          actionButton(
            inputId = ns("mv_start"),
            label = "Start",icon = icon("play")
          )
      ),
      accordion_panel(
        title = "MV percentage of samples",
        icon = bsicons::bs_icon("image"),
        radioButtons(
          inputId = ns("sample_group"),
          label = "Sample group",
          choices = c("QC","Subject","All"),
          selected = "QC"
        ),
        actionButton(inputId = ns("vis_butt_1"),label = "Show plot",icon = icon("play"))
      )
      ),
      page_fluid(
        nav_panel(title = "remove noise",
          ###> mv percentage table ========
          navset_card_tab(
                    title = "MV percentage summary",
                    height = 400,
                    full_screen = TRUE,
                    nav_panel(
                      "Positive",
                      DT::dataTableOutput(outputId = ns("vari_info_pos"))
                    ),
                    nav_panel(
                      "Negative",
                      DT::dataTableOutput(outputId = ns("vari_info_neg"))
                    ),
                    nav_panel(
                      shiny::icon("circle-info"),
                      markdown("description of noise remove method.")
                    )
                  ),
          ###> mv plot ===========
          navset_card_tab(
            title = "MV percentage plot",
            height = 400,
            full_screen = TRUE,
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
                materialSwitch(inputId = ns("fig1_data_clean_plt_format"),label = "Interactive plot", status = "primary")
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
              uiOutput(ns("smv_plt.pos"),fill = T)
            ),
            nav_panel(
              "Negative",
              uiOutput(ns("smv_plt.neg"),fill = T)
            ),
            nav_panel(
              shiny::icon("circle-info"),
              HTML('<a href="https://www.tidymass.org/docs/chapter6/1-data_cleaning/" target="_blank">show_variable_missing_values</a>')
            )
          ),
          ###> status
          navset_card_tab(
            title = "Status",
            height = 400,
            full_screen = TRUE,
            nav_panel(
              "Positive",
              verbatimTextOutput(ns("obj_mv.pos"))
            ),
            nav_panel(
              "Negative",
              verbatimTextOutput(ns("obj_mv.neg"))
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
#' @import shiny
#' @importFrom shinyjs toggle runjs useShinyjs
#' @importFrom dplyr select left_join filter
#' @importFrom massdataset activate_mass_dataset
#' @importFrom plotly renderPlotly plotlyOutput
#' @importFrom shinyalert shinyalert
#' @param id module ID
#' @param volumes shinyFiles volumes configuration
#' @param prj_init Project initialization object
#' @param data_import_rv Reactive values for imported data
#' @param data_export_rv Reactive values for exported data
#' @noRd
remove_noise_server <- function(id, volumes, prj_init, data_import_rv, data_export_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    p2_dataclean <- reactiveValues(data = NULL)

    ### Parameter Reactives ###
    anal_para <- reactive({
      list(
        qc_cut = as.numeric(input$qc_cut),
        cut_index = as.character(input$cut_index),
        sample_cut = as.numeric(input$sample_cut)
      )
    })

    plot1_para <- reactive({
      list(
        fig1_sample_group = as.character(input$sample_group),
        fig1_color_by = as.character(input$color_by_smv),
        fig1_order_by = as.character(input$order_by_smv),
        fig1_percentage = as.logical(input$percentage_smv),
        fig1_show_x_text = as.logical(input$show_x_text_smv),
        fig1_show_x_ticks = as.logical(input$show_x_ticks_smv),
        fig1_desc = as.logical(input$desc_smv)
      )
    })

    download_para <- reactive({
      list(
        fig1_width = as.numeric(input$fig1_width),
        fig1_height = as.numeric(input$fig1_height),
        fig1_format = as.character(input$fig1_format)
      )
    })

    ### Dynamic UI Updates ###
    observe({
      tryCatch({
        cut_index_choices <- NULL
        sources <- list(
          prj_init$object_negative.init,
          prj_init$object_positive.init,
          data_import_rv$object_neg_raw,
          data_import_rv$object_pos_raw
        )

        valid_source <- Find(Negate(is.null), sources)
        if(!is.null(valid_source)) {
          cut_index_choices <- colnames(valid_source@sample_info)
          updateSelectInput(session, "cut_index",
                            choices = cut_index_choices,
                            selected = "group")
        }
      }, error = function(e) {
        message("UI update error: ", e$message)
      })
    })

    ### Noise Removal Core Logic ###
    observeEvent(input$mv_start, {
      # Data validation
      if(is.null(prj_init$sample_info)) {
        shinyalert("Error", "Sample information not loaded", type = "error")
        return()
      }

      # Detect available modes
      has_pos <- !is.null(data_import_rv$object_pos_raw) || !is.null(prj_init$object_positive.init)
      has_neg <- !is.null(data_import_rv$object_neg_raw) || !is.null(prj_init$object_negative.init)

      if(!has_pos && !has_neg) {
        shinyalert("Error", "No valid data detected", type = "error")
        return()
      }

      # Initialize processing
      shinyalert("Processing", "Starting noise removal...", type = "info", timer = 1000)

      # Process POS mode
      if(has_pos) {
        tryCatch({
          shinyalert("Processing", "Removing noise from POSITIVE mode data...",
                     type = "info", timer = 1300)

          p2_dataclean$object_pos <- if(!is.null(prj_init$object_positive.init) &&
                                        prj_init$steps == "Remove noisy feature") {
            prj_init$object_positive.init
          } else {
            data_import_rv$object_pos_raw
          }

          processed_pos <- tryCatch({
            find_noise_multiple(
              object = p2_dataclean$object_pos,
              tag = anal_para()$cut_index,
              qc_na_freq = anal_para()$qc_cut/100,
              S_na_freq = anal_para()$sample_cut/100
            )
          }, error = function(e) {
            return(NULL)
            shinyalert("POS Error", paste("POS processing failed:", e$message), type = "error")
          })

          p2_dataclean$object_pos_mv <- processed_pos$object_mv
          output$vari_info_pos <- renderDataTable_formated(
            actions = input$mv_start,condition1 = processed_pos$noisy_tbl,
            tbl = processed_pos$noisy_tbl,
            filename.a = "Noisy_features_pos.csv"
          )

          save_massobj(
            polarity = 'positive',
            file_path = paste0(prj_init$wd,"/Result/POS/Objects/"),
            stage = 'mv',
            obj = p2_dataclean$object_pos_mv
          )
        }, error = function(e) {
          return()
          shinyalert("POS Error", paste("POS processing failed:", e$message), type = "error")
        })
      }

      # Process NEG mode
      if(has_neg) {
        shinyalert("Processing", "Removing noise from NEGATIVE mode data...",
                   type = "info", timer = 2)

       if(!is.null(prj_init$object_negative.init) && prj_init$steps == "Remove noisy feature") {
         p2_dataclean$object_neg  =  prj_init$object_negative.init
       } else {
         p2_dataclean$object_neg  =  data_import_rv$object_neg_raw
       }

      if (!inherits(p2_dataclean$object_neg, "mass_dataset")) {
          stop("Input object must be a 'mass_dataset' class object.\n",
               "Please check the class of your input with class(object).")
          return(invisible())
      }

        processed_neg <- find_noise_multiple(
          object = p2_dataclean$object_neg,
          tag = anal_para()$cut_index,
          qc_na_freq = anal_para()$qc_cut/100,
          S_na_freq = anal_para()$sample_cut/100
        )

        p2_dataclean$object_neg_mv <- processed_neg$object_mv
        output$vari_info_neg <- renderDataTable_formated(
          actions = input$mv_start,condition1 = processed_neg$noisy_tbl,
          tbl = processed_neg$noisy_tbl,
          filename.a = "Noisy_features_neg.csv"
        )

        save_massobj(
          polarity = 'negative',
          file_path = paste0(prj_init$wd,"/Result/NEG/Objects/"),
          stage = 'mv',
          obj = p2_dataclean$object_neg_mv
        )
      }

      # Finalize processing
      data_import_rv$object_pos_mv <- p2_dataclean$object_pos_mv
      data_import_rv$object_neg_mv <- p2_dataclean$object_neg_mv

      shinyalert("Success", "Noise removal completed!", type = "success")
    })

    ### Visualization Handlers ###
    observeEvent(
      input$vis_butt_1,
      {
        #> positive
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
          if(is.null(p2_dataclean$object_pos_mv)){return()}
          if(para$fig1_sample_group != "All") {
            temp_obj <- p2_dataclean$object_pos_mv %>%
              activate_mass_dataset(what = 'sample_info') %>%
              dplyr::filter(class == para$fig1_sample_group)
          } else {temp_obj <- p2_dataclean$object_pos_mv}
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
          if(is.null(p2_dataclean$object_pos_mv)){return()}
          if(para$fig1_sample_group != "All") {
            temp_obj <- p2_dataclean$object_pos_mv %>%
              activate_mass_dataset(what = 'sample_info') %>%
              dplyr::filter(class == para$fig1_sample_group)
          } else {temp_obj <- p2_dataclean$object_pos_mv}
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
          if(is.null(p2_dataclean$object_neg_mv)){return()}
          if(para$fig1_sample_group != "All") {
            temp_obj <- p2_dataclean$object_neg_mv %>%
              activate_mass_dataset(what = 'sample_info') %>%
              dplyr::filter(class == para$fig1_sample_group)
          } else {temp_obj <- p2_dataclean$object_neg_mv}
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
          if(is.null(p2_dataclean$object_neg_mv)){return()}
          if(para$fig1_sample_group != "All") {
            temp_obj <- p2_dataclean$object_neg_mv %>%
              activate_mass_dataset(what = 'sample_info') %>%
              dplyr::filter(class == para$fig1_sample_group)
          } else {temp_obj <- p2_dataclean$object_neg_mv}
          p2_dataclean$object_neg_mv %>% massqc::show_sample_missing_values(
            color_by = para$fig1_color_by,
            order_by = para$fig1_order_by,
            percentage = para$fig1_percentage,
            show_x_text = para$fig1_show_x_text,
            show_x_ticks = para$fig1_show_x_ticks,
            desc = para$fig1_desc
          ) %>% plotly::ggplotly()
        })

      }
    )

    ##> Download ===============
    ###> fig1 ======
    output$fig1_download = downloadHandler(
      filename = function() {
        paste0("03.Sample_missing_value.", download_para()$fig1_format)
      },
      content = function(file) {
        # extract parameters
        para <- plot1_para()
        para_d <- download_para()

        # draw condition
        if (!is.null(p2_dataclean$object_neg_mv) & !is.null(p2_dataclean$object_neg_mv)) {
          para_d$fig1_width = para_d$fig1_width * 2
          if(para$fig1_sample_group != "All") {
            temp_obj.pos <- p2_dataclean$object_pos_mv %>%
              activate_mass_dataset(what = 'sample_info') %>%
              dplyr::filter(class == para$fig1_sample_group)
            temp_obj.neg <- p2_dataclean$object_neg_mv %>%
              activate_mass_dataset(what = 'sample_info') %>%
              dplyr::filter(class == para$fig1_sample_group)
          } else {
            temp_obj.pos <- p2_dataclean$object_pos_mv
            temp_obj.neg <- p2_dataclean$object_neg_mv
          }
          p1 <- temp_obj.pos %>% massqc::show_sample_missing_values(
            color_by = para$fig1_color_by,
            order_by = para$fig1_order_by,
            percentage = para$fig1_percentage,
            show_x_text = para$fig1_show_x_text,
            show_x_ticks = para$fig1_show_x_ticks,
            desc = para$fig1_desc
          )
          p2 <- temp_obj.neg %>% massqc::show_sample_missing_values(
            color_by = para$fig1_color_by,
            order_by = para$fig1_order_by,
            percentage = para$fig1_percentage,
            show_x_text = para$fig1_show_x_text,
            show_x_ticks = para$fig1_show_x_ticks,
            desc = para$fig1_desc
          )
          p <- (p1 + ggtitle("Positive")) + (p2 + ggtitle("Negative"))
        } else if (!is.null(p2_dataclean$object_neg_mv)) {
          if(para$fig1_sample_group != "All") {
            temp_obj.pos <- p2_dataclean$object_pos_mv %>%
              activate_mass_dataset(what = 'sample_info') %>%
              dplyr::filter(class == para$fig1_sample_group)
          } else {
            temp_obj.pos <- p2_dataclean$object_pos_mv
          }
          p <- p2_dataclean$object_neg_mv %>% massqc::show_sample_missing_values(
            color_by = para$fig1_color_by,
            order_by = para$fig1_order_by,
            percentage = para$fig1_percentage,
            show_x_text = para$fig1_show_x_text,
            show_x_ticks = para$fig1_show_x_ticks,
            desc = para$fig1_desc
          )
        } else {
          if(para$fig1_sample_group != "All") {
            temp_obj.neg <- p2_dataclean$object_neg_mv %>%
              activate_mass_dataset(what = 'sample_info') %>%
              dplyr::filter(class == para$fig1_sample_group)
          } else {
            temp_obj.neg <- p2_dataclean$object_neg_mv
          }
          p <- p2_dataclean$object_neg_mv %>% massqc::show_sample_missing_values(
            color_by = para$fig1_color_by,
            order_by = para$fig1_order_by,
            percentage = para$fig1_percentage,
            show_x_text = para$fig1_show_x_text,
            show_x_ticks = para$fig1_show_x_ticks,
            desc = para$fig1_desc
          )
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

  })
}
