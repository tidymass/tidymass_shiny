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
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs toggle runjs useShinyjs
#' @importFrom dplyr select left_join filter
#' @importFrom massdataset activate_mass_dataset
#' @importFrom plotly renderPlotly plotlyOutput
#' @param id module of server
#' @param volumes shinyFiles volumes
#' @param prj_init use project init variables.
#' @param data_import_rv reactivevalues mass_dataset export
#' @param data_export_rv reactivevalues mass_dataset export
#' @noRd


remove_noise_server <- function(id,volumes,prj_init,data_import_rv,data_export_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    p2_dataclean <- reactiveValues(data = NULL)

    ###> plot parameters =========
    anal_para = reactive({
      list(
        qc_cut = input$qc_cut %>% as.numeric(),
        cut_index = input$cut_index %>% as.character(),
        sample_cut = input$sample_cut %>% as.numeric()
      )
    })

    plot1_para = reactive({
      list(
        fig1_sample_group = input$sample_group %>% as.character(),
        fig1_color_by = input$color_by_smv %>% as.character(),
        fig1_order_by = input$order_by_smv %>% as.character(),
        fig1_percentage = input$percentage_smv %>% as.logical(),
        fig1_show_x_text = input$show_x_text_smv %>% as.logical(),
        fig1_show_x_ticks = input$show_x_ticks_smv %>% as.logical(),
        fig1_desc = input$desc_smv %>% as.logical()
      )
    })
    ###> download parameters =========
    download_para = reactive({
      list(
        fig1_width = as.numeric(input$fig1_width),
        fig1_height = as.numeric(input$fig1_height),
        fig1_format = as.character(input$fig1_format)
      )
    })


    observe({
      cut_index_choices <- NULL

      if (!is.null(prj_init$object_negative.init)) {
        cut_index_choices <- colnames(prj_init$object_negative.init@sample_info)
      } else if (!is.null(prj_init$object_positive.init)) {
        cut_index_choices <- colnames(prj_init$object_positive.init@sample_info)
      } else if (!is.null(data_import_rv$object_neg)) {
        cut_index_choices <- colnames(data_import_rv$object_neg@sample_info)
      } else if (!is.null(data_import_rv$object_pos)) {
        cut_index_choices <- colnames(data_import_rv$object_pos@sample_info)
      }

      # update
      if (!is.null(cut_index_choices)) {
        updateSelectInput(session, "cut_index", choices = cut_index_choices, selected = "group")
      }
    })


    ###> remove noise =========
    observeEvent(
      input$mv_start,
      {
        ####> check object ===============
        if(is.null(prj_init$sample_info)) {return()}
        if(!is.null(prj_init$object_negative.init) & !is.null(prj_init$object_positive.init) & prj_init$steps == "Remove noisey feature"){
          p2_dataclean$object_neg = prj_init$object_negative.init
          p2_dataclean$object_pos = prj_init$object_positive.init
        } else {
          if(is.null(data_import_rv$object_neg)) {return()}
          if(is.null(data_import_rv$object_pos)) {return()}
          p2_dataclean$object_neg = data_import_rv$object_neg
          p2_dataclean$object_pos= data_import_rv$object_pos
        }



        ####> remove noise ===============
        para <- anal_para()
        qc_na_freq <- para$qc_cut / 100
        S_na_freq <- para$sample_cut / 100
        temp_mv_pos_noise <- find_noise_multiple(
          object = p2_dataclean$object_pos,
          tag = para$cut_index,
          qc_na_freq = qc_na_freq,
          S_na_freq = S_na_freq
        )


        p2_dataclean$object_pos.mv = temp_mv_pos_noise$object_mv
        temp_mv_neg_noise <- find_noise_multiple(
          object = p2_dataclean$object_neg,
          tag = para$cut_index,
          qc_na_freq = qc_na_freq,
          S_na_freq = S_na_freq
        )
        p2_dataclean$object_neg.mv = temp_mv_neg_noise$object_mv
        #print('check point 3')

        ####> table ===============
        output$vari_info_pos = renderDataTable_formated(
          actions = input$mv_start,
          tbl = temp_mv_pos_noise$noisy_tbl,
          filename.a = "1.Noisy_features_pos.csv"
        )
        output$vari_info_neg = renderDataTable_formated(
          actions = input$mv_start,
          tbl = temp_mv_neg_noise$noisy_tbl,
          filename.a = "1.Noisy_features_neg.csv"
        )
        #print('check point 4')


        #> information of mass datasets

        output$obj_mv.pos = renderPrint({
          print(p2_dataclean$object_neg.mv)
        })

        output$obj_mv.neg = renderPrint({
          print(p2_dataclean$object_pos.mv)
        })
        #print('check point 5')
        #> save object
        save_massobj(
          polarity = 'negative',
          file_path = paste0(prj_init$wd,"/Result/NEG/Objects/"),
          stage = 'mv',
          obj = p2_dataclean$object_neg.mv)
        save_massobj(
          polarity = 'positive',
          file_path = paste0(prj_init$wd,"/Result/POS/Objects/"),
          stage = 'mv',
          obj = p2_dataclean$object_pos.mv)
        observe({
          updateSelectInput(session, "color_by_smv",choices = colnames(p2_dataclean$object_neg.mv@sample_info),selected = "group")
          updateSelectInput(session, "order_by_smv",choices = colnames(p2_dataclean$object_neg.mv@sample_info),selected = "sample_id")
        })
        #print('check point 5')
        data_import_rv$object_neg.mv = p2_dataclean$object_neg.mv
        data_import_rv$object_pos.mv  = p2_dataclean$object_pos.mv
      }
    )


    ###> mv plot =====

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
        print("Check point 1")
        output$plot_smv_plt.pos <- renderPlot({
          para = plot1_para()
          if(is.null(p2_dataclean$object_pos.mv)){return()}
          if(para$fig1_sample_group != "All") {
            temp_obj <- p2_dataclean$object_pos.mv %>%
              activate_mass_dataset(what = 'sample_info') %>%
              dplyr::filter(class == para$fig1_sample_group)
          } else {temp_obj <- p2_dataclean$object_pos.mv}
          temp_obj %>% massqc::show_sample_missing_values(
            color_by = para$fig1_color_by,
            order_by = para$fig1_order_by,
            percentage = para$fig1_percentage,
            show_x_text = para$fig1_show_x_text,
            show_x_ticks = para$fig1_show_x_ticks,
            desc = para$fig1_desc
          )
        })
        print("Check point 2")
        output$plotly_smv_plt.pos <- renderPlotly({
          para = plot1_para()
          if(is.null(p2_dataclean$object_pos.mv)){return()}
          if(para$fig1_sample_group != "All") {
            temp_obj <- p2_dataclean$object_pos.mv %>%
              activate_mass_dataset(what = 'sample_info') %>%
              dplyr::filter(class == para$fig1_sample_group)
          } else {temp_obj <- p2_dataclean$object_pos.mv}
          temp_obj %>% massqc::show_sample_missing_values(
            color_by = para$fig1_color_by,
            order_by = para$fig1_order_by,
            percentage = para$fig1_percentage,
            show_x_text = para$fig1_show_x_text,
            show_x_ticks = para$fig1_show_x_ticks,
            desc = para$fig1_desc
          ) %>% plotly::ggplotly()
        })
        print("Check point 3")
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
          if(is.null(p2_dataclean$object_neg.mv)){return()}
          if(para$fig1_sample_group != "All") {
            temp_obj <- p2_dataclean$object_neg.mv %>%
              activate_mass_dataset(what = 'sample_info') %>%
              dplyr::filter(class == para$fig1_sample_group)
          } else {temp_obj <- p2_dataclean$object_neg.mv}
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
          if(is.null(p2_dataclean$object_neg.mv)){return()}
          if(para$fig1_sample_group != "All") {
            temp_obj <- p2_dataclean$object_neg.mv %>%
              activate_mass_dataset(what = 'sample_info') %>%
              dplyr::filter(class == para$fig1_sample_group)
          } else {temp_obj <- p2_dataclean$object_neg.mv}
          p2_dataclean$object_neg.mv %>% massqc::show_sample_missing_values(
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
        if (!is.null(p2_dataclean$object_neg.mv) & !is.null(p2_dataclean$object_neg.mv)) {
          para_d$fig1_width = para_d$fig1_width * 2
          if(para$fig1_sample_group != "All") {
            temp_obj.pos <- p2_dataclean$object_pos.mv %>%
              activate_mass_dataset(what = 'sample_info') %>%
              dplyr::filter(class == para$fig1_sample_group)
            temp_obj.neg <- p2_dataclean$object_neg.mv %>%
              activate_mass_dataset(what = 'sample_info') %>%
              dplyr::filter(class == para$fig1_sample_group)
          } else {
            temp_obj.pos <- p2_dataclean$object_pos.mv
            temp_obj.neg <- p2_dataclean$object_neg.mv
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
        } else if (!is.null(p2_dataclean$object_neg.mv)) {
          if(para$fig1_sample_group != "All") {
            temp_obj.pos <- p2_dataclean$object_pos.mv %>%
              activate_mass_dataset(what = 'sample_info') %>%
              dplyr::filter(class == para$fig1_sample_group)
          } else {
            temp_obj.pos <- p2_dataclean$object_pos.mv
          }
          p <- p2_dataclean$object_neg.mv %>% massqc::show_sample_missing_values(
            color_by = para$fig1_color_by,
            order_by = para$fig1_order_by,
            percentage = para$fig1_percentage,
            show_x_text = para$fig1_show_x_text,
            show_x_ticks = para$fig1_show_x_ticks,
            desc = para$fig1_desc
          )
        } else {
          if(para$fig1_sample_group != "All") {
            temp_obj.neg <- p2_dataclean$object_neg.mv %>%
              activate_mass_dataset(what = 'sample_info') %>%
              dplyr::filter(class == para$fig1_sample_group)
          } else {
            temp_obj.neg <- p2_dataclean$object_neg.mv
          }
          p <- p2_dataclean$object_neg.mv %>% massqc::show_sample_missing_values(
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

