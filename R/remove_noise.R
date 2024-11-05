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
            label = "MV cutoff",
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
              plotOutput(ns("mv_percentage_pos"))
            ),
            nav_panel(
              "Negative",
              plotOutput(ns("mv_percentage_neg"))
            ),
            nav_panel(
              shiny::icon("circle-info"),
              markdown("[show_variable_missing_values](https://www.tidymass.org/docs/chapter6/1-data_cleaning/)")
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


remove_noise_server <- function(id,volumes,prj_init,data_import_rv,data_clean_rv,data_export_rv) {
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

    ###> remove noise =========
    observeEvent(
      input$mv_start,
    {
      ####> check object ===============
      if(is.null(prj_init$sample_info)) {return()}
      if(!is.null(prj_init$object_negative.init) & !is.null(prj_init$object_positive.init) & prj_init$steps == "Remove noisey feature"){
        p2_dataclean$object_neg = prj_init$object_negative.init %>%
          activate_mass_dataset('sample_info') %>%
          dplyr::select('sample_id') %>%
          dplyr::left_join(prj_init$sample_info)
        p2_dataclean$object_pos = prj_init$object_positive.init %>%
          activate_mass_dataset('sample_info') %>%
          dplyr::select('sample_id') %>%
          dplyr::left_join(prj_init$sample_info)
      } else {
        if(is.null(data_clean_rv$object_neg)) {return()}
        if(is.null(data_clean_rv$object_pos)) {return()}
        p2_dataclean$object_neg = data_clean_rv$object_neg
        p2_dataclean$object_pos = data_clean_rv$object_pos
      }
      ####> remove noise ===============
      para <- anal_para()
      p2_dataclean$temp_mv_pos_noise <- find_noise(
        object = p2_dataclean$object_pos,
        tag = para$cut_index,
        qc_na_freq = para$qc_cut,
        S_na_freq = para$sample_cut
      )
      p2_dataclean$object_pos.mv = p2_dataclean$temp_mv_pos_noise$object_mv
      p2_dataclean$temp_mv_neg_noise <- find_noise(
        object = p2_dataclean$object_neg,
        tag = para$cut_index,
        qc_na_freq = para$qc_cut,
        S_na_freq = para$sample_cut
      )
      p2_dataclean$object_neg.mv = p2_dataclean$temp_mv_neg_noise$object_mv
      ####> table ===============
      output$vari_info_pos = renderDataTable_formated(
        actions = input$mv_start,
        condition1 = p2_dataclean$object_pos,
        tbl = p2_dataclean$temp_mv_neg_noise$noisy_tbl,
        filename.a = "1.Noisy_features_pos.csv"
      )
      output$vari_info_neg = renderDataTable_formated(
        actions = input$mv_start,
        condition1 = p2_dataclean$object_neg,
        tbl = p2_dataclean$temp_mv_neg_noise$noisy_tbl,
        filename.a = "1.Noisy_features_neg.csv"
      )


    }
    )
    ###> mv plot
    observeEvent(
      input$vis_butt_1,
      {

      }
    )
  })
}

