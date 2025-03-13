#' Enrichment
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

enrichment_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Enrichment',
    icon = bs_icon("balloon"),
    layout_sidebar(
      sidebar = accordion(
        id = ns("database_accordion"),
        open = ns("select_type_panel"),
        accordion_panel(
          id = ns("select_type_panel"),
          title = "Select data type",
          icon = bsicons::bs_icon("menu-app"),
          radioButtons(
            inputId = ns('hsa_db_type'),
            label = "Pathway data type",
            choices = c("KEGG (hsa)", "HMDB (hsa)", "Wikipedia (hsa)", "Customized (any species)"),
            selected = "KEGG (hsa)"
          )
        ),
        accordion_panel(
          id = ns("upload_panel"),
          title = "Upload custom database",
          icon = bsicons::bs_icon("folder"),
          shinyFilesButton(
            id = ns('pathway_db'),
            buttonType = "default",
            title = "load pathway database (option) ",
            label = 'load database file ',
            class = NULL,, multiple = FALSE,
            icon = bsicons::bs_icon("folder")
          )
        ),
        accordion_panel(
          title = "Enrichment parameters",
          icon = bsicons::bs_icon("gear"),
          selectInput(
            inputId = ns("p_adjust_method"),
            label = "p value adjust methods",
            choices = p.adjust.methods,
            selected = "BH",multiple = FALSE
          ),
          textInput(
            inputId = ns("threads"),
            label = tooltip(
              trigger = list(
                "threads",
                bsicons::bs_icon("info-circle")
              ),
              "Do not exceed the maximum number of cores of the machine."
            ),
            value = 5,
          ),
          selectInput(
            inputId = ns("enrich_method"),
            label = tooltip(
              trigger = list(
                "threads",
                bsicons::bs_icon("info-circle")
              ),
              "Do not exceed the maximum number of cores of the machine."
            ),choices = c("hypergeometric","fisher.test")
          ),
          sliderInput(
            inputId = ns('p_cutoff'),
            label = "p_value cutoff",
            min = 0,max = 1,value = 0.05,
            step = 0.01
          ),
          actionButton(inputId = ns('enrich_start'),label = "Start enrichment",icon = icon("play"))
        )
        ),
      page_fluid(
        card(
          card_header(
            "Enrichment table"
          ),
          height = 400,
          full_screen = TRUE,
          layout_columns(
            col_widths = c(6,6),
            dataTableOutput(ns('enrich_tbl')),
            dataTableOutput(ns("Compound_detail"))
          )
        ),
        layout_column_wrap(
          width = 1/2,
          height = 400,
          navset_card_tab(
            height = 400,
            full_screen = T,
            title = "Barplot",
            sidebar = accordion(
              open = FALSE,
              accordion_panel(
                title = 'Parameters',
                selectInput(
                  inputId = ns('fig1_x_axis'), label = 'x axis by', choices = c("p_value_adjust", "p_value"),selected = 'p_value_adjust',multiple = F
                ),
                sliderInput(
                  inputId = ns('fig1_cutoff'),label = 'cutoff',min = 0,max = 1,value = 0.05,step = 0.01
                ),
                textInput(
                  inputId = ns('fig1_top'),label = tooltip(
                    trigger = list(
                      "Top",
                      bsicons::bs_icon("info-circle")
                    ),
                    "Show top N pathways"
                  ),value = 10
                ),
                textInput(
                  inputId = ns('fig1_axis.text.x.width'),label = tooltip(
                    trigger = list(
                      "axis.text.x.width",
                      bsicons::bs_icon("info-circle")
                    ),
                    "Figure width"
                  ),value = 15
                ),
                textInput(
                  inputId = ns('fig1_axis.text.y.width'),label = tooltip(
                    trigger = list(
                      "axis.text.y.width",
                      bsicons::bs_icon("info-circle")
                    ),
                    "Figure height"
                  ),value = 15
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
              "Barplot",
              card_title("Barplot"),
              uiOutput(ns("fig1_barplot"),fill = T)
            )
          ),
          navset_card_tab(
            height = 400,
            full_screen = TRUE,
            title = 'Enrich scatter plot',
            sidebar = accordion(
              open = FALSE,
              accordion_panel(
                title = 'Parameters',
                selectInput(
                  ns('fig2_x_axis'),"x_axis",choices = c("mapped_percentage", "mapped_number"),"mapped_percentage"
                ),
                selectInput(
                  ns('fig2_y_axis'),"y_axis",choices = c("p_value_adjust", "p_value"),"p_value_adjust"
                ),
                selectInput(
                  ns('fig2_point_size'),"point_size",choices = c("mapped_percentage", "all_number"),"mapped_percentage"
                ),
                textInput(
                  inputId = ns('fig2_x_axis_cutoff'),
                  label = "x_axis_cutoff",
                  value = 0
                ),
                sliderInput(
                  inputId = ns('fig2_y_axis_cutoff'),
                  label = "y_axis_cutoff",min = 0,max = 1,
                  value = 0.05,step = 0.01
                ),
                radioButtons(
                  inputId = ns('fig2_label'),
                  label = "label",
                  choices = c("TRUE","FALSE"),selected = "TRUE"
                ),
                textInput(
                  ns('fig2_label_size'),"label_size",4
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
              "Scatter plot",
              card_title("Scatter Plot"),
              uiOutput(ns("fig2_corr_plt.pos"),fill = T)
            )

          )
        )
      )

      )
    )

}


#' Enrichment analysis
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs toggle runjs useShinyjs
#' @importFrom dplyr select left_join
#' @importFrom massdataset activate_mass_dataset
#' @importFrom plotly renderPlotly plotlyOutput
#' @import metpath
#' @import plantmdb
#' @param id module of server
#' @param volumes shinyFiles volumes
#' @param prj_init use project init variables.
#' @param data_import_rv reactivevalues mass_dataset export
#' @param data_clean_rv reactivevalues p2 dataclean
#' @param data_export_rv reactivevalues mass_dataset export
#' @param data_enrich reactivevalues data annotation
#' @noRd


enrichment_server <- function(id,volumes,prj_init,data_import_rv,data_clean_rv,data_enrich,data_export_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    p2_dataclean <- reactiveValues(data = NULL)
    #> load positive object
    enrich_para = reactive({
      list(
        hsa_db_type = input$hsa_db_type %>% as.character(),
        p_adjust_method = input$p_adjust_method %>% as.character(),
        threads = input$threads %>% as.numeric(),
        enrich_method = input$enrich_method %>% as.character()
      )
    })
    plot1_para = reactive({
      list(
        fig1_x_axis = input$fig1_x_axis %>% as.character(),
        fig1_cutoff = input$fig1_cutoff %>% as.numeric(),
        fig1_top = input$fig1_top %>% as.numeric(),
        fig1_axis.text.x.width = input$fig1_axis.text.x.width %>% as.numeric(),
        fig1_axis.text.y.width = input$fig1_axis.text.y.width %>% as.numeric()
      )
    })
    plot2_para = reactive({
      list(
        fig2_x_axis = input$fig2_x_axis %>% as.numeric(),
        fig2_y_axis = input$fig2_y_axis %>% as.numeric(),
        fig2_point_size = input$fig2_point_size %>% as.numeric(),
        fig2_x_axis_cutoff = input$fig2_x_axis_cutoff %>% as.numeric(),
        fig2_y_axis_cutoff = input$fig2_y_axis_cutoff %>% as.numeric(),
        fig2_label = input$fig2_label %>% as.logical(),
        fig2_label_size = input$fig2_label_size %>% as.numeric()
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



    ##> step1 add ms2
    observeEvent(
      input$enrich_start,
      {
        if(prj_init$steps == "DAM and rest") {
          if (!is.null(prj_init$object_negative.init)) {
            data_enrich$object_dam = prj_init$object_negative.init
          } else if (!is.null(prj_init$object_positive.init)) {
            data_enrich$object_dam = prj_init$object_positive.init
          } else {
            return()
          }
        } else {
          if (!is.null(data_clean_rv$object_dam)) {
            data_enrich$object_dam = data_clean_rv$object_dam
          } else {
            return()
          }
        }
        para = enrich_para()
        ## > extract DAMs
        diff_metabolites = data_enrich$object_dam %>% extract_variable_info()
        if(para$hsa_db_type == "KEGG (hsa)") {
          data("kegg_hsa_pathway", package = "metpath")
          pathway_class =
            metpath::pathway_class(kegg_hsa_pathway)
          remain_idx =
            pathway_class %>%
            unlist() %>%
            stringr::str_detect("Disease") %>%
            `!`() %>%
            which()
          pathway_database =
            kegg_hsa_pathway[remain_idx]
          kegg_id <-
            diff_metabolites$KEGG.ID
          kegg_id <-
            kegg_id[!is.na(kegg_id)]
          result <-
            enrich_kegg(query_id = kegg_id,
                        query_type = "compound",
                        id_type = "KEGG",
                        pathway_database = pathway_database,
                        p_cutoff = 0.05,
                        p_adjust_method = "BH",
                        threads = 3)
        } else if(para$hsa_db_type == "HMDA (hsa)") {

        } else if(para$hsa_db_type == "Wikipedia (hsa)") {

        } else if(para$hsa_db_type == "Customized (any species)") {

        } else {
          return()
        }

        ## > bar plot
        ###> fig1 PCA =============
        output$fig1_barplot <- renderUI({
          plot_type <- input$fig1_data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_barplot.pos"))
          } else {
            plotOutput(outputId = ns("plot_barplot.pos"))
          }
        })
        output$plot_barplot.pos <- renderPlot({
          para = plot1_para()
          if(is.null(input$enrich_start)){return()}
          if(is.null(object)){return()}

          temp_obj %>%
            metid::enrich_bar_plot(
              object = data_enrich$object_dam,
              x_axis = para$fig1_x_axis,
              cutoff = para$fig1_cutoff,
              top = para$fig1_top,
              axis.text.x.width = para$axis.text.x.width,
              axis.text.y.width = para$axis.text.y.width
            )
        })
        output$plotly_barplot.pos <- renderPlotly({
          para = plot1_para()
          if(is.null(input$enrich_start)){return()}
          if(is.null(object)){return()}
          temp_barplot =
          temp_obj %>%
            metid::enrich_bar_plot(
              object = data_enrich$object_dam,
              x_axis = para$fig1_x_axis,
              cutoff = para$fig1_cutoff,
              top = para$fig1_top,
              axis.text.x.width = para$axis.text.x.width,
              axis.text.y.width = para$axis.text.y.width
            )
          temp_barplot %>% plotly::ggplotly()
        })

      }
    )

  }
  )}


