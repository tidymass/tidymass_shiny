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
            choices = c("KEGG (hsa)", "HMDB (hsa)", "Customized (any species)"),
            selected = "KEGG (hsa)"
          )
        ),
        accordion_panel(
          id = ns("upload_panel"),
          title = "Upload custom database",
          icon = bsicons::bs_icon("folder"),
          fileInput(
            inputId = ns("cuz_pathway_database"),
            label = tooltip(
              trigger = list(
                "File upload (option)",
                bsicons::bs_icon("info-circle")
              ),
              "Pathway database such kegg"
            ),
            multiple = FALSE,
            buttonLabel = "Browse...",
            placeholder = "No file selected",
            accept = ".rda"
          ),
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
            value = 1,
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
          height = 500,
          full_screen = TRUE,
          layout_columns(
            col_widths = c(5,2,5),
            dataTableOutput(ns('enrich_tbl')),
            actionButton(inputId = ns("show_detail"),"Compound information"),
            dataTableOutput(ns("Compound_detail"))
          )
        ),
        layout_column_wrap(
          width = 1/2,
          height = 500,
          navset_card_tab(
            height = 500,
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
            height = 500,
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
              uiOutput(ns("fig2_scatter_plt"),fill = T)
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
#' @param id module of server
#' @param volumes shinyFiles volumes
#' @param prj_init use project init variables.
#' @param data_import_rv reactivevalues mass_dataset export
#' @param data_clean_rv reactivevalues p2 dataclean
#' @param data_enrich reactivevalues data annotation
#' @noRd


enrichment_server <- function(id,volumes,prj_init,data_import_rv,data_clean_rv,data_enrich) {
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
        fig2_x_axis = input$fig2_x_axis %>% as.character(),
        fig2_y_axis = input$fig2_y_axis %>% as.character(),
        fig2_point_size = input$fig2_point_size %>% as.character(),
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
        if (!is.null(data_clean_rv$object_dam)) {
          data_enrich$object_dam = data_clean_rv$object_dam
        }
        ## > extract DAMs


        para = enrich_para()
        pro_enrichment_step1 = c(
          'running enrichment analysis ...',
          'All finish'
        )
        withProgress(
          message = 'Run enrichment analysis',value = 0,
          expr = {
            for (i in 1:2) {
              incProgress(1/2,detail = pro_enrichment_step1[i])
              if(i == 1) {

                if(para$hsa_db_type == "KEGG (hsa)") {
                  data("kegg_hsa_pathway", package = "metpath")
                  diff_metabolites = data_enrich$object_dam %>% extract_variable_info()
                  data_enrich$diff_metabolites <- diff_metabolites
                  pathway_database = kegg_hsa_pathway
                  kegg_id <-
                    diff_metabolites$KEGG.ID
                  kegg_id <-
                    kegg_id[!is.na(kegg_id)]
                  res = enrich_pathways(query_id = kegg_id,query_type = "compound",
                                  id_type = "KEGG",
                                  pathway_database = pathway_database,
                                  p_cutoff = 0.05,
                                  p_adjust_method = para$p_adjust_method,
                                  method = para$enrich_method,
                                  threads = para$threads)
                  data_enrich$res <- res

                } else if(para$hsa_db_type == "HMDB (hsa)") {
                  data("hmdb_pathway", package = "metpath")
                  temp_diff <- data_enrich$object_dam %>% extract_variable_info()
                  anno_diff <- data_enrich$object_dam %>% extract_annotation_table() %>%
                    group_by(variable_id) %>% slice_head(n = 1) %>% ungroup() %>%
                    dplyr::select(variable_id,HMDB.ID)
                  diff_metabolites =  left_join(temp_diff,anno_diff)

                  data_enrich$diff_metabolites <- diff_metabolites
                  pathway_database = hmdb_pathway
                  hmdb_id <-
                    diff_metabolites$HMDB.ID
                  hmdb_id <-
                    hmdb_id[!is.na(hmdb_id)]
                  res = enrich_pathways(query_id = hmdb_id,query_type = "compound",
                                        id_type = "HMDB",
                                        pathway_database = pathway_database,
                                        p_cutoff = 0.05,
                                        p_adjust_method = para$p_adjust_method,
                                        method = para$enrich_method,
                                        threads = para$threads)
                  data_enrich$res <- res

                } else if(para$hsa_db_type == "Customized (any species)") {
                  pathway_database <- load_rdata(path = input$cuz_pathway_database$datapath)
                  diff_metabolites = data_enrich$object_dam %>% extract_variable_info()
                  data_enrich$diff_metabolites <- diff_metabolites
                  kegg_id <-
                    diff_metabolites$KEGG.ID
                  kegg_id <-
                    kegg_id[!is.na(kegg_id)]
                  res = enrich_pathways(query_id = kegg_id,query_type = "compound",
                                        id_type = "KEGG",
                                        pathway_database = pathway_database,
                                        p_cutoff = 0.05,
                                        p_adjust_method = para$p_adjust_method,
                                        method = para$enrich_method,
                                        threads = para$threads)
                  data_enrich$res <- res



                } else {
                  return()
                }
              } else if (i == 2) {
                print("Finish")
              }
            }
          }
        )


        data_enrich$enrich_tbl = res@result %>% as.data.frame() %>%
          dplyr::filter(p_value < 0.05)
        if (nrow(data_enrich$enrich_tbl) == 0) {
          shinyalert(title = "Error",text = "No significant enriched pathway", type = "warning")
          return()
        }

        output$enrich_tbl = renderDataTable_formated(
          actions = input$enrich_start,
          condition1 = data_enrich$enrich_tbl,filename.a = "Enrichment_result.xlsx",
          tbl = data_enrich$enrich_tbl
        )

        ## > bar plot
        ###> fig1 barpolt =============
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
          if(is.null(res)){return()}
          metpath::enrich_bar_plot(
              object = res,
              x_axis = para$fig1_x_axis,
              cutoff = para$fig1_cutoff,
              top = para$fig1_top,
              axis.text.x.width = para$fig1_axis.text.x.width,
              axis.text.y.width = para$fig1_axis.text.y.width
            )
        })
        output$plotly_barplot.pos <- renderPlotly({
          para = plot1_para()
          if(is.null(input$enrich_start)){return()}
          if(is.null(res)){return()}
          temp_barplot =
            metpath::enrich_bar_plot(
              object = res,
              x_axis = para$fig1_x_axis,
              cutoff = para$fig1_cutoff,
              top = para$fig1_top,
              axis.text.x.width = para$fig1_axis.text.x.width,
              axis.text.y.width = para$fig1_axis.text.y.width
            )
          temp_barplot %>% plotly::ggplotly()
        })

        ## > bar plot
        ###> fig2 scatter plot =============
        output$fig2_scatter_plt <- renderUI({
          plot_type <- input$fig2_data_clean_plt_format
          if (plot_type) {
            plotlyOutput(outputId = ns("plotly_scatter_plt"))
          } else {
            plotOutput(outputId = ns("scatter_plt"))
          }
        })



        output$scatter_plt <- renderPlot({
          para = plot2_para()
          if(is.null(input$enrich_start)){return()}
          if(is.null(res)){return()}
          metpath::enrich_scatter_plot(
            object = res,
            x_axis = para$fig2_x_axis,
            y_axis = para$fig2_y_axis,
            point_size = para$fig2_point_size,
            x_axis_cutoff = para$fig2_x_axis_cutoff,
            y_axis_cutoff = para$fig2_y_axis_cutoff,
            label = para$fig2_label,
            label_size = para$fig2_label_size
          )
        })
        output$plotly_scatter_plt <- renderPlotly({
          para = plot2_para()
          if(is.null(input$enrich_start)){return()}
          if(is.null(res)){return()}
          temp_scatter_plot =
            metpath::enrich_scatter_plot(
              object = res,
              x_axis = para$fig2_x_axis,
              y_axis = para$fig2_y_axis,
              point_size = para$fig2_point_size,
              x_axis_cutoff = para$fig2_x_axis_cutoff,
              y_axis_cutoff = para$fig2_y_axis_cutoff,
              label = para$fig2_label,
              label_size = para$fig2_label_size
            )
          temp_scatter_plot %>% plotly::ggplotly()
        })



      }
    )

    observeEvent(input$show_detail,{

      enrich_row_idx = input$enrich_tbl_rows_selected
      print(enrich_row_idx)
      enrich_row = data_enrich$enrich_tbl[enrich_row_idx, ] %>%
        pull(mapped_id) %>% stringr::str_split(pattern = "\\;",n = Inf) %>% unlist()
      print(enrich_row)
      temp_pathway = data_enrich$enrich_tbl[enrich_row_idx, ] %>%
        pull(pathway_name)
      print(temp_pathway)
      select_idx = data_enrich$diff_metabolites %>%
        dplyr::filter(KEGG.ID %in% enrich_row) %>%
        dplyr::select(variable_id,mz,rt,Compound.name,fc,p_value,p_value_adjust) %>%
        left_join(data_enrich$object_dam %>% extract_expression_data() %>%
                    rownames_to_column('variable_id'),by = "variable_id" )
      print(select_idx)
      output$Compound_detail = renderDataTable_formated(
        actions = input$show_detail,
        filename.a = paste0(temp_pathway,"DAMs_detail.xls"),
        tbl = select_idx
      )
    }
    )



    # At the top level of the server function, add the following download handlers:

    output$fig1_download <- downloadHandler(
      filename = function() {
        dp <- download_para()
        format <- dp$fig1_format
        paste0("Enrichment_barplot.", format)
      },
      content = function(file) {
        if (is.null(data_enrich$res)) {
          stop("No enrichment results available. Please run the enrichment analysis first.")
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
        p <- metpath::enrich_bar_plot(
          object = data_enrich$res,
          x_axis = para$fig1_x_axis,
          cutoff = para$fig1_cutoff,
          top = para$fig1_top,
          axis.text.x.width = para$fig1_axis.text.x.width,
          axis.text.y.width = para$fig1_axis.text.y.width
        )

        print(p)
        dev.off()
      }
    )

    output$fig2_download <- downloadHandler(
      filename = function() {
        dp <- download_para()
        format <- dp$fig2_format
        paste0("Enrichment_scatterplot.", format)
      },
      content = function(file) {
        if (is.null(data_enrich$res)) {
          stop("No enrichment results available. Please run the enrichment analysis first.")
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

        para <- plot2_para()
        p <- metpath::enrich_scatter_plot(
          object = data_enrich$res,
          x_axis = para$fig2_x_axis,
          y_axis = para$fig2_y_axis,
          point_size = para$fig2_point_size,
          x_axis_cutoff = para$fig2_x_axis_cutoff,
          y_axis_cutoff = para$fig2_y_axis_cutoff,
          label = para$fig2_label,
          label_size = para$fig2_label_size
        )

        print(p)
        dev.off()
      }
    )

  }
  )}


