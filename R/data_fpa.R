#' Feature-based Pathway Analysis
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


fpa_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Feature-based Pathway Analysis (FPA)',
    icon = bs_icon("diagram-3"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Data upload",
          icon = bsicons::bs_icon("upload"),
          fileInput(
            inputId = ns("fpa_result"),
            label = tooltip(
              trigger = list(
                "fpa_result(option)",
                bsicons::bs_icon("info-circle")
              ),
              "Saved feature-based pathway analysis (FPA) result. For result visualize"
            ),
            multiple = FALSE,
            buttonLabel = "Browse...",
            placeholder = "No file selected",
            accept = ".rda"
          ),
          fileInput(
            inputId = ns("feature_table_marker"),
            label = tooltip(
              trigger = list(
                "feature table marker",
                bsicons::bs_icon("info-circle")
              ),
              "A data frame of marker features and saved as .rda format."
            ),
            multiple = FALSE,
            buttonLabel = "Browse...",
            placeholder = "No file selected",
            accept = ".rda"
          ),
          fileInput(
            inputId = ns("feature_table_all"),
            label = tooltip(
              trigger = list(
                "feature table all",
                bsicons::bs_icon("info-circle")
              ),
              "A data frame containing all features and saved as .rda format."
            ),
            multiple = FALSE,
            buttonLabel = "Browse...",
            placeholder = "No file selected",
            accept = ".rda"
          ),
          fileInput(
            inputId = ns("metabolite_database"),
            label = tooltip(
              trigger = list(
                "metabolite database",
                bsicons::bs_icon("info-circle")
              ),
              "A metabolite database object containing metabolite spectra and saved as .rda format."
            ),
            multiple = FALSE,
            buttonLabel = "Browse...",
            placeholder = "No file selected",
            accept = ".rda"
          ),
          fileInput(
            inputId = ns("metabolic_network"),
            label = tooltip(
              trigger = list(
                "metabolic network",
                bsicons::bs_icon("info-circle")
              ),
              "A metabolic network object representing metabolic reactions and saved as .rda format."
            ),
            multiple = FALSE,
            buttonLabel = "Browse...",
            placeholder = "No file selected",
            accept = ".rda"
          ),
          fileInput(
            inputId = ns("pathway_database"),
            label = tooltip(
              trigger = list(
                "pathway database",
                bsicons::bs_icon("info-circle")
              ),
              "A pathway database object containing metabolic pathways and saved as .rda format."
            ),
            multiple = FALSE,
            buttonLabel = "Browse...",
            placeholder = "No file selected",
            accept = ".rda"
          ),
          fileInput(
            inputId = ns("adduct.table"),
            label = tooltip(
              trigger = list(
                "adduct table (option)",
                bsicons::bs_icon("info-circle")
              ),
              "A adducut table and saved as .rda format."
            ),
            multiple = FALSE,
            buttonLabel = "Browse...",
            placeholder = "No file selected",
            accept = ".rda"
          )
        ),
        accordion_panel(
          title = "Set parameters",
          icon = bsicons::bs_icon("gear"),
          selectInput(
            inputId = ns("fpa_column"),
            label = tooltip(
              trigger = list(
                "column",
                bsicons::bs_icon("info-circle")
              ),
              "rp: reverse phase \nhilic: HILIC column"
            ),choices = c("rp","hilic"), selected = "rp"
          ),
          textInput(
            inputId = ns("fpa_ms1.match.ppm"),
            label = tooltip(
              trigger = list(
                "ms1.match.ppm",
                bsicons::bs_icon("info-circle")
              ),
              "mass tolerance for MS1 matching in parts per million (ppm). Default is '25'"
            ),value = 25
          ),
          textInput(
            inputId = ns("fpa_rt.match.tol"),
            label = tooltip(
              trigger = list(
                "rt.match.tol",
                bsicons::bs_icon("info-circle")
              ),
              "retention time tolerance threshold in seconds. Default is '5'"
            ),value = 5
          ),
          textInput(
            inputId = ns("fpa_mz.ppm.thr"),
            label = tooltip(
              trigger = list(
                "mz.ppm.thr",
                bsicons::bs_icon("info-circle")
              ),
              "M/Z tolerance threshold for filtering in ppm. Default is '400'."
            ),value = 400
          ),
          textInput(
            inputId = ns("fpa_threads"),
            label = tooltip(
              trigger = list(
                "threads",
                bsicons::bs_icon("info-circle")
              ),
              "number of threads to use for parallel processing. Default is '3'"
            ),value = 3
          ),
          radioButtons(
            inputId = ns("fpa_include_hidden_metabolites"),
            label = tooltip(
              trigger = list(
                "include_hidden_metabolites",
                bsicons::bs_icon("info-circle")
              ),
              "whether to include hidden metabolites. Default is 'FALSE'"
            ),choices = c("TRUE","FALSE"),select = "FALSE"
          ),
          actionButton(inputId = ns('fpa_start'),label = "Perform FPA",icon = icon("play"))
          )
        ),
      page_fluid(
        tags$head(
          tags$style(HTML("
            .progress-modal .modal-dialog {
              max-width: 400px;
            }
            .progress-modal .modal-title {
              display: flex;
              align-items: center;
            }
            .fa-spinner {
              margin-right: 10px;
              animation: fa-spin 2s infinite linear;
            }
          "))),

        navset_card_tab(
          height = 500,
          full_screen = T,
          title = "Dysregulated metabolic network",
          sidebar = accordion(
            open = FALSE,
            accordion_panel(
              title = 'Parameters',
              radioButtons(
                inputId = ns('fig2_include_feature'), label = tooltip(
                  trigger = list(
                    "include_feature",
                    bsicons::bs_icon("info-circle")
                  ),
                  "Logical. Whether to include detected metabolic features in the plot. Default is 'FALSE'."
                ), choices = c("TRUE", "FALSE"),selected = 'FALSE'
              ),
              radioButtons(
                inputId = ns('fig2_include_hidden_metabolites'), label = tooltip(
                  trigger = list(
                    "include_hidden_metabolites",
                    bsicons::bs_icon("info-circle")
                  ),
                  "Logical. Whether to include hidden metabolites in the plot. Default is 'FALSE'."
                ), choices = c("TRUE", "FALSE"),selected = 'FALSE'
              ),
              radioButtons(
                inputId = ns('fig2_add_compound_name'), label = tooltip(
                  trigger = list(
                    "add_compound_name",
                    bsicons::bs_icon("info-circle")
                  ),
                  "Logical. Whether to add compound names as labels in the visualization. Default is 'TRUE'."
                ), choices = c("TRUE", "FALSE"),selected = 'FALSE'
              ),
              radioButtons(
                inputId = ns('fig2_node_color_by_module'), label = tooltip(
                  trigger = list(
                    "node_color_by_module",
                    bsicons::bs_icon("info-circle")
                  ),
                  "Add pathways beside of the network or not. Default is 'FALSE'."
                ), choices = c("TRUE", "FALSE"),selected = 'FALSE'
              ),
              textInput(
                inputId = ns('fig2_layout'),label = tooltip(
                  trigger = list(
                    "layout",
                    bsicons::bs_icon("info-circle")
                  ),
                  "The layout of the network, such as 'kk' or 'fr'."
                ),value = "fr"
              ),

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
          nav_panel("Dysregulated metabolic network",
                    actionButton(inputId = ns("show_network_plot"),label = "Show network",width = "20%"),
                    downloadButton(outputId = ns("download_res"),label = "Download fpa result",style = "width:20%",icon = icon("download")),
                    plotOutput(ns("network_plt"))
          )
        ),
        navset_card_tab(
          height = 500,
          full_screen = T,
          title = "Module network",
          sidebar = accordion(
            open = FALSE,
            accordion_panel(
              title = 'Parameters',
              radioButtons(
                inputId = ns('fig1_include_feature'), label = tooltip(
                  trigger = list(
                    "include_feature",
                    bsicons::bs_icon("info-circle")
                  ),
                  "Logical. Whether to include detected metabolic features in the plot. Default is 'FALSE'."
                ), choices = c("TRUE", "FALSE"),selected = 'FALSE'
              ),
              radioButtons(
                inputId = ns('fig1_include_hidden_metabolites'), label = tooltip(
                  trigger = list(
                    "include_hidden_metabolites",
                    bsicons::bs_icon("info-circle")
                  ),
                  "Logical. Whether to include hidden metabolites in the plot. Default is 'FALSE'."
                ), choices = c("TRUE", "FALSE"),selected = 'FALSE'
              ),
              radioButtons(
                inputId = ns('fig1_add_compound_name'), label = tooltip(
                  trigger = list(
                    "add_compound_name",
                    bsicons::bs_icon("info-circle")
                  ),
                  "Logical. Whether to add compound names as labels in the visualization. Default is 'TRUE'."
                ), choices = c("TRUE", "FALSE"),selected = 'FALSE'
              ),
              radioButtons(
                inputId = ns('fig1_add_pathways'), label = tooltip(
                  trigger = list(
                    "add_pathways",
                    bsicons::bs_icon("info-circle")
                  ),
                  "Add pathways beside of the network or not. Default is 'FALSE'."
                ), choices = c("TRUE", "FALSE"),selected = 'FALSE'
              ),
              textInput(
                inputId = ns('fig1_layout'),label = tooltip(
                  trigger = list(
                    "layout",
                    bsicons::bs_icon("info-circle")
                  ),
                  "The layout of the network, such as 'kk' or 'fr'."
                ),value = "fr"
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
          nav_panel("Module network",
            actionButton(inputId = ns("show_module_plot"),label = "Show module network",width = "20%"),
            textInput(
              inputId = ns('fig1_metabolic_module_index'),label = tooltip(
                trigger = list(
                  "metabolic_module_index",
                  bsicons::bs_icon("info-circle")
                ),
                "The layout of the network, such as 'kk' or 'fr'."
              ),value = 1
            ),
            plotOutput(ns("module_plt"))
            )
          )
        )
      )
    )
}


#' Feature-based Pathway Analysis
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
#' @noRd


fpa_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    fpa_values <- reactiveValues(data = NULL)

    fpa_para = reactive({
      list(
        column = input$fpa_column %>% as.character(),
        ms1.match.ppm = input$fpa_ms1.match.ppm %>% as.numeric(),
        rt.match.tol = input$fpa_rt.match.tol %>% as.numeric(),
        mz.ppm.thr = input$fpa_mz.ppm.thr %>% as.numeric(),
        threads = input$fpa_threads %>% as.numeric(),
        include_hidden_metabolites = input$fpa_include_hidden_metabolites %>% as.logical()
      )
    })

    fig2_para = reactive({
      list(
        include_feature = input$fig2_include_feature %>% as.logical(),
        include_hidden_metabolites = input$fig2_include_hidden_metabolites %>% as.logical(),
        add_compound_name = input$fig2_add_compound_name %>% as.logical(),
        node_color_by_module = input$fig2_node_color_by_module %>% as.logical(),
        layout = input$fig2_layout %>% as.character()
      )
    })

    fig1_para = reactive({
      list(
        include_feature = input$fig1_include_feature %>% as.logical(),
        include_hidden_metabolites = input$fig1_include_hidden_metabolites %>% as.logical(),
        add_compound_name = input$fig1_add_compound_name %>% as.logical(),
        add_pathways = input$fig1_add_pathways %>% as.logical(),
        layout = input$fig1_layout %>% as.character(),
        metabolic_module_index = input$fig1_metabolic_module_index %>% as.numeric()
      )
    })

    observeEvent(input$fpa_start, {
      req(input$feature_table_marker)
      req(input$feature_table_all)
      req(input$metabolic_network)
      req(input$pathway_database)
      req(input$metabolite_database)

      tryCatch({
        # Show modal dialog with spinner and message
        showModal(modalDialog(
          title = tags$div(
            tags$i(class = "fa fa-spinner fa-spin"),
            "Performing FPA"
          ),
          "This may take a long time (20-30 minutes). Please be patient.",
          easyClose = FALSE,
          footer = NULL
        ))

        fpa_values$feature_table_marker <- load_rdata(input$feature_table_marker$datapath)
        fpa_values$metabolic_network <- load_rdata(input$metabolic_network$datapath)
        fpa_values$pathway_database <- load_rdata(input$pathway_database$datapath)
        fpa_values$metabolite_database <- load_rdata(input$metabolite_database$datapath)
        fpa_values$feature_table_all <- load_rdata(input$feature_table_all$datapath)
        fpa_values$adduct.table <- if (!is.null(input$adduct.table)) {
          load_rdata(input$adduct.table$datapath)
        }

        if (!is.null(input$fpa_result)) {
          fpa_values$fpa_result <- load_rdata(input$fpa_result$datapath)
        } else {
          para <- fpa_para()
          fpa_values$fpa_result <- metpath::perform_fpa(
            feature_table_marker = fpa_values$feature_table_marker,
            feature_table_all = fpa_values$feature_table_all,
            metabolite_database = fpa_values$metabolite_database,
            column = para$column,
            adduct.table = fpa_values$adduct.table,
            ms1.match.ppm = para$ms1.match.ppm,
            rt.match.tol = para$rt.match.tol,
            mz.ppm.thr = para$mz.ppm.thr,
            threads = para$threads,
            include_hidden_metabolites = para$include_hidden_metabolites,
            metabolic_network = fpa_values$metabolic_network,
            pathway_database = fpa_values$pathway_database
          )
        }

        # Remove modal after computation
        removeModal()

      }, error = function(e) {
        removeModal()
        shinyalert("Error", e$message, type = "error")
      })
    })

    output$download_res <- downloadHandler(
      filename = function() {
        "fpa_analysis_result.rda"
      },
      content = function(file) {
        req(fpa_values$fpa_result)
        fpa_result <- fpa_values$fpa_result
        save(list = fpa_result, file = file)
      }
    )

    observeEvent(input$show_network_plot, {
      req(fpa_values$fpa_result)
      req(fpa_values$feature_table_marker)

      tryCatch({
        para <- fig2_para()
        fpa_values$network_plot <- metpath::plot_metabolic_network_fpa(
          fpa_result = fpa_values$fpa_result,
          feature_table_marker = fpa_values$feature_table_marker,
          include_feature = para$include_feature,
          node_color_by_module = para$node_color_by_module,
          include_hidden_metabolites = para$include_hidden_metabolites,
          add_compound_name = para$add_compound_name,
          layout = para$layout
        )

        output$network_plt <- renderPlot({
          para <- fig2_para()
          metpath::plot_metabolic_network_fpa(
            fpa_result = fpa_values$fpa_result,
            feature_table_marker = fpa_values$feature_table_marker,
            include_feature = para$include_feature,
            node_color_by_module = para$node_color_by_module,
            include_hidden_metabolites = para$include_hidden_metabolites,
            add_compound_name = para$add_compound_name,
            layout = para$layout
          )

        })
      }, error = function(e) {
        shinyalert("Plot Error", e$message, type = "error")
      })
    })

    observeEvent(input$show_module_plot, {
      req(fpa_values$fpa_result)
      req(fpa_values$feature_table_marker)

      tryCatch({

        para <- fig1_para()
        fpa_values$module_plot <- metpath::plot_metabolic_module_fpa(
          fpa_result = fpa_values$fpa_result,
          feature_table_marker = fpa_values$feature_table_marker,
          include_feature = para$include_feature,
          add_pathways = para$add_pathways,
          metabolic_module_index = para$metabolic_module_index,
          include_hidden_metabolites = para$include_hidden_metabolites,
          add_compound_name = para$add_compound_name,
          layout = para$layout
        )

        output$module_plt <- renderPlot({
          para <- fig1_para()
          metpath::plot_metabolic_module_fpa(
            fpa_result = fpa_values$fpa_result,
            feature_table_marker = fpa_values$feature_table_marker,
            include_feature = para$include_feature,
            add_pathways = para$add_pathways,
            metabolic_module_index = para$metabolic_module_index,
            include_hidden_metabolites = para$include_hidden_metabolites,
            add_compound_name = para$add_compound_name,
            layout = para$layout
          )
        })
      }, error = function(e) {
        shinyalert("Plot Error", e$message, type = "error")
      })
    })

    # Download handlers for plots
    output$fig2_download <- downloadHandler(
      filename = function() {
        paste("network_plot", input$fig2_format, sep = ".")
      },
      content = function(file) {
        ggsave(file, plot = fpa_values$network_plot, device = input$fig2_format,
               height = as.numeric(input$fig2_height), width = as.numeric(input$fig2_width))
      }
    )

    output$fig1_download <- downloadHandler(
      filename = function() {
        paste("module_plot", input$fig1_format, sep = ".")
      },
      content = function(file) {
        ggsave(file, plot = fpa_values$module_plot, device = input$fig1_format,
               height = as.numeric(input$fig1_height), width = as.numeric(input$fig1_width))
      }
    )

    # Enable/disable download buttons based on plot availability
    observe({
      if (is.null(fpa_values$network_plot)) {
        shinyjs::disable("fig2_download")
      } else {
        shinyjs::enable("fig2_download")
      }
    })

    observe({
      if (is.null(fpa_values$module_plot)) {
        shinyjs::disable("fig1_download")
      } else {
        shinyjs::enable("fig1_download")
      }
    })
  })
}
