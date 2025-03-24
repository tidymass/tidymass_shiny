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
          shinyFilesButton(
            id = ns('fpa_result'),
            buttonType = "default",
            title = "load fpa_result",
            label = 'fpa_result(option)',
            class = NULL,, multiple = FALSE,
            icon = bsicons::bs_icon("files")
          ),
          tags$span(textOutput(outputId = ns("file0_path")), class = "text-wrap"),
          shinyFilesButton(
            id = ns('feature_table_marker'),
            buttonType = "default",
            title = "load feature_table_marker",
            label = 'feature_table_marker',
            class = NULL,, multiple = FALSE,
            icon = bsicons::bs_icon("files")
          ),
          tags$span(textOutput(outputId = ns("file1_path")), class = "text-wrap"),
          shinyFilesButton(
            id = ns('feature_table_all'),
            buttonType = "default",
            title = "load feature_table_all",
            label = 'feature_table_all',
            class = NULL,, multiple = FALSE,
            icon = bsicons::bs_icon("files")
          ),
          tags$span(textOutput(outputId = ns("file2_path")), class = "text-wrap"),
          shinyFilesButton(
            id = ns('metabolic_network'),
            buttonType = "default",
            title = "load metabolic_network",
            label = 'metabolic_network',
            class = NULL,, multiple = FALSE,
            icon = bsicons::bs_icon("files")
          ),
          tags$span(textOutput(outputId = ns("file3_path")), class = "text-wrap"),
          shinyFilesButton(
            id = ns('pathway_database'),
            buttonType = "default",
            title = "load pathway_database",
            label = 'pathway_database',
            class = NULL,, multiple = FALSE,
            icon = bsicons::bs_icon("files")
          ),
          tags$span(textOutput(outputId = ns("file4_path")), class = "text-wrap"),
          shinyFilesButton(
            id = ns('metabolite_database'),
            buttonType = "default",
            title = "load metabolite_database",
            label = 'metabolite_database',
            class = NULL,, multiple = FALSE,
            icon = bsicons::bs_icon("database-add")
          ),
          tags$span(textOutput(outputId = ns("file5_path")), class = "text-wrap"),
          shinyFilesButton(
            id = ns('adduct.table'),
            buttonType = "default",
            title = "load adduct.table",
            label = 'adduct.table(option)',
            class = NULL,, multiple = FALSE,
            icon = bsicons::bs_icon("files")
          ),
          tags$span(textOutput(outputId = ns("file6_path")), class = "text-wrap")
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
          )
          ),
        actionButton(inputId = ns('fpa_start'),label = "Perform FPA",icon = icon("play"))
        ),
      page_fluid(

        navset_card_tab(
          height = 400,
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
                    plotOutput(ns("network_plt"))
          )
        ),
        navset_card_tab(
          height = 400,
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
#' @import metid
#' @import plantmdb
#' @param id module of server
#' @param volumes shinyFiles volumes
#' @noRd


fpa_server <- function(id,volumes) {
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
        include_hidden_metabolites = input$fig2_include_hidden_metabolites%>% as.logical(),
        add_compound_name = input$fig2_add_compound_name %>% as.logical(),
        node_color_by_module = input$fig2_node_color_by_module %>% as.logical(),
        layout = input$fig2_layout %>% as.character()
      )
    })
    fig1_para = reactive({
      list(
        include_feature = input$fig1_include_feature %>% as.logical(),
        include_hidden_metabolites = input$fig1_include_hidden_metabolites%>% as.logical(),
        add_compound_name = input$fig1_add_compound_name %>% as.logical(),
        add_pathways = input$fig1_add_pathways %>% as.logical(),
        layout = input$fig1_layout %>% as.character(),
        metabolic_module_index = input$fig1_metabolic_module_index %>% as.numeric()
      )
    })

    observe({
      shinyFileChoose(input, "fpa_result", roots = volumes, session = session)
      if(!is.null(input$fpa_result)){
        # browser()
        temp_file_text <-parseFilePaths(roots = volumes,  input$fpa_result)
        output$file0_path <- renderText(temp_file_text$datapath)
      }})
    observe({
      shinyFileChoose(input, "feature_table_marker", roots = volumes, session = session)
      if(!is.null(input$feature_table_marker)){
        # browser()
        temp_file_text <-parseFilePaths(roots = volumes,  input$feature_table_marker)
        output$file1_path <- renderText(temp_file_text$datapath)
      }})
    observe({
      shinyFileChoose(input, "feature_table_all", roots = volumes, session = session)
      if(!is.null(input$feature_table_all)){
        # browser()
        temp_file_text <-parseFilePaths(roots = volumes,  input$feature_table_all)
        output$file2_path <- renderText(temp_file_text$datapath)
      }})
    observe({
      shinyFileChoose(input, "metabolic_network", roots = volumes, session = session)
      if(!is.null(input$metabolic_network)){
        # browser()
        temp_file_text <-parseFilePaths(roots = volumes,  input$metabolic_network)
        output$file3_path <- renderText(temp_file_text$datapath)
      }})
    observe({
      shinyFileChoose(input, "pathway_database", roots = volumes, session = session)
      if(!is.null(input$pathway_database)){
        # browser()
        temp_file_text <-parseFilePaths(roots = volumes,  input$pathway_database)
        output$file4_path <- renderText(temp_file_text$datapath)
      }})
    observe({
      shinyFileChoose(input, "metabolite_database", roots = volumes, session = session)
      if(!is.null(input$metabolite_database)){
        # browser()
        temp_file_text <-parseFilePaths(roots = volumes,  input$metabolite_database)
        output$file5_path <- renderText(temp_file_text$datapath)
      }})
    observe({
      shinyFileChoose(input, "adduct.table", roots = volumes, session = session)
      if(!is.null(input$adduct.table)){
        # browser()
        temp_file_text <-parseFilePaths(roots = volumes,  input$adduct.table)
        output$file6_path <- renderText(temp_file_text$datapath)
      }})

    observeEvent(
      input$fpa_start,{
        temp_file_text <-parseFilePaths(roots = volumes,  input$fpa_result)
        fpa_result_path <- as.character(temp_file_text$datapath)
        print(fpa_result_path)
        temp_file_text <-parseFilePaths(roots = volumes,  input$feature_table_marker)
        feature_table_marker_path <- as.character(temp_file_text$datapath)
        print(feature_table_marker_path)
        if(str_detect(fpa_result_path,"\\.rda") & str_detect(feature_table_marker_path,"\\.rda")) {
          fpa_result = load(file = fpa_result_path)
          fpa_values$fpa_result = get(fpa_result)
          feature_table_marker= load(file = feature_table_marker_path)
          fpa_values$feature_table_marker = get(feature_table_marker)

          print((fpa_values$fpa_result)[[2]])
        }
        temp_file_text <-parseFilePaths(roots = volumes,  input$metabolic_network)
        metabolic_network_path <- as.character(temp_file_text$datapath)
        temp_file_text <-parseFilePaths(roots = volumes,  input$pathway_database)
        pathway_database_path <- as.character(temp_file_text$datapath)
        temp_file_text <-parseFilePaths(roots = volumes,  input$metabolite_database)
        metabolite_database_path <- as.character(temp_file_text$datapath)
        temp_file_text <-parseFilePaths(roots = volumes,  input$adduct.table)
        adduct.table_path <- as.character(temp_file_text$datapath)

        if(length(fpa_result_path) == 0 &
           length(feature_table_marker_path) > 0 &
           length(metabolic_network_path) > 0 &
           length(pathway_database_path) > 0 &
           length(metabolite_database_path) > 0) {
          fpa_values$feature_table_marker = load(file = feature_table_marker_path)
          metabolic_network= load(file = metabolic_network_path)
          pathway_database = load(file = pathway_database_path)
          metabolite_database_path = load(file = metabolite_database_path)
          if(length(adduct.table_path) == 0){
            adduct.table = NULL
          } else {
            adduct.table = load(adduct.table_path)
          }
          pro_enrichment_step1 = c(
            'running FPA analysis ...',
            'All finish'
          )
          para = fpa_para()
          fpa_values$fpa_result = metpath::perform_fpa(
            feature_table_marker = fpa_values$feature_table_marker,
            feature_table_all = feature_table_all,
            metabolite_database = metabolite_database,
            column = para$column,
            adduct.table = adduct.table,
            ms1.match.ppm = para$ms1.match.ppm,
            rt.match.tol = para$rt.match.tol,
            mz.ppm.thr = para$mz.ppm.thr,
            threads = para$threads,
            include_hidden_metabolites = para$include_hidden_metabolites,
            metabolic_network = metabolic_network,
            pathway_database =pathway_database
          )
        } else {return()}
      }
    )

    observeEvent(
      input$show_network_plot,{
        if (is.null(fpa_values$fpa_result) | is.null(fpa_values$feature_table_marker)){
          return()
        } else {

          output$network_plt = renderPlot({
            para = fig2_para()
            metpath::plot_metabolic_network_fpa(
              fpa_result = fpa_values$fpa_result,
              feature_table_marker = fpa_values$feature_table_marker,
              include_feature = para$include_feature,
              node_color_by_module = para$node_color_by_module,
              include_hidden_metabolites = para$include_hidden_metabolites,
              add_compound_name = para$add_compound_name,
              layout = para$layout
            )
          }
          )

        }
      }
    )

    observeEvent(
      input$show_module_plot,{
        if (is.null(fpa_values$fpa_result) | is.null(fpa_values$feature_table_marker)){
          return()
        } else {

          output$module_plt = renderPlot({
            para = fig1_para()
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
          }
          )

        }
      }
    )

  }
  )}

