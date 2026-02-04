#' Feature-based Metabolite Set Enrichment Analysis (fMSEA)
#' @import shiny
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @importFrom shinyFiles shinyFilesButton parseFilePaths
#' @noRd
fmsea_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'fMSEA Analysis',
    icon = bsicons::bs_icon("diagram-3"),
    layout_sidebar(
      sidebar = accordion(
        open = c("Data Upload", "Step 1: Annotation", "Step 2: Analysis"),

        # --- 1. Data Upload ---
        accordion_panel(
          title = "Data Upload",
          icon = bsicons::bs_icon("upload"),
          shinyFiles::shinyFilesButton(id = ns('feature_table_file'), label = 'Feature Table', title = "Select", multiple = FALSE, buttonType = "default"),
          div(textOutput(ns("feature_table_path")), style = "font-size: 0.75em; color: grey; margin-bottom: 5px;"),

          shinyFiles::shinyFilesButton(id = ns('ms1_db_file'), label = 'MS1 DB', title = "Select", multiple = FALSE, buttonType = "default"),
          div(textOutput(ns("ms1_db_path")), style = "font-size: 0.75em; color: grey; margin-bottom: 5px;"),

          shinyFiles::shinyFilesButton(id = ns('pathway_db_file'), label = 'Pathway DB', title = "Select", multiple = FALSE, buttonType = "default"),
          div(textOutput(ns("pathway_db_path")), style = "font-size: 0.75em; color: grey;"),

          hr(),
          p("Existing Results:", style = "font-size: 0.85em; font-weight: bold; margin-bottom: 5px;"),
          shinyFiles::shinyFilesButton(id = ns('results_rda_file'), label = 'Load results.rda', title = "Select RDA", multiple = FALSE, buttonType = "info"),
          div(textOutput(ns("results_rda_path")), style = "font-size: 0.75em; color: grey;")
        ),

        # --- 2. Step 1 Parameters ---
        accordion_panel(
          title = "Step 1: Annotation",
          icon = bsicons::bs_icon("1-circle"),
          selectInput(ns("column"), "Column", choices = c("rp", "hilic"), selected = "rp"),
          selectInput(ns("database_type"), "DB Type", choices = c("KEGG", "HMDB"), selected = "KEGG"),
          numericInput(ns("ms1_match_ppm"), "MS1 PPM", value = 15),
          numericInput(ns("mfc_rt_tol"), "RT Tol (s)", value = 10),
          numericInput(ns("isotope_number"), "Isotope No.", value = 3),
          actionButton(ns("run_step1"), "Run Step 1", class = "btn-primary", width = "100%"),
          verbatimTextOutput(ns("step1_log"))
        ),

        # --- 3. Step 2 Parameters ---
        accordion_panel(
          title = "Step 2: fMSEA",
          icon = bsicons::bs_icon("2-circle"),
          numericInput(ns("threads"), "Threads", value = 3, min = 1),
          numericInput(ns("min_compounds"), "Min Compounds", value = 15),
          numericInput(ns("max_compounds"), "Max Compounds", value = 300),
          numericInput(ns("perm_num"), "Permutations", value = 1000),
          numericInput(ns("fdr_thr"), "FDR Thr", value = 0.05),
          actionButton(ns("run_step2"), "Run Step 2", class = "btn-success", width = "100%")
        )
      ),

      card(
        full_screen = TRUE,
        card_header(
          div(class = "d-flex justify-content-between align-items-center",
              "Analysis Results",
              downloadButton(ns("download_table"), "Download Table (CSV)", class = "btn-sm"))
        ),
        card_body(
          padding = 0, # Remove padding to reduce white space
          # Table Section
          div(
            style = "padding: 10px; border-bottom: 1px solid #eee;",
            h6("Significant Modules (Select to visualize)"),
            DT::dataTableOutput(ns("sig_modules_table"))
          ),

          # Visualization & Download Section
          div(
            style = "padding: 10px;",
            h6("Enrichment Plot"),
            plotOutput(ns("fmsea_plot"), height = "400px"),
            div(
              style = "display: flex; gap: 10px; margin-top: 5px;",
              downloadButton(ns("download_png"), "PNG", class = "btn-sm"),
              downloadButton(ns("download_pdf"), "PDF", class = "btn-sm")
            )
          ),

          accordion(
            open = FALSE,
            accordion_panel("Detailed Summary", verbatimTextOutput(ns("result_summary")))
          )
        )
      )
    )
  )
}

#' fMSEA Server
#' @noRd
fmsea_server <- function(id, volumes) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    vals <- reactiveValues(feature_table = NULL, ms1_db = NULL, pathway_db = NULL, final_result = NULL)

    load_rda_data <- function(path) {
      if (is.null(path) || length(path) == 0 || path == "") return(NULL)
      env <- new.env()
      name <- load(path, envir = env)
      return(get(name, envir = env))
    }

    # --- Secure File Handlers ---
    observe({
      shinyFiles::shinyFileChoose(input, "feature_table_file", roots = volumes, session = session)
      req(input$feature_table_file)
      file_info <- shinyFiles::parseFilePaths(volumes, input$feature_table_file)
      if (nrow(file_info) > 0) {
        path <- as.character(file_info$datapath)
        vals$feature_table <- load_rda_data(path)
        output$feature_table_path <- renderText(path)
      }
    })

    observe({
      shinyFiles::shinyFileChoose(input, "ms1_db_file", roots = volumes, session = session)
      req(input$ms1_db_file)
      file_info <- shinyFiles::parseFilePaths(volumes, input$ms1_db_file)
      if (nrow(file_info) > 0) {
        path <- as.character(file_info$datapath)
        vals$ms1_db <- load_rda_data(path)
        output$ms1_db_path <- renderText(path)
      }
    })

    observe({
      shinyFiles::shinyFileChoose(input, "pathway_db_file", roots = volumes, session = session)
      req(input$pathway_db_file)
      file_info <- shinyFiles::parseFilePaths(volumes, input$pathway_db_file)
      if (nrow(file_info) > 0) {
        path <- as.character(file_info$datapath)
        vals$pathway_db <- load_rda_data(path)
        output$pathway_db_path <- renderText(path)
      }
    })

    observe({
      shinyFiles::shinyFileChoose(input, "results_rda_file", roots = volumes, session = session)
      req(input$results_rda_file)
      file_info <- shinyFiles::parseFilePaths(volumes, input$results_rda_file)
      if (nrow(file_info) > 0) {
        path <- as.character(file_info$datapath)
        vals$final_result <- load_rda_data(path)
        output$results_rda_path <- renderText(path)
      }
    })

    # --- Analysis Logics ---
    observeEvent(input$run_step1, {
      req(vals$feature_table, vals$ms1_db)
      tryCatch({
        res1 <- featuremsea::annotate_feature_table(vals$feature_table, input$column, vals$ms1_db, input$database_type, input$ms1_match_ppm, input$mfc_rt_tol, input$isotope_number)
        res2 <- featuremsea::process_annotation_table(featuremsea::remove_redundancy(res1), input$database_type)
        vals$ranking_table <- res2$ranking_table
        vals$annotation_table <- res2$original_score_annotation
        output$step1_log <- renderText("Step 1 Done!")
      }, error = function(e) output$step1_log <- renderText(e$message))
    })

    observeEvent(input$run_step2, {
      req(vals$pathway_db, vals$ranking_table, vals$annotation_table)
      l_db <- vals$pathway_db; l_anno <- vals$annotation_table; l_rank <- vals$ranking_table
      l_threads <- as.numeric(input$threads); l_db_type <- input$database_type

      withProgress(message = 'Running fMSEA...', {
        vals$final_result <- featuremsea::perform_fmsea_analysis(
          pathway_database = l_db, annotation_table = l_anno, ranking_table = l_rank,
          threads = l_threads, min.compounds.num = input$min_compounds,
          max.compounds.num = input$max_compounds, id.col = ifelse(l_db_type=="KEGG", "KEGG_ID", "HMDB_ID"),
          perm.num = input$perm_num, fdr.thr = input$fdr_thr
        )
      })
    })

    # --- Table Rendering (with Content Control) ---
    output$sig_modules_table <- DT::renderDataTable({
      req(vals$final_result)
      df <- vals$final_result@significant_modules

      DT::datatable(df, selection = 'single', rownames = FALSE,
                    options = list(
                      scrollX = TRUE,
                      scrollY = "200px",
                      pageLength = 5,
                      dom = 'tp', # Simplified DOM to save space
                      columnDefs = list(list(
                        targets = "_all",
                        render = DT::JS(
                          "function(data, type, row, meta) {",
                          "  return type === 'display' && data !== null && data.length > 20 ?",
                          "  '<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                          "}"
                        )
                      ))
                    ),
                    escape = FALSE # Crucial for rendering the JS/HTML
      )
    })

    # --- Plot Rendering ---
    current_plot <- reactive({
      req(vals$final_result, input$sig_modules_table_rows_selected)
      idx <- input$sig_modules_table_rows_selected
      target_id <- vals$final_result@significant_modules$pathway_id[idx]
      featuremsea::plot_fmsea_plot(vals$final_result, target_id)
    })

    output$fmsea_plot <- renderPlot({ current_plot() })

    # --- Download Handlers ---
    output$download_table <- downloadHandler(
      filename = function() { paste0("fMSEA_Results_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(vals$final_result@significant_modules, file, row.names = FALSE) }
    )

    output$download_png <- downloadHandler(
      filename = function() { paste0("fMSEA_Plot_", Sys.Date(), ".png") },
      content = function(file) { ggplot2::ggsave(file, plot = current_plot(), device = "png", width = 8, height = 6) }
    )
    output$download_pdf <- downloadHandler(
      filename = function() { paste0("fMSEA_Plot_", Sys.Date(), ".pdf") },
      content = function(file) { ggplot2::ggsave(file, plot = current_plot(), device = "pdf", width = 8, height = 6) }
    )

    output$result_summary <- renderPrint({ req(vals$final_result); print(vals$final_result) })
  })
}
