#' Feature-based Metabolite Set Enrichment Analysis (fMSEA) UI
#'
#' @param id module id
#' @import shiny
#' @importFrom bsicons bs_icon
#' @importFrom shinyFiles shinyFilesButton
#' @import featuremsea
#' @noRd
fmsea_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Feature-based Metabolite Set Enrichment Analysis (fMSEA)',
    icon = bsicons::bs_icon("diagram-3"),
    layout_sidebar(
      sidebar = accordion(
        open = c("Data Upload", "Step 1: Annotation", "Step 2: Analysis"),

        # --- 1. Data Upload ---
        accordion_panel(
          title = "Data Upload",
          icon = bsicons::bs_icon("upload"),

          # Feature Table Upload
          shinyFiles::shinyFilesButton(id = ns('feature_table_file'), label = 'Load Feature Table (.rda)', title = "Select Feature Table", multiple = FALSE, buttonType = "default", icon = icon("table")),
          div(textOutput(ns("feature_table_path"), inline = TRUE), style = "font-size: 0.8em; color: grey; margin-bottom: 10px;"),

          # MS1 Database Upload
          shinyFiles::shinyFilesButton(id = ns('ms1_db_file'), label = 'Load MS1 Database (.rda)', title = "Select MS1 Database", multiple = FALSE, buttonType = "default", icon = icon("database")),
          div(textOutput(ns("ms1_db_path"), inline = TRUE), style = "font-size: 0.8em; color: grey; margin-bottom: 10px;"),

          # Pathway Database Upload
          shinyFiles::shinyFilesButton(id = ns('pathway_db_file'), label = 'Load Pathway Database (.rda)', title = "Select Pathway Database", multiple = FALSE, buttonType = "default", icon = icon("bezier-curve")),
          div(textOutput(ns("pathway_db_path"), inline = TRUE), style = "font-size: 0.8em; color: grey;")
        ),

        # --- 2. Step 1 Parameters ---
        accordion_panel(
          title = "Step 1: Annotation & Processing",
          icon = bsicons::bs_icon("1-circle"),
          selectInput(ns("column"), "Column", choices = c("rp", "hilic"), selected = "rp"),
          selectInput(ns("database_type"), "Database Type", choices = c("KEGG", "HMDB"), selected = "KEGG"),
          numericInput(ns("ms1_match_ppm"), "MS1 Match PPM", value = 15),
          numericInput(ns("mfc_rt_tol"), "RT Tolerance (s)", value = 10),
          numericInput(ns("isotope_number"), "Isotope Number", value = 3),

          actionButton(ns("run_step1"), "Run Step 1 (Annotate)", class = "btn-primary", width = "100%"),
          verbatimTextOutput(ns("step1_log"), placeholder = TRUE)
        ),

        # --- 3. Step 2 Parameters ---
        accordion_panel(
          title = "Step 2: fMSEA Analysis",
          icon = bsicons::bs_icon("2-circle"),
          numericInput(ns("threads"), "Threads", value = 3, min = 1), # Default 3
          numericInput(ns("min_compounds"), "Min Compounds", value = 15),
          numericInput(ns("max_compounds"), "Max Compounds", value = 300),
          numericInput(ns("perm_num"), "Permutations", value = 1000),
          numericInput(ns("fdr_thr"), "FDR Threshold", value = 0.05),

          actionButton(ns("run_step2"), "Run Step 2 (fMSEA)", class = "btn-success", width = "100%")
        )
      ),

      # --- Main Panel Output ---
      card(
        card_header("Analysis Results"),
        card_body(
          h5("Step 1 Status:"),
          textOutput(ns("status_step1_text")),
          hr(),
          h5("Step 2 Result Summary:"),
          verbatimTextOutput(ns("result_summary"))
        )
      )
    )
  )
}


#' Feature-based Metabolite Set Enrichment Analysis (fMSEA) Server
#'
#' @param id module id
#' @param volumes shinyFiles volumes
#' @import shiny
#' @import featuremsea
#' @noRd
fmsea_server <- function(id, volumes) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Store loaded data and intermediate results
    vals <- reactiveValues(
      feature_table = NULL,
      ms1_db = NULL,
      pathway_db = NULL,
      ranking_table = NULL,
      annotation_table = NULL,
      final_result = NULL
    )

    # Helper function to load RDA safely
    # This prevents overwriting existing variables and returns the loaded object directly
    load_rda_data <- function(file_path) {
      env <- new.env()
      name <- load(file_path, envir = env)
      return(get(name, envir = env))
    }

    # --- 1. File Upload Logic ---
    observe({
      shinyFiles::shinyFileChoose(input, "feature_table_file", roots = volumes, session = session)
      if (!is.null(input$feature_table_file)) {
        file_selected <- shinyFiles::parseFilePaths(roots = volumes, input$feature_table_file)
        if (nrow(file_selected) > 0) {
          path <- as.character(file_selected$datapath)
          output$feature_table_path <- renderText(path)
          vals$feature_table <- load_rda_data(path)
        }
      }
    })

    observe({
      shinyFiles::shinyFileChoose(input, "ms1_db_file", roots = volumes, session = session)
      if (!is.null(input$ms1_db_file)) {
        file_selected <- shinyFiles::parseFilePaths(roots = volumes, input$ms1_db_file)
        if (nrow(file_selected) > 0) {
          path <- as.character(file_selected$datapath)
          output$ms1_db_path <- renderText(path)
          vals$ms1_db <- load_rda_data(path)
        }
      }
    })

    observe({
      shinyFiles::shinyFileChoose(input, "pathway_db_file", roots = volumes, session = session)
      if (!is.null(input$pathway_db_file)) {
        file_selected <- shinyFiles::parseFilePaths(roots = volumes, input$pathway_db_file)
        if (nrow(file_selected) > 0) {
          path <- as.character(file_selected$datapath)
          output$pathway_db_path <- renderText(path)
          vals$pathway_db <- load_rda_data(path)
        }
      }
    })

    # --- 2. Step 1 Logic: Annotation ---
    observeEvent(input$run_step1, {
      req(vals$feature_table, vals$ms1_db)
      output$step1_log <- renderText("Running Step 1... Please wait.")

      # Snapshot inputs to avoid reactive dependency issues during calculation
      local_column <- input$column
      local_db_type <- input$database_type
      local_ppm <- as.numeric(input$ms1_match_ppm)
      local_rt_tol <- as.numeric(input$mfc_rt_tol)
      local_isotope <- as.numeric(input$isotope_number)

      tryCatch({
        # 1. Annotation
        annotation_table_final <- featuremsea::annotate_feature_table(
          feature_table = vals$feature_table,
          column = local_column,
          metabolite_database = vals$ms1_db,
          database_type = local_db_type,
          ms1_match_ppm = local_ppm,
          mfc_rt_tol = local_rt_tol,
          isotope_number = local_isotope
        )

        # 2. Remove Redundancy
        annotation_table_final2 <- featuremsea::remove_redundancy(
          annotation_table = annotation_table_final
        )

        # 3. Process Annotation Table
        results_step1 <- featuremsea::process_annotation_table(
          annotation_table_final2 = annotation_table_final2,
          database_type = local_db_type
        )

        vals$ranking_table <- results_step1$ranking_table
        vals$annotation_table <- results_step1$original_score_annotation

        output$step1_log <- renderText("Step 1 Complete! Ready for fMSEA Analysis.")
        output$status_step1_text <- renderText(paste("Step 1 Done. Ranking Table Rows:", nrow(vals$ranking_table)))

      }, error = function(e) {
        output$step1_log <- renderText(paste("Error:", e$message))
      })
    })

    # --- 3. Step 2 Logic: fMSEA Analysis ---
    observeEvent(input$run_step2, {
      req(vals$pathway_db, vals$ranking_table, vals$annotation_table)

      # *** CRITICAL FIX FOR PARALLEL PROCESSING ***
      # Snapshot all reactive inputs and reactive values to local variables.
      # This prevents the "Reactive context was created in one process and accessed from another" error.
      local_pathway_db <- vals$pathway_db
      local_annotation_table <- vals$annotation_table
      local_ranking_table <- vals$ranking_table

      local_threads <- as.numeric(input$threads)
      local_min_comp <- as.numeric(input$min_compounds)
      local_max_comp <- as.numeric(input$max_compounds)
      local_perm_num <- as.numeric(input$perm_num)
      local_fdr_thr <- as.numeric(input$fdr_thr)
      local_db_type <- input$database_type

      withProgress(message = 'Running fMSEA Analysis...', value = 0, {
        tryCatch({
          incProgress(0.1, detail = "Preparing data...")

          # Determine ID column based on DB type
          current_id_col <- if(local_db_type == "KEGG") "KEGG_ID" else "HMDB_ID"

          incProgress(0.3, detail = "Calculating (this may take a while)...")

          # Pass LOCAL variables to the function, not input$xxx
          results <- featuremsea::perform_fmsea_analysis(
            pathway_database = local_pathway_db,
            annotation_table = local_annotation_table,
            ranking_table = local_ranking_table,
            threads = local_threads,
            min.compounds.num = local_min_comp,
            max.compounds.num = local_max_comp,
            id.col = current_id_col,
            perm.num = local_perm_num,
            seed = 123,
            fdr.thr = local_fdr_thr,
            max.iter.num = 1,
            verbose = TRUE
          )

          vals$final_result <- results
          incProgress(1, detail = "Finished!")

        }, error = function(e) {
          vals$final_result <- paste("Error in Step 2:", e$message)
        })
      })
    })

    # --- 4. Result Display ---
    output$result_summary <- renderPrint({
      req(vals$final_result)
      print(vals$final_result)
    })
  })
}

