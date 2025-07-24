#' Metabolite annotation
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


feature_annotation_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Metabolite annotation',
    icon = bs_icon("envelope-open-heart"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Add MS2 spectra",
          icon = bsicons::bs_icon("menu-app"),
          # 文件来源选择
          radioButtons(
            inputId = ns("ms2_source"),
            label = "Select MS2 source:",
            choices = c("Upload ZIP file" = "upload"),
            selected = "upload"
          ),

          # 文件上传区域
          conditionalPanel(
            condition = "input.ms2_source == 'upload'",
            ns = ns,
            fileInput(
              inputId = ns('ms2_zip'),
              label = 'Upload MS2 ZIP file',
              multiple = FALSE,
              accept = '.zip',
              buttonLabel = "Browse...",
              placeholder = "No file selected"
            )
          ),

          # URL 输入区域
          conditionalPanel(
            condition = "input.ms2_source == 'url'",
            ns = ns,
            textInput(
              inputId = ns('ms2_url'),
              label = 'Enter MS2 ZIP URL:',
              placeholder = "https://example.com/ms2_data.zip"
            ),
            helpText("URL must point to a .zip file")
          ),

          # 处理按钮
          actionButton(
            inputId = ns("process_ms2"),
            label = "Process MS2 Files",
            icon = icon("gear"),
            class = "btn-primary",
            width = "100%"
          ),

          # 状态显示
          uiOutput(ns("ms2_status")),

          # 路径显示
          verbatimTextOutput(outputId = ns("MS2_path")),

          # 参数设置
          textInput(
            inputId = ns('column'),label = 'column',value = 'rp'
          ),
          textInput(
            inputId = ns('ms1.ms2.match.rt.tol'),label = 'ms1.ms2.match.rt.tol',value = 15
          ),
          textInput(
            inputId = ns('ms1.ms2.match.mz.tol'),label = 'ms1.ms2.match.mz.tol',value = 30
          ),
          actionButton(inputId = ns("add_ms2"),label = "Add MS2 to Dataset",icon = icon("play"))
        ),
        accordion_panel(
          title = "Annotation parameters",
          icon = bsicons::bs_icon("gear"),
          textInput_div(
            inputId = ns('anno_ms1.match.ppm'),
            label = "ms1.match.ppm ",
            value = 25,
            placeholder = "Only accept number, Precursor match ppm tolerance.",
            title = "Precursor match ppm tolerance."
          ),
          textInput_div(
            inputId = ns('anno_ms2.match.ppm'),
            label = "ms2.match.ppm ",
            value = 30,
            placeholder = "Only accept number, Fragment ion match ppm tolerance.",
            title = "Fragment ion match ppm tolerance."
          ),
          textInput_div(
            inputId = ns('anno_rt.match.tol'),
            label = "rt.match.tol",
            value = 30,
            placeholder = "Only accept number, RT match tolerance.",
            title = "RT match tolerance."
          ),
          textInput_div(
            inputId = ns('anno_candidate.num'),
            label = "candidate.num",
            value = 3,
            placeholder = "Only accept number, The number of candidate.",
            title = "The number of candidate."
          ),
          selectInput_div(
            inputId = ns('anno_column'),
            label = "column",choices = c("rp","hilic"),
            selected = "rp",multiple = FALSE,
            title = "rp: reverse phase \nhilic: HILIC column"
          ),
          textInput_div(
            inputId = ns('anno_threads'),
            label = "threads",
            value = 3,
            placeholder = "Only accept number, The number of threads",
            title = "Number of threads"
          )
        ),
        accordion_panel(
          title = "Optional parameters",
          textInput_div(
            inputId = ns('anno_mz.ppm.thr'),label = "mz.ppm.thr",value = 400,title = "Accurate mass tolerance for m/z error calculation.",placeholder = "numeric"
          ),
          textInput_div(
            inputId = ns('anno_ms2.match.tol'),label = 'ms2.match.tol',value = 0.5,placeholder = "numeric",title = "MS2 match (MS2 similarity) tolerance."
          ),
          textInput_div(
            inputId = ns('anno_fraction.weight'),label = 'fraction.weight',value = 0.3,title = "The weight for matched fragments.",placeholder = "numeric"
          ),
          textInput_div(
            inputId = ns('anno_dp.forward.weight'),label = 'dp.forward.weight',value = 0.6,title = "Forward dot product weight.",placeholder = "numeric"
          ),
          textInput_div(
            inputId = ns('anno_dp.reverse.weight'),label = 'dp.reverse.weight',value = 0.1,title = "Reverse dot product weight.",placeholder = "numeric"
          ),
          hr_head(),
          textInput_div(
            inputId = ns('anno_remove_fragment_intensity_cutoff'),label = 'remove_fragment_intensity_cutoff',value = 0,title = "remove_fragment_intensity_cutoff",placeholder = "numeric"
          ),
          hr_head(),
          textInput_div(
            inputId = ns('anno_ce'),label = 'ce',value = "all",title = "Collision energy. Please confirm the CE values in your database. Default is all",placeholder = "CE model"
          ),
          textInput_div(
            inputId = ns('anno_ms1.match.weight'),label = 'ms1.match.weight',value = 0.25,title = "The weight of MS1 match for total score calculation.",placeholder = "numeric"
          ),
          hr_head(),
          textInput_div(
            inputId = ns('anno_rt.match.weight'),label = 'rt.match.weight',value = 0.25,title = "The weight of RT match for total score calculation.",placeholder = "numeric"
          ),
          textInput_div(
            inputId = ns('anno_ms2.match.weight'),label = 'ms2.match.weight',value = 0.5,title = "The weight of MS2 match for total score calculation.",placeholder = "numeric"
          ),
          textInput_div(
            inputId = ns('anno_total.score.tol'),label = 'total.score.tol',value = 0.5,title = "Total score tolerance. The total score are referring to MS-DIAL.",placeholder = "numeric"
          )
        ),
        accordion_panel(
          title = "Database",
          icon = bsicons::bs_icon("database"),
          selectInput_div(
            inputId = ns('norm_db'),
            label = "Public database",
            choices = c("MoNA","Massbank","HMDB","NULL"),
            selected = c("MoNA","Massbank","HMDB"),
            multiple = TRUE,
            title = "Select database"
          ),

          # 改为ZIP文件上传
          fileInput(
            inputId = ns('cuz_db_zip'),
            label = 'Upload Customized Database ZIP',
            multiple = FALSE,
            accept = '.zip',
            buttonLabel = "Browse...",
            placeholder = "No file selected"
          ),

          # 状态显示
          uiOutput(ns("cuz_db_status")),
          br(),
          actionButton(
            inputId = ns('anno_start'),
            label = "Start annotation",
            icon = icon("play")
          )
        )
      ),
      page_fluid(
        nav_panel(
          title = "Feature annotation",
          htmlOutput(ns("anno_check1_pos")),
          navset_card_tab(
            title = "Annotation table",
            height = 400,
            full_screen = TRUE,
            nav_panel(
              "Positive",
              DT::dataTableOutput(outputId = ns("Annotation_pos"))
            ),
            nav_panel(
              "Negative",
              DT::dataTableOutput(outputId = ns("Annotation_neg"))
            ),
            nav_panel(
              shiny::icon("circle-info"),
              markdown("description of noise remove method.")
            )
          ),
          navset_card_tab(
            title = "Status",
            height = 400,
            full_screen = TRUE,
            nav_panel(
              "Positive",
              verbatimTextOutput(ns("obj_anno.pos"))
            ),
            nav_panel(
              "Negative",
              verbatimTextOutput(ns("obj_anno.neg"))
            )
          )
        )
      )
    )
  )
}



#' Data normalization and integration
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
#' @import massdbbuildin
#' @param id module of server
#' @param volumes shinyFiles volumes
#' @param prj_init use project init variables.
#' @param data_import_rv reactivevalues mass_dataset export
#' @param data_clean_rv reactivevalues p2 dataclean
#' @param data_anno reactivevalues data annotation
#' @noRd


feature_annotation_server <- function(id, volumes, prj_init, data_import_rv, data_clean_rv, data_anno) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. MS2 File Handling -----------------------------------------------------
    # Store MS2 path and processing status
    ms2_path <- reactiveVal(NULL)
    ms2_processing_status <- reactiveVal("idle")  # idle, processing, success, error

    # UI for MS2 status display
    output$ms2_status <- renderUI({
      status <- ms2_processing_status()
      path <- ms2_path()

      if (status == "idle") {
        tags$div(
          class = "alert alert-info",
          bsicons::bs_icon("info-circle"),
          " Please process MS2 files first"
        )
      } else if (status == "processing") {
        tags$div(
          class = "alert alert-warning",
          bsicons::bs_icon("hourglass-split"),
          " Processing MS2 files..."
        )
      } else if (status == "success" && !is.null(path)) {
        tags$div(
          class = "alert alert-success",
          bsicons::bs_icon("check-circle-fill"),
          " MS2 files processed successfully"
        )
      } else if (status == "error") {
        tags$div(
          class = "alert alert-danger",
          bsicons::bs_icon("exclamation-triangle-fill"),
          " Error processing MS2 files"
        )
      }
    })

    # Display MS2 path
    output$MS2_path <- renderText({
      path <- ms2_path()
      if (is.null(path)) "MS2 path not set yet" else path
    })

    # Process MS2 files
    observeEvent(input$process_ms2, {
      tryCatch({
        # Validate project initialization
        if (is.null(prj_init$wd) || !dir.exists(prj_init$wd)) {
          shinyalert("Error", "Project not initialized. Please initialize project first.", type = "error")
          return()
        }

        # Set processing status
        ms2_processing_status("processing")

        # Target directory
        target_dir <- file.path(prj_init$wd, "MS2")

        # Clean existing directory
        if (dir.exists(target_dir)) unlink(target_dir, recursive = TRUE)
        dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)

        # Create temp directory
        temp_dir <- file.path(prj_init$wd, "temp_MS2")
        if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
        dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

        # Get ZIP file path
        zip_file <- NULL
        if (input$ms2_source == "upload") {
          req(input$ms2_zip)
          zip_file <- input$ms2_zip$datapath

          # Validate file type
          if (!grepl("\\.zip$", input$ms2_zip$name, ignore.case = TRUE)) {
            shinyalert("Error", "Please upload a ZIP file", type = "error")
            ms2_processing_status("error")
            return()
          }
        } else if (input$ms2_source == "url") {
          req(input$ms2_url)
          zip_file <- file.path(temp_dir, "downloaded_ms2.zip")

          # Validate URL
          if (!grepl("^https?://", input$ms2_url)) {
            shinyalert("Invalid URL", "URL must start with http:// or https://", type = "error")
            ms2_processing_status("error")
            return()
          }

          # Download file
          withProgress(
            message = 'Downloading MS2 files',
            detail = 'This may take a while...',
            value = 0.3,
            {
              tryCatch({
                download.file(input$ms2_url, zip_file, mode = "wb")
              }, error = function(e) {
                shinyalert("Download Failed", paste("Error details:", e$message), type = "error")
                ms2_processing_status("error")
                return()
              })
            }
          )
        }

        # Extract files
        withProgress(
          message = 'Extracting ZIP file',
          detail = 'This may take a while...',
          value = 0.6,
          {
            tryCatch({
              unzip(zip_file, exdir = temp_dir)
            }, error = function(e) {
              shinyalert("Extraction Failed", paste("Error details:", e$message), type = "error")
              ms2_processing_status("error")
              return()
            })
          }
        )

        # Find actual MS2 data directory
        extracted_dirs <- list.dirs(temp_dir, full.names = TRUE, recursive = FALSE)

        # Case 1: Directly contains POS/NEG
        if (all(c("POS", "NEG") %in% basename(extracted_dirs))) {
          data_dir <- temp_dir
        }
        # Case 2: Contains subdirectory with POS/NEG
        else {
          sub_dirs <- list.dirs(temp_dir, full.names = TRUE, recursive = FALSE)

          # Find directories containing POS/NEG
          data_dir_candidates <- sub_dirs[
            sapply(sub_dirs, function(x) {
              dirs_in_x <- list.dirs(x, full.names = FALSE, recursive = FALSE)
              all(c("POS", "NEG") %in% dirs_in_x)
            })
          ]

          if (length(data_dir_candidates)) {
            data_dir <- data_dir_candidates[1]
          } else {
            # Try recursive search for POS/NEG
            pos_dirs <- list.dirs(temp_dir, recursive = TRUE)[
              grepl("POS$", list.dirs(temp_dir, recursive = TRUE))
            ]
            neg_dirs <- list.dirs(temp_dir, recursive = TRUE)[
              grepl("NEG$", list.dirs(temp_dir, recursive = TRUE))
            ]

            if (length(pos_dirs) && length(neg_dirs)) {
              common_parent <- dirname(common_path(c(pos_dirs[1], neg_dirs[1])))
              data_dir <- common_parent
            } else {
              stop("No valid directory containing POS/NEG found")
            }
          }
          }

        # Move POS/NEG to target
        if (dir.exists(file.path(data_dir, "POS"))) {
          file.rename(file.path(data_dir, "POS"), file.path(target_dir, "POS"))
        }
        if (dir.exists(file.path(data_dir, "NEG"))) {
          file.rename(file.path(data_dir, "NEG"), file.path(target_dir, "NEG"))
        }

        # Remove macOS artifacts
        macosx_dirs <- list.dirs(temp_dir, recursive = TRUE)[
          grepl("__MACOSX|_MACOSX", list.dirs(temp_dir, recursive = TRUE))
        ]
        if (length(macosx_dirs)) unlink(macosx_dirs, recursive = TRUE)

        # Remove hidden files (starting with .)
        hidden_files <- list.files(temp_dir, all.files = TRUE, full.names = TRUE, recursive = TRUE)
        hidden_files <- hidden_files[grepl("/\\.", hidden_files)]
        if (length(hidden_files)) file.remove(hidden_files)

        # Verify directory structure
        pos_exists <- dir.exists(file.path(target_dir, "POS"))
        neg_exists <- dir.exists(file.path(target_dir, "NEG"))
        if (!pos_exists && !neg_exists) {
          shinyalert("Error", "ZIP must contain POS and/or NEG directories", type = "error")
          ms2_processing_status("error")
          return()
        }

        # Clean temp directory
        unlink(temp_dir, recursive = TRUE)

        # Update status
        ms2_path(target_dir)
        ms2_processing_status("success")
        shinyalert("Success", "MS2 files processed successfully", type = "success")

        }, error = function(e) {
          shinyalert("Error", paste("Failed to process MS2 files:", e$message), type = "error")
          ms2_processing_status("error")
        })
      })

      # 2. Custom Database Handling ----------------------------------------------
      # Store custom DB path and status
      cuz_db_path <- reactiveVal(NULL)
      cuz_db_processing_status <- reactiveVal("idle")

      # UI for custom DB status
      output$cuz_db_status <- renderUI({
        status <- cuz_db_processing_status()
        path <- cuz_db_path()

        if (status == "idle") {
          tags$div(
            class = "alert alert-info",
            bsicons::bs_icon("info-circle"),
            " Please upload custom database ZIP"
          )
        } else if (status == "processing") {
          tags$div(
            class = "alert alert-warning",
            bsicons::bs_icon("hourglass-split"),
            " Processing database files..."
          )
        } else if (status == "success" && !is.null(path)) {
          tags$div(
            class = "alert alert-success",
            bsicons::bs_icon("check-circle-fill"),
            " Custom database processed successfully"
          )
        } else if (status == "error") {
          tags$div(
            class = "alert alert-danger",
            bsicons::bs_icon("exclamation-triangle-fill"),
            " Error processing database files"
          )
        }
      })

      # Process custom database ZIP
      observeEvent(input$cuz_db_zip, {
        tryCatch({
          # Validate project initialization
          if (is.null(prj_init$wd) || !dir.exists(prj_init$wd)) {
            shinyalert("Error", "Project not initialized", type = "error")
            return()
          }

          # Set processing status
          cuz_db_processing_status("processing")

          # Target directory
          target_dir <- file.path(prj_init$wd, "cuz_db")

          # Clean existing directory
          if (dir.exists(target_dir)) unlink(target_dir, recursive = TRUE)
          dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)

          # Create temp directory
          temp_dir <- file.path(prj_init$wd, "temp_cuz_db")
          if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
          dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

          # Get uploaded file
          req(input$cuz_db_zip)
          zip_file <- input$cuz_db_zip$datapath

          # Validate file type
          if (!grepl("\\.zip$", input$cuz_db_zip$name, ignore.case = TRUE)) {
            shinyalert("Error", "Please upload a ZIP file", type = "error")
            cuz_db_processing_status("error")
            return()
          }

          # Extract files
          withProgress(
            message = 'Extracting database files',
            detail = 'This may take a while...',
            value = 0.5,
            {
              unzip(zip_file, exdir = temp_dir)
            }
          )

          # Remove macOS artifacts
          macosx_dirs <- list.dirs(temp_dir, recursive = TRUE)[
            grepl("__MACOSX|_MACOSX", list.dirs(temp_dir, recursive = TRUE))
          ]
          if (length(macosx_dirs)) unlink(macosx_dirs, recursive = TRUE)

          # Remove hidden files (starting with .)
          hidden_files <- list.files(temp_dir, all.files = TRUE, full.names = TRUE, recursive = TRUE)
          hidden_files <- hidden_files[grepl("/\\.", hidden_files)]
          if (length(hidden_files)) file.remove(hidden_files)

          # Move all .rda files to target directory
          rda_files <- list.files(temp_dir, pattern = "\\.rda$", full.names = TRUE, recursive = TRUE)
          if (length(rda_files)) {
            file.copy(rda_files, target_dir)
          } else {
            shinyalert("Warning", "No .rda database files found in ZIP", type = "warning")
          }

          # Clean temp directory
          unlink(temp_dir, recursive = TRUE)

          # Update status
          cuz_db_path(target_dir)
          cuz_db_processing_status("success")
          shinyalert("Success", "Custom database processed successfully", type = "success")

        }, error = function(e) {
          shinyalert("Error", paste("Failed to process database files:", e$message), type = "error")
          cuz_db_processing_status("error")
        })
      })


      # 3. Utility Functions ----------------------------------------------------
      check_ion_modes <- function(data_rv, prj) {
        list(
          has_pos = !is.null(data_rv$object_pos_norm) || !is.null(prj$object_positive.init),
          has_neg = !is.null(data_rv$object_neg_norm) || !is.null(prj$object_negative.init)
        )
      }

      common_path <- function(paths) {
        path_split <- strsplit(paths, "/")
        common_elements <- Reduce(intersect, path_split)
        paste(common_elements, collapse = "/")
      }

      # 4. Add MS2 Spectra ------------------------------------------------------
      perform_add_ms2 = function(object, para, polarity, ms2_path) {
        tryCatch({
          subdir <- if (polarity == "positive") "POS" else "NEG"
          full_path <- file.path(ms2_path, subdir)

          if (!dir.exists(full_path)) {
            shinyalert("Path Error", paste("MS2 directory not found:", full_path), type = "error")
            return(NULL)
          }

          res <- object %>%
            mutate_ms2(
              polarity = polarity,
              column = para$column,
              ms1.ms2.match.rt.tol = para$ms1.ms2.match.rt.tol,
              ms1.ms2.match.mz.tol = para$ms1.ms2.match.mz.tol,
              path = full_path
            )
          return(res)
        }, error = function(e) {
          shinyalert("Add MS2 Error", paste("Error details:", e$message), type = "error")
          return(NULL)
        })
      }

      # MS2 parameters
      para_ms2 =  reactive({
        list(
          column = as.character(input$column),
          ms1.ms2.match.rt.tol = as.numeric(input$ms1.ms2.match.rt.tol),
          ms1.ms2.match.mz.tol = as.numeric(input$ms1.ms2.match.mz.tol)
        )
      })

      # Add MS2 spectra event
      observeEvent(input$add_ms2, {
        tryCatch({
          # Check ion modes
          modes <- check_ion_modes(data_clean_rv, prj_init)

          # Validate data existence
          if (!modes$has_pos && !modes$has_neg) {
            shinyalert("Data Not Loaded", "No ion mode data found. Process data first.", type = "error")
            return()
          }

          # Validate MS2 path
          if (is.null(ms2_path())) {
            shinyalert("MS2 Path Not Set", "Please process MS2 files first", type = "error")
            return()
          }

          # Get parameters
          para <- para_ms2()

          # Determine data source based on workflow step
          if (prj_init$steps == "Annotation") {
            # Use initialized objects from project
            if (modes$has_pos) data_anno$object_pos <- prj_init$object_positive.init
            if (modes$has_neg) data_anno$object_neg <- prj_init$object_negative.init
          } else {
            # Use objects from previous processing steps
            if (modes$has_pos) data_anno$object_pos <- data_clean_rv$object_pos_norm
            if (modes$has_neg) data_anno$object_neg <- data_clean_rv$object_neg_norm
          }

          # Validate objects exist
          if ((modes$has_pos && is.null(data_anno$object_pos)) ||
              (modes$has_neg && is.null(data_anno$object_neg))) {
            shinyalert("Data Error", "Required data objects not found", type = "error")
            return()
          }

          # Process steps
          steps <- character()
          if (modes$has_pos) steps <- c(steps, "Processing positive mode")
          if (modes$has_neg) steps <- c(steps, "Processing negative mode")
          steps <- c(steps, "Saving results")
          total_steps <- length(steps)

          withProgress(message = "Adding MS2 Spectra...", value = 0, {
            # Positive mode processing
            if (modes$has_pos) {
              incProgress(1/total_steps, detail = steps[1])
              data_anno$object_pos <- perform_add_ms2(
                object = data_anno$object_pos,
                para = para,
                polarity = "positive",
                ms2_path = ms2_path()
              )

              # Save results
              if (!is.null(data_anno$object_pos)) {
                object_pos_ms2 <- data_anno$object_pos
                save(
                  object_pos_ms2,
                  file = file.path(prj_init$mass_dataset_dir, "06.object_pos_ms2.rda")
                )
              }
            }

            # Negative mode processing
            if (modes$has_neg) {
              incProgress(1/total_steps, detail = steps[2])
              data_anno$object_neg <- perform_add_ms2(
                object = data_anno$object_neg,
                para = para,
                polarity = "negative",
                ms2_path = ms2_path()
              )

              # Save results
              if (!is.null(data_anno$object_neg)) {
                object_neg_ms2 <- data_anno$object_neg
                save(
                  object_neg_ms2,
                  file = file.path(prj_init$mass_dataset_dir, "06.object_neg_ms2.rda")
                )
              }
            }

            # Update status
            incProgress(1/total_steps, detail = steps[3])
            data_anno$ms2_status <- TRUE
            shinyalert("Success", "MS2 spectra added to dataset", type = "success")
          })

        }, error = function(e) {
          shinyalert("Add MS2 Failed", paste("Error details:", e$message), type = "error")
        })
      })


      run_annotation <- function(object, para, polarity, database) {
        annotate_metabolites_mass_dataset(
          object = object,
          polarity = polarity,
          database = database,
          ms1.match.ppm = para$ms1.match.ppm,
          ms2.match.ppm = para$ms2.match.ppm,
          rt.match.tol = para$rt.match.tol,
          candidate.num = para$candidate.num,
          column = para$column,
          threads = para$threads,
          mz.ppm.thr = para$mz.ppm.thr,
          ms2.match.tol = para$ms2.match.tol,
          fraction.weight = para$fraction.weight,
          dp.forward.weight = para$dp.forward.weight,
          dp.reverse.weight = para$dp.reverse.weight,
          remove_fragment_intensity_cutoff = para$remove_fragment_intensity_cutoff,
          ce = para$ce,
          ms1.match.weight = para$ms1.match.weight,
          rt.match.weight = para$rt.match.weight,
          ms2.match.weight = para$ms2.match.weight,
          total.score.tol = para$total.score.tol
        )
      }
      check_ms2 = function(object){
        if(length(object@ms2_data) == 0) {
          return(FALSE)
        } else {
          return(TRUE)
        }
      }

      para_anno = reactive({
        list(
          ms1.match.ppm = as.numeric(input$anno_ms1.match.ppm),
          ms2.match.ppm = as.numeric(input$anno_ms2.match.ppm),
          rt.match.tol = as.numeric(input$anno_rt.match.tol),
          candidate.num = as.numeric(input$anno_candidate.num),
          column = as.character(input$anno_column),
          threads= as.numeric(input$anno_threads),
          norm_db = as.character(input$norm_db),
          mz.ppm.thr = as.numeric(input$anno_mz.ppm.thr),
          ms2.match.tol = as.numeric(input$anno_ms2.match.tol),
          fraction.weight = as.numeric(input$anno_fraction.weight),
          dp.forward.weight = as.numeric(input$anno_dp.forward.weight),
          dp.reverse.weight = as.numeric(input$anno_dp.reverse.weight),
          remove_fragment_intensity_cutoff = as.numeric(input$anno_remove_fragment_intensity_cutoff),
          ce = as.character(input$anno_ce),
          rt.match.weight = as.numeric(input$anno_rt.match.weight),
          ms2.match.weight = as.numeric(input$anno_ms2.match.weight),
          ms1.match.weight = as.numeric(input$anno_ms1.match.weight),
          total.score.tol= as.numeric(input$anno_total.score.tol)
        )
      })

      ##> run_anno
      observeEvent(
        input$anno_start,
        {
          shinyjs::disable("anno_start")
          modes <- check_ion_modes(data_clean_rv, prj_init)

          if (!modes$has_pos && !modes$has_neg) {
            # No data initialized at all
            shinyalert(
              "Data Not Loaded",
              "No positive/negative ion mode data found. Upload data first.",
              type = "error"
            )
            return()
          }
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
          if(is.null(data_clean_rv$object_pos_norm) && is.null(data_clean_rv$object_neg_norm)){
            if (!is.null(prj_init$object_negative.init) || !is.null(prj_init$object_positive.init)) {
              # Data initialized but current step is invalid
              if (prj_init$steps != "Annotation") {
                shinyalert(
                  "Step Error",
                  "Invalid workflow sequence detected.\nPlease restart from the 'ANNOTATION' step.",
                  type = "error"
                )
                return()
              }
            }
          }
          ## previous add ms2 has been finished
          if(!isTRUE(data_anno$ms2_status)) {
            if(prj_init$steps == "Annotation") {
              if(modes$has_pos) data_anno$object_pos <- prj_init$object_positive.init
              if(modes$has_neg) data_anno$object_neg <- prj_init$object_negative.init
            } else {
              if(modes$has_pos) data_anno$object_pos <- data_clean_rv$object_pos_norm
              if(modes$has_neg) data_anno$object_neg <- data_clean_rv$object_neg_norm
            }
          }


          # check ms2
          if ((modes$has_pos && !isTRUE(check_ms2(data_anno$object_pos))) || (modes$has_neg && !isTRUE(check_ms2(data_anno$object_neg)))){
            shinyalert(
              "Warning!",
              "MS2 data was not detected. Annotations will be based on MS1 data only.",
              type = "warning"
            )
          }

          para = para_anno()

          ## buildin database

          ##! The integrated database needs to be loaded, then replace this code.
          shinyalert(
            title = "Preparing Annotation Database",
            text = HTML("Processing will take <b>10-20 seconds</b>. <br><br>
                  <span style='color:red;'>DO NOT click the 'Start annotation' button again</span>"),
            type = "warning",
            timer = 5000,    # close in 5 s
            html = TRUE
          )

          data_anno$buildin_db <-
            list(
              MoNA = mona_ms2,
              Massbank = massbank_ms2,
              HMDB = hmdb_ms2
            )

          data_anno$buildin_name = para$norm_db %>% as.character()

          if(length(data_anno$buildin_name) == 0) {
            data_anno$buildin_db = NULL
          } else {
            temp_anno_idx = match(data_anno$buildin_name,names(data_anno$buildin_db))
            data_anno$buildin_db = data_anno$buildin_db[temp_anno_idx]
          }

          ## Customized ms database
          # Custom databases
          data_anno$cuz_db_path <- cuz_db_path()

          temp_file_name = dir(data_anno$cuz_db_path,"*.rda")

          if(length(temp_file_name) == 0) {
            data_anno$db = data_anno$buildin_db
          } else {
            data_anno$cuz_db = list()
            for (i in 1:length(temp_file_name)) {
              xx = load(file = paste0(data_anno$cuz_db_path,"/",temp_file_name[[i]]))
              data_anno$cuz_db[[i]] = get(xx)
            }
            data_anno$cuz_name = str_remove(string = temp_file_name,pattern = "\\.rda")

            names(data_anno$cuz_db) = data_anno$cuz_name

            if(is.null(data_anno$buildin_db)){
              data_anno$db = data_anno$cuz_db
            } else {
              data_anno$db <- c(data_anno$buildin_db,data_anno$cuz_db)
            }
          }
          dir.create(path = paste0(prj_init$wd,"/temp/Anno_Database/"),showWarnings = F,recursive = T)
          temp_db <- data_anno$db
          data_clean_rv$db <- data_anno$db
          save(temp_db,file =  paste0(prj_init$wd,"/temp/Anno_Database/auto_saved.dblist"))
          print("check point2")
          #> annotation
          if(length(data_anno$db) == 0) {
            shinyalert(
              "Error!",
              "No metabolomics database detected. Please select an existing database or upload a METID-generated metabolite database",
              type = "error"
            )
            return()
          } else {
            tags = names(data_anno$db)
            ##> compound annotation
            pro_steps_anno = c(paste0("Database ",tags," in progress..."),"Finish!")

            anno_steps = length(pro_steps_anno)
            withProgress(message = 'Compound annoation', value = 0,
                         expr = {
                           for (i in 1:(anno_steps)) {
                             incProgress(1/anno_steps,detail = pro_steps_anno[i])
                             if(i == 1) {
                               if(modes$has_pos){
                                 print("check point3")
                                 para = para_anno()
                                 data_anno$object_pos_anno <- run_annotation(
                                   object = data_anno$object_pos,
                                   para = para,
                                   polarity = "positive",
                                   database = data_anno$db[[i]]
                                 )
                               }
                               if(modes$has_neg){
                                 print("check point4")
                                 para = para_anno()
                                 data_anno$object_neg_anno <- run_annotation(
                                   object = data_anno$object_neg,
                                   para = para,
                                   polarity = "negative",
                                   database = data_anno$db[[i]]
                                 )
                               }

                             } else if(i > 1 & i < anno_steps) {
                               if(modes$has_pos){
                                 para = para_anno()
                                 data_anno$object_pos_anno <- run_annotation(
                                   object = data_anno$object_pos_anno,
                                   para = para,
                                   polarity = "positive",
                                   database = data_anno$db[[i]]
                                 )
                               }
                               if(modes$has_neg){
                                 para = para_anno()
                                 data_anno$object_neg_anno <- run_annotation(
                                   object = data_anno$object_neg_anno,
                                   para = para,
                                   polarity = "negative",
                                   database = data_anno$db[[i]]
                                 )
                               }
                             } else if(i == anno_steps) {
                               if (modes$has_pos) {
                                 data_clean_rv$object_pos_anno = data_anno$object_pos_anno
                                 object_pos_anno <- data_anno$object_pos_anno
                                 save(
                                   object_pos_anno,
                                   file = file.path(prj_init$mass_dataset_dir, "07.object_pos_anno.rda")
                                 )
                               }
                               if (modes$has_neg) {
                                 data_clean_rv$object_neg_anno = data_anno$object_neg_anno
                                 object_neg_anno <- data_anno$object_neg_anno
                                 save(
                                   object_neg_anno,
                                   file = file.path(prj_init$mass_dataset_dir, "07.object_neg_anno.rda")
                                 )
                               }
                             }
                           }
                         }

            )



            # show process
            output$obj_anno.pos  = check_massdata_info(
              object = data_anno$object_pos_anno,
              mode = "positive"
            )

            output$obj_anno.neg  = check_massdata_info(
              object = data_anno$object_neg_anno,
              mode = "negative"
            )

            #> data table
            #>
            output$Annotation_pos = renderDataTable_formated(
              actions = input$anno_start,
              condition1 = data_anno$object_pos_anno,filename.a = "3.6.6.annotation_pos",
              tbl = data_anno$object_pos_anno %>% extract_annotation_table()
            )

            output$Annotation_neg = renderDataTable_formated(
              actions = input$anno_start,
              condition1 = data_anno$object_neg_anno,filename.a = "3.6.6.annotation_neg",
              tbl = data_anno$object_neg_anno %>% extract_annotation_table()
            )

            #> Summary
            temp_db_name = paste(tags,collapse = " | ")
            output$anno_check1_pos = renderUI({
              isolate(HTML(paste0(
                '<font color = blue> <b>Selected database: </b> </font> <font color=red>',temp_db_name,'</font> ')))
            })

          }

        })
  })
  }
