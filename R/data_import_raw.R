#' import from raw data of UI
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyFiles shinyDirButton
#' @importFrom DT dataTableOutput
#' @noRd


data_import_raw_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = 'Start with MS file',
    icon = bs_icon("upload"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "MS files",
          icon = bsicons::bs_icon("menu-app"),
          shinyDirButton(id = ns("MS1"), label = "Select MS1 folder" ,
                         title = "The MS1 file folder:",
                         buttonType = "default", class = NULL,
                         icon = bs_icon("folder"), multiple = FALSE),
          tags$span(textOutput(outputId = ns("MS1_path")), class = "text-wrap")
         ),
        accordion_panel(
          title = "Peak picking parameters",
          icon = bsicons::bs_icon("gear"),
          textInput(
            inputId = ns('ppm'),label = "ppm",value = 15,
          ),

          textInput(
            inputId = ns('p_min'),label = 'peakwidth min',value = 5
          ),

          textInput(
            inputId = ns('p_max'),label = 'peakwidth max',value = 30
          ),

          textInput(
            inputId = ns('snthresh'),label = 'snthresh',value = 10
          ),

          textInput(
            inputId = ns('pre_left'),label = 'prefilter peaks',value = 3
          ),

          textInput(
            inputId = ns('pre_right'),label = 'prefilter intensity',value = 500
          ),

          selectInput(
            inputId = ns('fitgauss'),label = 'fitgauss',choices = c("TRUE","FALSE"),selected = "FALSE",multiple = F
          ),

          textInput(
            inputId = ns('integrate'),label = 'integrate',value = 2
          ),

          textInput(
            inputId = ns('noise'),label = 'noise',value = 500
          ),

          textInput(
            inputId = ns('mzdiff'),label = 'mzdiff',value = 0.01
          ),

          textInput(
            inputId = ns('threads'),label = 'threads',value = 6
          ),

          textInput(
            inputId = ns('binSize'),label = 'binSize',value = 0.025
          ),

          textInput(
            inputId = ns('bw'),label = 'bw',value = 5
          ),

          textInput(
            inputId = ns('min_fraction'),label = 'min_fraction',value = 0.5
          ),

          selectInput(
            inputId = ns('out_put_peak'),label = 'figure output',choices = c("TRUE","FALSE"),selected = "TRUE",multiple = F
          ),

          selectInput(
            inputId = ns('fill_peaks'),label = 'fill_peaks',choices = c("TRUE","FALSE"),selected = "FALSE",multiple = F
          ),

          selectInput(
            inputId = ns('column'),label = 'column',choices = c("rp","hilic"),selected = "rp",multiple = F
          )
        )
      ),
      page_fluid(
        nav_panel(
          title = "File check",
          tags$h3("Check input MS files",style = 'color: black'),
          hr_head(),
          icon = bsicons::bs_icon("inbox"),
          actionButton(ns('action1'),'1. Check input file',icon = icon("computer-mouse"),width = "15%"),
          htmlOutput(ns("file_check1")),
          navset_card_tab(
            height = 350,
            full_screen = TRUE,
            title = "Input file list",
            nav_panel(
              "MS1",
              card_title(".mzxml file list (MS1)"),
              DT::dataTableOutput(ns("tbl_ms1"))
            )
          ),
          tags$h3("Optimize peak picking parameters (option)",style = 'color: black'),
          hr_head(),
          navset_card_tab(
            height = 700,
            full_screen = TRUE,
            title = "optimize peak picking parameters",
            nav_panel(
            title =    "Step1",
              card_header("Choose ppmCut"),
              layout_sidebar(
                sidebar = sidebar(
                  textInput(inputId = ns("massSDrange.1"),label = "massSDrange",value = 2),
                  textInput(inputId = ns("smooth.1"),label = "smooth",value = 0),
                  textInput(inputId = ns("cutoff.1"),label = "cutoff",value = 0.95),
                  textInput(inputId = ns("thread.1"),label = "thread",value = 5),
                  radioButtons(inputId = ns("filenum.1"),label = "filenum",choices = c(3,5,"all"),selected = 3),
                ),
                actionButton(ns('action3'),'Start',icon = icon("computer-mouse")),
                plotOutput(outputId = ns("ppmCut_plt"))
              )
            ),
            nav_panel(
            title =    "Step2",
              card_header("Recommanded parameters"),
                  layout_sidebar(
                    sidebar = sidebar(
                      textInput(inputId = ns("massSDrange.2"),label = "massSDrange",value = 2),
                      textInput(inputId = ns("smooth.2"),label = "smooth",value = 0),
                      textInput(inputId = ns("cutoff.2"),label = "cutoff",value = 0.95),
                      textInput(inputId = ns("thread.2"),label = "thread",value = 5),
                      textInput(inputId = ns("ppmCut.2"),label = "ppmCut",value = 7),
                      radioButtons(inputId = ns("filenum.2"),label = "filenum",choices = c(3,5,"all"),selected = 3),
                    ),
                    actionButton(ns('action4'),'Start',icon = icon("computer-mouse")),
                    DT::dataTableOutput(ns("parameters_opt")),
                    radioButtons(inputId = ns("para_choise"),label = "use optimized parameters",choices = c("yes","no"),selected = "yes")
                  )
            )
          ),
          tags$h3("Start peak picking",style = 'color: black'),
          hr_head(),
          actionButton(ns('action2'),'2. Star peak picking',icon = icon("computer-mouse"),width = "15%"),
          navset_card_tab(
            height = 350,
            full_screen = TRUE,
            title = "Status",
            nav_panel(
              "Positive",
              card_title("Positive model"),
              verbatimTextOutput(ns("obj_mass_check.pos"))
            ),
            nav_panel(
              "Negative",
              card_title("Negative model"),
              verbatimTextOutput(ns("obj_mass_check.neg"))
            )
          ),
          card(
            full_screen = T,
            height = 350,
            card_header(
              "Peak picking parameters"
            ),
            DT::dataTableOutput(ns("para_clean_tbl"))
          )
        )
      )
    )
    )
}

#' import from raw data of server
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs toggle runjs useShinyjs
#' @importFrom shinyFiles shinyDirChoose parseDirPath parseFilePaths
#' @importFrom massprocesser process_data
#' @importFrom massdataset mutate_ms2
#' @import patchwork
#' @param id module of server
#' @param volumes shinyFiles volumes
#' @param prj_init use project init variables.
#' @param data_import_rv reactivevalues mass_dataset out
#' @param data_export_rv reactivevalues mass_dataset export
#' @noRd
data_import_raw_server <- function(id, volumes, prj_init, data_import_rv, data_export_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ## 3.3 Import from raw data ----------------------------------------------------
    observe({
      shinyDirChoose(input = input, id = "MS1", roots = volumes, session = session)
      if (!is.null(input$MS1)) {
        ms1_folder_selected <- parseDirPath(roots = volumes, input$MS1)
        output$MS1_path <- renderText(ms1_folder_selected)
      }
    })

    para_data_check <- reactiveValues(data = NULL)

    # Main file processing observer
    observeEvent(input$action1, {
      tryCatch({
        req(input$MS1)
        req(prj_init$sample_info)

        # Get selected directory
        para_data_check$ms1_folder_selected <- parseDirPath(volumes, input$MS1)
        para_data_check$MS1_path <- as.character(para_data_check$ms1_folder_selected)

        # Check directory existence
        dir_pos <- dir.exists(file.path(para_data_check$MS1_path, "POS"))
        dir_neg <- dir.exists(file.path(para_data_check$MS1_path, "NEG"))

        # Validate directory structure
        if (!dir_pos && !dir_neg) {
          shinyalert("Error", "No POS/NEG directories found", type = "error")
          return()
        }

        # Mode detection alerts
        if (dir_pos && !dir_neg) {
          shinyalert("Information", "Only positive mode data detected", type = "info")
        } else if (!dir_pos && dir_neg) {
          shinyalert("Information", "Only negative mode data detected", type = "info")
        }

        # Initialize file lists
        para_data_check$QC_number.n <- NULL
        para_data_check$QC_number.p <- NULL
        para_data_check$S_number.n <- NULL
        para_data_check$S_number.p <- NULL

        # POS mode processing
        if (dir_pos) {
          qc_pos_path <- file.path(para_data_check$MS1_path, "POS", "QC")
          subj_pos_path <- file.path(para_data_check$MS1_path, "POS", "Subject")

          if (dir.exists(qc_pos_path)) {
            para_data_check$QC_number.p <- list.files(qc_pos_path,pattern = ".mzXML$")
          } else {
            shinyalert("Information", "No QC directory found in POS mode", type = "info")
          }

          if (dir.exists(subj_pos_path)) {
            para_data_check$S_number.p <- list.files(subj_pos_path,pattern = ".mzXML$")
          } else {
            shinyalert("Information", "No Subject directory found in POS mode", type = "info")
          }
        }

        # NEG mode processing
        if (dir_neg) {
          qc_neg_path <- file.path(para_data_check$MS1_path, "NEG", "QC")
          subj_neg_path <- file.path(para_data_check$MS1_path, "NEG", "Subject")

          if (dir.exists(qc_neg_path)) {
            para_data_check$QC_number.n <- list.files(qc_neg_path,pattern = ".mzXML$")
          } else {
            shinyalert("Information", "No QC directory found in NEG mode", type = "info")
          }

          if (dir.exists(subj_neg_path)) {
            para_data_check$S_number.n <- list.files(subj_neg_path,pattern = ".mzXML$")
          } else {
            shinyalert("Information", "No Subject directory found in NEG mode", type = "info")
          }
        }

        # Validate files for existing modes
        validation_passed <- TRUE
        if (dir_pos) validation_passed <- validation_passed && validate_sample_files(
          mode = "POS",
          QC_num.p = para_data_check$QC_number.p,
          S_num.p = para_data_check$S_number.p,
          QC_num.n = para_data_check$QC_number.n,
          S_num.n = para_data_check$S_number.n,
          sample_info_temp = prj_init$sample_info)
        if (dir_neg) validation_passed <- validation_passed && validate_sample_files(
          mode = "NEG",
          QC_num.p = para_data_check$QC_number.p,
          S_num.p = para_data_check$S_number.p,
          QC_num.n = para_data_check$QC_number.n,
          S_num.n = para_data_check$S_number.n,
          sample_info_temp = prj_init$sample_info)
        if (!validation_passed) return()

        # File correspondence check when both modes exist
        if (dir_pos && dir_neg) {
          # QC file count check
          if (length(para_data_check$QC_number.p) != length(para_data_check$QC_number.n)) {
            shinyalert("Warning",
                       "Mismatch in QC file counts between POS and NEG modes",
                       type = "warning")
          }

          # Subject file count check
          if (length(para_data_check$S_number.p) != length(para_data_check$S_number.n)) {
            shinyalert("Warning",
                       "Mismatch in Subject file counts between POS and NEG modes",
                       type = "warning")
          }
        }
      }, error = function(e) {
        shinyalert("Error", paste("File validation failed:", e$message), type = "error")
      })
    })

    ## Parameter optimization ------------------------------------------------------
    data_para_opt <- reactiveValues(data = NULL)

    observeEvent(input$action3, {
      tryCatch({
        req(input$MS1)
        req(para_data_check$MS1_path)

        dir_pos <- dir.exists(file.path(para_data_check$MS1_path, "POS"))
        dir_neg <- dir.exists(file.path(para_data_check$MS1_path, "NEG"))

        # POS mode processing
        if (dir_pos) {
          temp_qc_num.pos <- length(para_data_check$QC_number.p)
          temp_dir_path.pos <- if (temp_qc_num.pos == 0) {
            file.path(para_data_check$MS1_path, "POS", "Subject/")
          } else {
            file.path(para_data_check$MS1_path, "POS", "QC/")
          }

          if (file.exists(file.path(temp_dir_path.pos, "ppmCut.xlsx"))) {
            data_para_opt$ppmCut.p <- readxl::read_xlsx(file.path(temp_dir_path.pos, "ppmCut.xlsx")) %>%
              dplyr::pull(ppmCut) %>% as.numeric()
          } else {
            withProgress(message = 'Testing POS ppm cutoff', value = 0, {
              massSDrange <- as.numeric(input$massSDrange.1)
              smooth <- as.numeric(input$smooth.1)
              cutoff <- as.numeric(input$cutoff.1)
              filenum <- as.numeric(input$filenum.1)

              data_para_opt$step1.p <- paramounter_part1(
                directory = temp_dir_path.pos,
                massSDrange = massSDrange,
                smooth = smooth,
                cutoff = cutoff,
                filenum = filenum,
                thread = as.numeric(input$thread.1)
              )

              data_para_opt$ppmCut.p <- data_para_opt$step1.p$ppmCut
              writexl::write_xlsx(
                data.frame(ion = "positive", ppmCut = data_para_opt$ppmCut.p),
                file.path(temp_dir_path.pos, "ppmCut.xlsx")
              )
            })
          }
        }

        # NEG mode processing
        if (dir_neg) {
          temp_qc_num.neg <- length(para_data_check$QC_number.n)
          temp_dir_path.neg <- if (temp_qc_num.neg == 0) {
            file.path(para_data_check$MS1_path, "NEG", "Subject/")
          } else {
            file.path(para_data_check$MS1_path, "NEG", "QC/")
          }

          if (file.exists(file.path(temp_dir_path.neg, "ppmCut.xlsx"))) {
            data_para_opt$ppmCut.n <- readxl::read_xlsx(file.path(temp_dir_path.neg, "ppmCut.xlsx")) %>%
              dplyr::pull(ppmCut) %>% as.numeric()
          } else {
            withProgress(message = 'Testing NEG ppm cutoff', value = 0, {
              massSDrange <- as.numeric(input$massSDrange.1)
              smooth <- as.numeric(input$smooth.1)
              cutoff <- as.numeric(input$cutoff.1)
              filenum <- as.numeric(input$filenum.1)

              data_para_opt$step1.n <- paramounter_part1(
                directory = temp_dir_path.neg,
                massSDrange = massSDrange,
                smooth = smooth,
                cutoff = cutoff,
                filenum = filenum,
                thread = as.numeric(input$thread.1)
              )

              data_para_opt$ppmCut.n <- data_para_opt$step1.n$ppmCut
              writexl::write_xlsx(
                data.frame(ion = "negative", ppmCut = data_para_opt$ppmCut.n),
                file.path(temp_dir_path.neg, "ppmCut.xlsx")
              )
            })
          }
        }

        # Plot generation
        output$ppmCut_plt <- renderPlot({
          plt_list <- list()
          if (dir_pos && !is.null(data_para_opt$step1.p)) {
            plt_list$pos <- data_para_opt$step1.p$plot + ggtitle("POS Mass Tolerance")
          }
          if (dir_neg && !is.null(data_para_opt$step1.n)) {
            plt_list$neg <- data_para_opt$step1.n$plot + ggtitle("NEG Mass Tolerance")
          }
          if (length(plt_list) > 0) patchwork::wrap_plots(plt_list)
        })

      }, error = function(e) {
        shinyalert("Error", paste("Parameter optimization failed:", e$message), type = "error")
      })
    })

    ## Data processing ------------------------------------------------------------
    observeEvent(input$action2, {
      tryCatch({
        req(input$MS1)
        req(para_data_check$MS1_path)

        dir_pos <- dir.exists(file.path(para_data_check$MS1_path, "POS"))
        dir_neg <- dir.exists(file.path(para_data_check$MS1_path, "NEG"))

        # Parameter preparation
        data_import_rv$parameters <- data.frame(
          para = c("ppm","threads","snthresh","noise",
                   "min_fraction","p_min","p_max","pre_left",
                   "pre_right","fill_peaks","fitgauss","integrate",
                   "mzdiff","binSize","bw","out_put_peak","column"),
          default = c(input$ppm, input$threads, input$snthresh, input$noise,
                      input$min_fraction, input$p_min, input$p_max, input$pre_left,
                      input$pre_right, input$fill_peaks, input$fitgauss, input$integrate,
                      input$mzdiff, input$binSize, input$bw, input$out_put_peak, input$column)
        )

        # Process data function
        process_data_fun <- function(path, polarity, parameters) {
          # Parameter selection logic
          n <- if (ncol(parameters) == 2) 2 else
            if (polarity == "positive") 4 else 5

          # Parameter assignment
          process_params <- list(
            path = path,
            polarity = polarity,
            ppm = as.numeric(parameters[1, n]),
            peakwidth = c(as.numeric(parameters[6, n]), as.numeric(parameters[7, n])),
            snthresh = as.numeric(parameters[3, n]),
            prefilter = c(as.numeric(parameters[8, n]), as.numeric(parameters[9, n])),
            fitgauss = as.logical(parameters[11, n]),
            integrate = as.numeric(parameters[12, n]),
            mzdiff = as.numeric(parameters[13, n]),
            noise = as.numeric(parameters[4, n]),
            threads = as.numeric(parameters[2, n]),
            binSize = as.numeric(parameters[14, n]),
            bw = as.numeric(parameters[15, n]),
            output_tic = as.logical(parameters[16, n]),
            output_bpc = as.logical(parameters[16, n]),
            output_rt_correction_plot = as.logical(parameters[16, n]),
            min_fraction = as.numeric(parameters[5, n]),
            fill_peaks = as.logical(parameters[10, n])
          )

          do.call(massprocesser::process_data, process_params)
        }

        withProgress(message = 'Peak picking', value = 0, {
          steps <- character(0)
          if (dir_pos) steps <- c(steps, 'Processing POS mode...')
          if (dir_neg) steps <- c(steps, 'Processing NEG mode...')
          steps <- c(steps, 'Finalizing...')
          total_steps <- length(steps)

          current_step <- 0

          # Process POS mode
          if (dir_pos) {
            current_step <- current_step + 1
            incProgress(current_step/total_steps, detail = steps[current_step])
            process_data_fun(
              path = file.path(para_data_check$MS1_path, "POS"),
              polarity = "positive",
              parameters = data_import_rv$parameters
            )
          }

          # Process NEG mode
          if (dir_neg) {
            current_step <- current_step + 1
            incProgress(current_step/total_steps, detail = steps[current_step])
            process_data_fun(
              path = file.path(para_data_check$MS1_path, "NEG"),
              polarity = "negative",
              parameters = data_import_rv$parameters
            )
          }

          # Save results
          current_step <- current_step + 1
          incProgress(current_step/total_steps, detail = steps[current_step])

          mass_dataset_dir <- file.path(prj_init$wd, "mass_dataset")
          dir.create(mass_dataset_dir, showWarnings = FALSE, recursive = TRUE)

          # Handle POS results
          if (dir_pos) {
            pos_result_path <- file.path(para_data_check$MS1_path, "POS/Result/object")
            if (file.exists(pos_result_path)) {
              load(pos_result_path)
              print(object)
              data_import_rv$object_pos_raw <- object
              object_pos_raw <- object
              print(object_pos_raw)
              save(object_pos_raw,
                   file = file.path(mass_dataset_dir, "object_pos_raw.rda"))
            }
          }

          # Handle NEG results
          if (dir_neg) {
            data_import_rv$object_neg_raw <- NULL
            neg_result_path <- file.path(para_data_check$MS1_path, "NEG/Result/object")
            if (file.exists(neg_result_path)) {
              load(neg_result_path)
              data_import_rv$object_neg_raw <- object
              object_neg_raw <- object
              save(object_neg_raw,
                   file = file.path(mass_dataset_dir, "object_neg_raw.rda"))
            }
          }
        })

        # Update UI feedback
        output$file_check1 <- renderUI({
          success_msg <- "Processing completed successfully!"
          if (dir_pos && dir_neg) {
            success_msg <- paste(success_msg, "Both modes processed.")
          } else if (dir_pos) {
            success_msg <- paste(success_msg, "Positive mode processed.")
          } else if (dir_neg) {
            success_msg <- paste(success_msg, "Negative mode processed.")
          }

          HTML(paste0(
            '<div class="alert alert-success" role="alert">',
            '<i class="fas fa-check-circle"></i> ', success_msg,
            '</div>'
          ))
        })
        output$obj_mass_check.pos = renderPrint({
          if(is.null(data_import_rv$object_pos_raw)){return()}
          print(data_import_rv$object_pos_raw)
        })
        output$obj_mass_check.neg = renderPrint({
          if(is.null(data_import_rv$object_neg_raw)){return()}
          print(data_import_rv$object_neg_raw )
        })
      }, error = function(e) {
        shinyalert("Error", paste("Data processing failed:", e$message), type = "error")
      })
    })

  })
  }

