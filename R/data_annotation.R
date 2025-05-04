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
          shinyDirButton(id = ns("MS2"), label = "Select MS2 folder" ,
                         title = "The MS2 file folder:",
                         buttonType = "default", class = NULL,
                         icon = bs_icon("folder"), multiple = FALSE),
          tags$span(textOutput(outputId = ns("MS2_path")), class = "text-wrap"),
          textInput(
            inputId = ns('column'),label = 'column',value = 'rp'
          ),
          textInput(
            inputId = ns('ms1.ms2.match.rt.tol'),label = 'ms1.ms2.match.rt.tol',value = 15
          ),
          textInput(
            inputId = ns('ms1.ms2.match.mz.tol'),label = 'ms1.ms2.match.mz.tol',value = 30
          ),
          actionButton(inputId = ns("add_ms2"),label = "start",icon = icon("play"))
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
            choices = c(
              "MoNA","Massbank","HMDB","NULL"
            ),selected = c(
              "MoNA","Massbank","HMDB"
            ),multiple = T,
            title = "Select database"
          ),
          shinyDirButton(id = ns("norm_customized_db"), label = "Choose folder",
                         title = "Customized database path:",
                         buttonType = "default", class = NULL,
                         icon = bs_icon("folder"), multiple = FALSE),
          tags$span(textOutput(outputId = ns("ms_db_folder_selected")), class = "text-wrap"),
          br(),
          actionButton(inputId = ns('anno_start'),label = "Start annotation",icon = icon("play")),
        )
      ),
      page_fluid(
        nav_panel(title = "Feature annotation",
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
#' @import plantmdb
#' @param id module of server
#' @param volumes shinyFiles volumes
#' @param prj_init use project init variables.
#' @param data_import_rv reactivevalues mass_dataset export
#' @param data_clean_rv reactivevalues p2 dataclean
#' @param data_export_rv reactivevalues mass_dataset export
#' @param data_anno reactivevalues data annotation
#' @noRd


feature_annotation_server <- function(id,volumes,prj_init,data_import_rv,data_clean_rv,data_anno,data_export_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    p2_dataclean <- reactiveValues(data = NULL)
    observe({
      shinyDirChoose(input = input,id = "MS2", roots =  volumes, session = session)
      if(!is.null(input$MS2)){
        # browser()
        ms2_folder_selected<-parseDirPath(roots = volumes, input$MS2)
        output$MS2_path <- renderText(ms2_folder_selected)
      }})
    observe({
      shinyDirChoose(input = input,id = "norm_customized_db", roots =  volumes, session = session)
      if(!is.null(input$norm_customized_db)){
        # browser()
        dblist_selected<-parseDirPath(roots = volumes, input$norm_customized_db)
        output$ms_db_folder_selected <- renderText(dblist_selected)
      }})

    # Utility functions ----
    check_ion_modes <- function(data_rv, prj) {
      list(
        has_pos = !is.null(data_rv$object_pos_norm) || !is.null(prj$object_positive.init),
        has_neg = !is.null(data_rv$object_neg_norm) || !is.null(prj$object_negative.init)
      )
    }

    # functions -----
    perform_add_ms2 = function(object,para,polarity) {
      tryCatch({
        if(polarity == "positive") {
          ms2_path = paste0(data_anno$MS2_path,"/POS/")
        } else if (polarity == "negative") {
          ms2_path = paste0(data_anno$MS2_path,"/NEG/")
        }
        res <- object %>%
          mutate_ms2(
            polarity = polarity,
            column = para$column,
            ms1.ms2.match.rt.tol = para$ms1.ms2.match.rt.tol,
            ms1.ms2.match.mz.tol = para$ms1.ms2.match.mz.tol,
            path = ms2_path
          )
      },error = function(e) {
        shinyalert("Add ms2 Error", paste("Error details:", e$message), type = "error")
        NULL
      })
    }


    ##> parameters ms2
    para_ms2 =  reactive({
      list(
        column = as.character(input$column),
        ms1.ms2.match.rt.tol = as.numeric(input$ms1.ms2.match.rt.tol),
        ms1.ms2.match.mz.tol = as.numeric(input$ms1.ms2.match.mz.tol)
      )
    })

    ##> step1 add ms2
    observeEvent(
      input$add_ms2,
      {

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
        if(prj_init$steps == "Annotation") {
          if(modes$has_pos) data_anno$object_pos <- prj_init$object_positive.init
          if(modes$has_neg) data_anno$object_neg <- prj_init$object_negative.init
        } else {
          if(modes$has_pos) data_anno$object_pos <- data_clean_rv$object_pos_norm
          if(modes$has_neg) data_anno$object_neg <- data_clean_rv$object_neg_norm
        }
        if(is.null(input$MS2)){return()}
        para = para_ms2()
        data_anno$ms2_folder_selected <- parseDirPath(volumes, input$MS2)
        data_anno$MS2_path <- data_anno$ms2_folder_selected |> as.character()
        steps = c(
          'running positive model',
          'running negative model',
          'All finish'
        )

        # Core processing
        withProgress(message = "Processing Add MS2 spectra...", value = 0, {
          para <- para_ms2()
          if (modes$has_pos) {
            incProgress(0.3, detail = "Processing positive mode")
            data_anno$object_pos <- perform_add_ms2(data_anno$object_pos, para,"positive")
            object_pos_ms2 <- data_anno$object_pos
            save(object_pos_ms2 ,
                 file = file.path(prj_init$mass_dataset_dir, "06.object_pos_ms2.rda"))
          }

          if (modes$has_neg) {
            incProgress(0.3, detail = "Processing negative mode")
            data_anno$object_neg <- perform_add_ms2(data_anno$object_neg, para,"negative")
            object_neg_ms2 <- data_anno$object_neg
            save(object_neg_ms2 ,
                 file = file.path(prj_init$mass_dataset_dir, "06.object_neg_ms2.rda"))
          }
        })
        if(!is.null(data_anno$object_pos) || !is.null(data_anno$object_neg)) {
          data_anno$ms2_status <- TRUE
          shinyalert::shinyalert(
            title = "MS2 spectra have been successfully added.",
            text = paste0("Please start compound annotation step."),
            html = TRUE,
            type = "success",
            animation = "pop"
          )
        } else {
          data_anno$ms2_status <- FALSE
        }


      }
    )
    ##> Annotation parameters
    ##>
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
        ##
        data_anno$buildin_name = para$norm_db %>% as.character()

        if(length(data_anno$buildin_name) == 0) {
          data_anno$buildin_db = NULL
        } else {
          temp_anno_idx = match(data_anno$buildin_name,names(data_anno$buildin_db))
          data_anno$buildin_db = data_anno$buildin_db[temp_anno_idx]
        }

        ## Customized ms database
        data_anno$norm_customized_db <- parseDirPath(volumes, input$norm_customized_db)
        data_anno$cuz_db_path <- data_anno$norm_customized_db %>% as.character()
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
  }
  )}

