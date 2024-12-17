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
              "MoNA","Massbank","ReSpect","PlaSMA","MetaboBASE","KEGG","KNApSAcK","Ath_Cyc","Orbitrap"
            ),selected = c(
              "MoNA","Massbank"
            ),multiple = T,
            title = "Select database"
          ),
          shinyDirButton(id = ns("norm_customized_db"), label = "Choose folder",
                         title = "Customized database path:",
                         buttonType = "default", class = NULL,
                         icon = bs_icon("folder"), multiple = FALSE),
          tags$span(textOutput(outputId = ns("ms_db_folder_selected")), class = "text-wrap"),
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
        if(is.null(input$MS2)){return()}
        if(!is.null(prj_init$object_negative.init) & !is.null(prj_init$object_positive.init) & prj_init$steps == "Annotation"){
          data_anno$object_neg.norm= prj_init$object_negative.init
          data_anno$object_pos.norm = prj_init$object_positive.init
        } else {
          if(is.null(data_clean_rv$object_neg.norm)){return()}
          if(is.null(data_clean_rv$object_pos.norm)){return()}
          data_anno$object_neg.norm = data_clean_rv$object_neg.norm
          data_anno$object_pos.norm = data_clean_rv$object_pos.norm
        }
        para = para_ms2()
        data_anno$ms2_folder_selected <- parseDirPath(volumes, input$MS2)
        data_anno$MS2_path <- data_anno$ms2_folder_selected |> as.character()
        steps = c(
          'running positive model',
          'running negative model',
          'All finish'
        )
        withProgress(message = 'Add MS2', value = 0,
                     expr = {
                       for (i in 1:3) {
                         incProgress(1/3,detail = steps[i])
                         if(i == 1) {
                           data_anno$object_pos.norm <-
                             mutate_ms2(
                               object = data_anno$object_pos.norm,
                               polarity = 'positive',
                               column = para$column,
                               ms1.ms2.match.rt.tol = para$ms1.ms2.match.rt.tol,
                               ms1.ms2.match.mz.tol = para$ms1.ms2.match.mz.tol,
                               path = paste0(data_anno$MS2_path,"/POS/")
                             )
                           save_massobj(
                             polarity = 'positive',
                             file_path = paste0(prj_init$wd,"/Result/POS/Objects/"),
                             stage = 'norm',obj = data_anno$object_pos.norm)

                         } else if(i ==2) {
                           data_anno$object_neg.norm <-
                             mutate_ms2(
                               object = data_anno$object_neg.norm,
                               polarity = 'negative',
                               column = para$column,
                               ms1.ms2.match.rt.tol = para$ms1.ms2.match.rt.tol,
                               ms1.ms2.match.mz.tol = para$ms1.ms2.match.mz.tol,
                               path = paste0(data_anno$MS2_path,"/NEG/")
                             )
                           save_massobj(
                             polarity = 'negative',
                             file_path = paste0(prj_init$wd,"/Result/NEG/Objects/"),
                             stage = 'norm',obj = data_anno$object_neg.norm)
                         }

                       }
                     })

      }
    )
    ##> Annotation parameters
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
        if(is.null(data_anno$object_neg.norm) | is.null(data_anno$object_neg.norm)) {
          if(!is.null(prj_init$object_negative.init) & !is.null(prj_init$object_positive.init) & prj_init$steps == "Annotation"){
            data_anno$object_neg.norm= prj_init$object_negative.init
            data_anno$object_pos.norm = prj_init$object_positive.init
          } else {
            if(is.null(data_clean_rv$object_neg.norm)){return()}
            if(is.null(data_clean_rv$object_pos.norm)){return()}
            data_anno$object_neg.norm = data_clean_rv$object_neg.norm
            data_anno$object_pos.norm = data_clean_rv$object_pos.norm
          }
        }
        para = para_anno()
        print(para)
        print(data_anno$object_neg.norm)
        print(data_anno$object_pos.norm)

        ## buildin database
        data_anno$buildin_db <-
          list(
            MoNA = plantmdb::mona_database0.0.4,
            Massbank = plantmdb::massbank_database0.0.4,
            ReSpect = plantmdb::respect_database0.0.1,
            PlaSMA = plantmdb::plasma_database0.0.1,
            Orbitrap = plantmdb::orbitrap_database0.0.3,
            KEGG = plantmdb::kegg_plant_database0.0.1,
            KNApSAcK = plantmdb::knapsack_agri_database0.0.1,
            Ath_Cyc = plantmdb::ath_plantcyc.database0.0.1,
            MetaboBASE = plantmdb::metabobase_database0.0.1

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
        dir.create(path = paste0(prj_init$wd,"/Result/Database/"),showWarnings = F,recursive = T)
        temp_db <- data_anno$db
        data_clean_rv$db <- data_anno$db
        save(temp_db,file =  paste0(prj_init$wd,"/Result/Database/auto_saved.dblist"))

        #> annotation
        if(length(data_anno$db) == 0) {
          output$anno_check1_pos = renderUI({
            isolate(HTML(paste0(
              '<font color = red>"No database were selected!"<b></font></b>',
            )))
          })
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
                             data_anno$object_neg.anno = annotate_metabolites_mass_dataset(
                               object = data_anno$object_neg.norm,
                               polarity = "negative",
                               database = data_anno$db[[i]] ,
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
                             data_anno$object_pos.anno = annotate_metabolites_mass_dataset(
                               object = data_anno$object_pos.norm,
                               polarity = "positive",
                               database = data_anno$db[[i]] ,
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
                           } else if(i > 1 & i < anno_steps) {
                             data_anno$object_neg.anno = annotate_metabolites_mass_dataset(
                               object = data_anno$object_neg.anno,polarity = "negative",
                               database = data_anno$db[[i]] ,
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
                             data_anno$object_pos.anno = annotate_metabolites_mass_dataset(
                               object = data_anno$object_pos.anno,polarity = "positive",
                               database = data_anno$db[[i]] ,
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
                           } else if(i == anno_steps) {
                             Sys.sleep(2)
                           }
                         }
                       }

          )
          data_clean_rv$object_pos.anno = data_anno$object_pos.anno
          data_clean_rv$object_neg.anno = data_anno$object_neg.anno
          save_massobj(
            polarity = 'positive',
            file_path = paste0(prj_init$wd,"/Result/POS/Objects/"),
            stage = 'anno',
            obj = data_anno$object_pos.anno)

          save_massobj(
            polarity = 'negative',
            file_path = paste0(prj_init$wd,"/Result/NEG/Objects/"),
            stage = 'anno',
            obj = data_anno$object_neg.anno)


          #> information of mass datasets
          output$obj_anno.pos = renderPrint({
            print(data_anno$object_pos.anno)
          })
          output$obj_anno.neg = renderPrint({
            print(data_anno$object_neg.anno)
          })

          #> data table
          #>
          output$Annotation_pos = renderDataTable_formated(
            actions = input$anno_start,
            condition1 = data_anno$object_pos.anno,filename.a = "3.6.6.annotation_pos",
            tbl = data_anno$object_pos.anno %>% extract_annotation_table()
          )

          output$Annotation_neg = renderDataTable_formated(
            actions = input$anno_start,
            condition1 = data_anno$object_neg.anno,filename.a = "3.6.6.annotation_neg",
            tbl = data_anno$object_neg.anno %>% extract_annotation_table()
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

