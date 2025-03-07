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
    icon = bs_icon("envelope-open-heart"),
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
            class = NULL,
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
          )
        ),
          actionButton(inputId = ns('enrich_start'),label = "Start enrichment",icon = icon("play")),
        )
      ),
      page_fluid(
        nav_panel(title = "Feature annotation",
                  navset_card_tab(
                    title = "Enrichment table",
                    height = 400,
                    full_screen = TRUE,
                    nav_panel(
                      "Enrichment Table",
                      DT::dataTableOutput(outputId = ns("Annotation_pos"))
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
#' @import metpath
#' @import plantmdb
#' @param id module of server
#' @param volumes shinyFiles volumes
#' @param prj_init use project init variables.
#' @param data_import_rv reactivevalues mass_dataset export
#' @param data_clean_rv reactivevalues p2 dataclean
#' @param data_export_rv reactivevalues mass_dataset export
#' @param data_anno reactivevalues data annotation
#' @noRd


enrichment_server <- function(id,volumes,prj_init,data_import_rv,data_clean_rv,data_anno,data_export_rv) {
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

        # check ms2
        if (length(data_anno$object_pos.norm@ms2_data) == 0 || length(data_anno$object_neg.norm@ms2_data) == 0) {
          showNotification(
            "MS2 data was not detected. Annotations will be based on MS1 data only.",
            type = "warning"
          )
        }

        para = para_anno()
        print(para)
        print(data_anno$object_neg.norm)
        print(data_anno$object_pos.norm)

        ## buildin database

        data_anno$buildin_db <-
          list(
            MoNA = mona_database0.0.4,
            Massbank = massbank_database0.0.4,
            ReSpect = respect_database0.0.1,
            PlaSMA = plasma_database0.0.1,
            Orbitrap = orbitrap_database0.0.3,
            KEGG = kegg_plant_database0.0.1,
            KNApSAcK = knapsack_agri_database0.0.1,
            Ath_Cyc = ath_plantcyc.database0.0.1,
            MetaboBASE = metabobase_database0.0.1

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

