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


data_import_raw_server <- function(id,volumes,prj_init,data_import_rv,data_export_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## 3.3 import from raw data ----------------------------------------------------
    #> Import MS1 folder
    observe({
      shinyDirChoose(input = input,id = "MS1", roots =  volumes, session = session)
      if(!is.null(input$MS1)){
        # browser()
        ms1_folder_selected<-parseDirPath(roots = volumes, input$MS1)
        output$MS1_path <- renderText(ms1_folder_selected)
      }})


    #> File check
    #> ##> default
    output$file_check1 = renderUI({
      isolate(HTML(
        paste0(
          '<div class="info-block">',
          '  <div>',
          '    <span class="info-label">The number of QC files:</span>',
          '    <span class="info-value">',
          '      Positive model: <font color="red">(', ')</font>',
          '      Negative model: <font color="red">(', ')</font>',
          '    </span>',
          '  </div>',
          '  <div>',
          '    <span class="info-label">The number of Subject files:</span>',
          '    <span class="info-value">',
          '      Positive model: <font color="red">(',')</font>',
          '      Negative model: <font color="red">(',')</font>',
          '    </span>',
          '  </div>',
          '</div>'
        )
      ))
    })
    para_data_check <- reactiveValues(data = NULL)
    observeEvent(
      input$action1,
      {
        if(is.null(input$MS1)){return()}
        if(is.null(prj_init$sample_info)){return()}

        #> MS1 data file
        para_data_check$ms1_folder_selected <- parseDirPath(volumes, input$MS1)
        para_data_check$MS1_path <- para_data_check$ms1_folder_selected |> as.character()
        para_data_check$QC_number.n <- list.files(paste0(para_data_check$MS1_path,"/NEG/QC"))
        para_data_check$QC_number.p <- list.files(paste0(para_data_check$MS1_path,"/POS/QC"))
        para_data_check$S_number.n <- list.files(paste0(para_data_check$MS1_path,"/NEG/Subject"))
        para_data_check$S_number.p <- list.files(paste0(para_data_check$MS1_path,"/POS/Subject"))

        temp_tbl_ms1 = data.frame(
          FileName = c(para_data_check$QC_number.n,
                       para_data_check$QC_number.p,
                       para_data_check$S_number.n,
                       para_data_check$S_number.p),
          Type = rep(c('QC_neg','QC_pos','Subject_neg','Subject_pos'),
                     c(length(para_data_check$QC_number.n),
                       length(para_data_check$QC_number.p),
                       length(para_data_check$S_number.n),
                       length(para_data_check$S_number.p)))
        )
        #> MS1 file tbl
        output$tbl_ms1 =
          renderDataTable_formated(actions = input$action1,
                                   condition1 = para_data_check$QC_number.n,
                                   condition2 = para_data_check$QC_number.p,
                                   condition3 = para_data_check$S_number.n,
                                   condition4 = para_data_check$S_number.p,
                                   tbl = temp_tbl_ms1,filename.a = "3.3.rawDataImport_summary_of_ms1_file")
        output$file_check1 = renderUI({
          isolate(HTML(
            paste0(
              '<div class="info-block">',
              '  <div>',
              '    <span class="info-label">The number of QC files:</span>',
              '    <span class="info-value">',
              '      Positive model: <font color="red">(', para_data_check$QC_number.p |> length(),')</font>',
              '      Negative model: <font color="red">(', para_data_check$QC_number.n |> length(),')</font>',
              '    </span>',
              '  </div>',
              '  <div>',
              '    <span class="info-label">The number of Subject files:</span>',
              '    <span class="info-value">',
              '      Positive model: <font color="red">(', para_data_check$S_number.p |> length(),')</font>',
              '      Negative model: <font color="red">(', para_data_check$S_number.n |> length(),')</font>',
              '    </span>',
              '  </div>',
              '</div>'
            )
          ))
        })
      }
    )
    #> optimize parameter
    #>
    data_para_opt <- reactiveValues(data = NULL)
    observeEvent(
      input$action3,
      {
        if(is.null(input$MS1)){return()}
        if(is.null(para_data_check$MS1_path)){return()}
        pro_optimize_step1 = c(
          'running positive model ...',
          'running negative model ...',
          'All finish'
        )
        temp_qc_num.pos = para_data_check$QC_number.p |> length()
        temp_qc_num.neg = para_data_check$QC_number.n |> length()
        if(temp_qc_num.pos == 0 ) {
          temp_dir_path.pos = paste0(para_data_check$MS1_path,"/POS/Subject/")
        } else {
          temp_dir_path.pos = paste0(para_data_check$MS1_path,"/POS/QC/")
        }
        if(temp_qc_num.neg == 0 ) {
          temp_dir_path.neg = paste0(para_data_check$MS1_path,"/NEG/Subject/")
        } else {
          temp_dir_path.neg = paste0(para_data_check$MS1_path,"/NEG/QC/")
        }
        if(file.exists(paste0(temp_dir_path.pos,"ppmCut.xlsx")) &
           file.exists(paste0(temp_dir_path.neg,"ppmCut.xlsx"))
           ){

          withProgress(message = 'Test ppm cutoff',value = 0,
                       expr = {
                         for (i in 1:3) {
                           incProgress(1/3,detail = pro_optimize_step1[i])
                           if(i == 1) {
                             data_para_opt$ppmCut.p = readxl::read_xlsx(paste0(temp_dir_path.pos,"ppmCut.xlsx")) %>% pull(ppmCut) %>% as.numeric()
                           } else if(i == 2) {
                             data_para_opt$ppmCut.n = readxl::read_xlsx(paste0(temp_dir_path.neg,"ppmCut.xlsx")) %>% pull(ppmCut) %>% as.numeric()
                           } else if (i == 3) {
                             print("already done, use previous saved ppmCut")
                           }
                         }
                         })

        } else {
          massSDrange = input$massSDrange.1 %>% as.numeric()
          smooth = input$smooth.1 %>% as.numeric()
          cutoff = input$cutoff.1 %>% as.numeric()
          filenum = input$filenum.1 %>% as.numeric()
          withProgress(message = 'Test ppm cutoff',value = 0,
                       expr = {
                         for (i in 1:3) {
                           incProgress(1/3,detail = pro_optimize_step1[i])
                           if (i == 1) {
                             data_para_opt$step1.p = paramounter_part1(
                               directory = temp_dir_path.pos,
                               massSDrange = massSDrange,
                               smooth = smooth,
                               cutoff = cutoff,
                               filenum = filenum,
                               thread = input$thread.1 %>% as.numeric()
                             )
                           } else if(i == 2) {
                             data_para_opt$step1.n = paramounter_part1(
                               directory = temp_dir_path.neg,
                               massSDrange = massSDrange,
                               smooth = smooth,
                               cutoff = cutoff,
                               filenum = filenum,
                               thread = input$thread.1 %>% as.numeric()
                             )
                           } else if(i == 3) {
                             data_para_opt$ppmCut.p = data_para_opt$step1.p$ppmCut
                             writexl::write_xlsx(
                               data.frame(ion = "positive",ppmCut = data_para_opt$ppmCut.p),paste0(para_data_check$MS1_path,"/POS/QC/ppmCut.xlsx")
                             )
                             data_para_opt$ppmCut.n = data_para_opt$step1.n$ppmCut
                             writexl::write_xlsx(
                               data.frame(ion = "negative",ppmCut = data_para_opt$ppmCut.n),paste0(para_data_check$MS1_path,"/NEG/QC/ppmCut.xlsx")
                             )
                             plt.p = data_para_opt$step1.p$plot + ggtitle("Mass tolerance in positive model")
                             plt.n = data_para_opt$step1.n$plot + ggtitle("Mass tolerance in negative model")
                           }
                         }
                       })
          output$ppmCut_plt = renderPlot({
            if(is.null(data_para_opt$step1.p)) {return()}
            plt.p + plt.n
          })
        }

      }
    )
    observeEvent(
      input$action4,
      {
        if(is.null(input$MS1)){return()}
        if(is.null(para_data_check$MS1_path)){return()}
        pro_optimize_step2 = c(
          'running positive model ...',
          'running negative model ...',
          'All finish'
        )
        temp_qc_num.pos = para_data_check$QC_number.p |> length()
        temp_qc_num.neg = para_data_check$QC_number.n |> length()
        if(temp_qc_num.pos == 0 ) {
          temp_dir_path.pos = paste0(para_data_check$MS1_path,"/POS/Subject/")
        } else {
          temp_dir_path.pos = paste0(para_data_check$MS1_path,"/POS/QC/")
        }
        if(temp_qc_num.neg == 0 ) {
          temp_dir_path.neg = paste0(para_data_check$MS1_path,"/NEG/Subject/")
        } else {
          temp_dir_path.neg = paste0(para_data_check$MS1_path,"/NEG/QC/")
        }
        if(file.exists(paste0(temp_dir_path.pos,"parameters.xlsx")) &
           file.exists(paste0(temp_dir_path.neg,"parameters.xlsx"))
        ) {
          withProgress(message = 'optimize parameter',value = 0,
                       expr = {
                         for (i in 1:3) {
                           incProgress(1/3,detail = pro_optimize_step2[i])
                           if(i == 1) {
                             data_para_opt$step2.p = readxl::read_xlsx(paste0(temp_dir_path.pos,"parameters.xlsx"))
                           } else if(i == 2) {
                             data_para_opt$step2.n = readxl::read_xlsx(paste0(temp_dir_path.neg,"parameters.xlsx"))
                           } else if (i == 3) {
                             print("already done, use previous saved parameters")
                           }
                         }
                       })

      } else {
        massSDrange = input$massSDrange.2 %>% as.numeric()
        smooth = input$smooth.2 %>% as.numeric()
        cutoff = input$cutoff.2 %>% as.numeric()
        filenum = input$filenum.2 %>% as.numeric()

        if(is.null(data_para_opt$ppmCut.p)) {
          data_para_opt$ppmCut.p = input$ppmCut.2 %>% as.numeric()
        }
        if(is.null(data_para_opt$ppmCut.n)) {
          data_para_opt$ppmCut.n = input$ppmCut.2 %>% as.numeric()
        }
       print(temp_dir_path.pos)
       print(temp_dir_path.neg)
        withProgress(message = 'optimize parameter',value = 0,
                     expr = {
                       for (i in 1:3) {
                         incProgress(1/3,detail = pro_optimize_step2[i])
                         if (i == 1) {
                           data_para_opt$step2.p = paramounter_part2(
                             directory = temp_dir_path.pos,
                             massSDrange = massSDrange,
                             smooth = smooth,
                             cutoff = cutoff,
                             filenum = filenum,
                             thread = input$thread.2 %>% as.numeric(),
                             ppmCut = data_para_opt$ppmCut.p
                           ) %>% dplyr::rename("Positive" = "Value")

                         } else if(i == 2) {
                           data_para_opt$step2.n = paramounter_part2(
                             directory = temp_dir_path.neg,
                             massSDrange = massSDrange,
                             smooth = smooth,
                             cutoff = cutoff,
                             filenum = filenum,
                             ppmCut = data_para_opt$ppmCut.n,
                             thread = input$thread.2 %>% as.numeric()
                           ) %>% dplyr::rename("Negative" = "Value")
                         } else if (i == 3) {
                           writexl::write_xlsx(data_para_opt$step2.p,paste0(temp_dir_path.pos,"parameters.xlsx"))
                           writexl::write_xlsx(data_para_opt$step2.n,paste0(temp_dir_path.neg,"parameters.xlsx"))
                         }
                       }
                     })
      }
        data_para_opt$out_tbl = left_join(data_para_opt$step2.p,data_para_opt$step2.n)

        output$parameters_opt = renderDataTable_formated(
          actions = input$action4,
          condition1 = data_para_opt$step2.p,
          condition2 = data_para_opt$step2.n,
          filename.a = "optimized_parameters",
          tbl = data_para_opt$out_tbl
        )

      }
    )

    para_choise = reactive({
      input$para_choise %>% as.character()
    })



    #>peak picking =========================================
    observeEvent(
      input$action2,
      {
        if(is.null(input$MS1)){return()}
        if(is.null(para_data_check$MS1_path)){return()}
        para_choise = para_choise()

        pro_step = c('running positive model ...',
                     'running negative model ...',
                     'All finish!')
         print("check point 1")

        data_import_rv$parameters =
          data.frame(
            para = c("ppm","threads","snthresh","noise",
                     "min_fraction","p_min","p_max","pre_left",
                     "pre_right","fill_peaks","fitgauss","integrate",
                     "mzdiff","binSize","bw","out_put_peak","column"),
            default = c(input$ppm,input$threads,input$snthresh,input$noise,
                        input$min_fraction,input$p_min,input$p_max,input$pre_left,
                        input$pre_right,input$fill_peaks,input$fitgauss,input$integrate,
                        input$mzdiff,input$binSize,input$bw,input$out_put_peak,input$column)
          )

         print('check point 2')


        if(para_choise == "yes" & !is.null(data_para_opt$out_tbl)) {
          data_import_rv$parameters =
          data_import_rv$parameters %>%
            dplyr::left_join(data_para_opt$out_tbl,by = "para") %>%
            dplyr::select(para,desc,default,Positive,Negative) %>%
            mutate(
              default = as.character(default),
              Positive = as.character(Positive),
              Negative = as.character(Negative),
            ) %>%
            dplyr::mutate(
              Positive = case_when(
                is.na(Positive) ~ default,
                TRUE ~ Positive
              ),
              Negative = case_when(
                is.na(Negative) ~ default,
                TRUE ~ Negative
              )
            )
        }
        print("Check point 3")
        print(data_import_rv$parameters)


        #> function
        process_data_fun = function(path,polarity,parameters){
          if(ncol(parameters) == 2) {
            n = 2
          } else if(ncol(parameters) == 5) {
            if(polarity == "positive") {n = 4} else if(polarity == "negative") {n = 5}
          }
          data_import_rv$ppm = as.numeric(data_import_rv$parameters[1,n])
          data_import_rv$threads = as.numeric(data_import_rv$parameters[2,n])
          data_import_rv$snthresh = as.numeric(data_import_rv$parameters[3,n])
          data_import_rv$noise = as.numeric(data_import_rv$parameters[4,n])
          data_import_rv$min_fraction =as.numeric(data_import_rv$parameters[5,n])
          data_import_rv$p_min = as.numeric(data_import_rv$parameters[6,n])
          data_import_rv$p_max = as.numeric(data_import_rv$parameters[7,n])
          data_import_rv$pre_left = as.numeric(data_import_rv$parameters[8,n])
          data_import_rv$pre_right = as.numeric(data_import_rv$parameters[9,n])
          data_import_rv$fill_peaks = as.logical(data_import_rv$parameters[10,n])
          data_import_rv$fitgauss = as.logical(data_import_rv$parameters[11,n])
          data_import_rv$integrate = as.numeric(data_import_rv$parameters[12,n])
          data_import_rv$mzdiff = as.numeric(data_import_rv$parameters[13,n])
          data_import_rv$binSize = as.numeric(data_import_rv$parameters[14,n])
          data_import_rv$bw = as.numeric(data_import_rv$parameters[15,n])
          data_import_rv$out_put_peak = as.logical(data_import_rv$parameters[16,n])
          data_import_rv$column = as.character(data_import_rv$parameters[17,n])
          process_data(
            path = path,
            polarity = polarity,
            ppm = data_import_rv$ppm,
            peakwidth = c(data_import_rv$p_min, data_import_rv$p_max),
            snthresh = data_import_rv$snthresh,
            prefilter = c(data_import_rv$pre_left , data_import_rv$pre_right),
            fitgauss = data_import_rv$fitgauss,
            integrate = data_import_rv$integrate,
            mzdiff = data_import_rv$mzdiff,
            noise = data_import_rv$noise,
            threads = data_import_rv$threads,
            binSize = data_import_rv$binSize,
            bw = data_import_rv$bw,
            output_tic = data_import_rv$out_put_peak,
            output_bpc = data_import_rv$out_put_peak,
            output_rt_correction_plot = data_import_rv$out_put_peak,
            min_fraction = data_import_rv$min_fraction,
            fill_peaks = data_import_rv$fill_peaks,
     #       group_for_figure = "QC"
          )
        }
        withProgress(message = 'Peak picking', value = 0,
                     expr = {
                       for (i in 1:4) {
                         incProgress(1/4, detail = pro_step[i])
                         if(i == 1) {
                           process_data_fun(
                             path = paste0(para_data_check$MS1_path,"/POS"),polarity = "positive",parameters = data_import_rv$parameters
                           )
                         } else if(i == 2) {
                           process_data_fun(
                             path = paste0(para_data_check$MS1_path,"/NEG"),polarity = "negative",parameters = data_import_rv$parameters
                           )
                         } else if(i == 3){
                           load(paste0(para_data_check$MS1_path,"/POS/Result/object"))
                           data_import_rv$object_pos <- object
                           save_massobj(
                             polarity = 'positive',
                             file_path = paste0(prj_init$wd,"/Result/POS/Objects/"),
                             stage = 'step1',obj = data_import_rv$object_pos)
                           rm(object)
                           load(paste0(para_data_check$MS1_path,"/NEG/Result/object"))
                           data_import_rv$object_neg <- object
                           save_massobj(
                             polarity = 'negative',
                             file_path = paste0(prj_init$wd,"/Result/NEG/Objects/"),
                             stage = 'step1',
                             obj = data_import_rv$object_neg)
                           rm(object)
                         } else {}
                       }
                     }
        )
        output$peak_result_path = renderUI({
          isolate(HTML(paste0(
            '<font color=blue><b>Positive model:</font></b> <a href="',
            paste0("file://", gsub(" ", "%20", prj_init$wd), "/Result/POS"), '"> ',
            paste0(prj_init$wd,"/Result/POS"), '</a>',
            br(),
            '<font color=blue><b>Negative model:</font></b> <a href="',
            paste0("file://", gsub(" ", "%20", prj_init$wd), "/Result/NEG"), '"> ',
            paste0(prj_init$wd,"/Result/NEG"), '</a>'
          )))
        })

        ##> update sample information
        data_import_rv$object_pos <-
        data_import_rv$object_pos %>%
          activate_mass_dataset("sample_info") %>%
          dplyr::select("sample_id") %>% left_join(
            prj_init$sample_info,by = "sample_id"
          )

        data_import_rv$object_neg <-
          data_import_rv$object_neg %>%
          activate_mass_dataset("sample_info") %>%
          dplyr::select("sample_id") %>% left_join(
            prj_init$sample_info,by = "sample_id"
          )


        ##> export parameters
        output$para_clean_tbl = renderDataTable_formated(
          actions = input$action2,
          condition1 = input$MS1,
          condition2 = para_data_check$MS1_path,
          tbl = data_import_rv$parameters,filename.a = "3.3.rawDataImport_summary_of_parameters"
        )
        ##> status

        output$obj_mass_check.pos = renderPrint({
          print(data_import_rv$object_pos)
        })
        output$obj_mass_check.neg = renderPrint({
          print(data_import_rv$object_neg)
        })
      }
    )
  })
}

