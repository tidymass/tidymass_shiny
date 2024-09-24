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
          tags$span(textOutput(outputId = ns("MS1_path")), class = "text-wrap"),
          hr_head(),
          p('Only accept .mzXML file. \nMake sure your file structure is consistent with the following image',
            style = "color: #7a8788;font-size: 12px; font-style:Italic"),
          img(src = "https://shawnmagic-1257599720.cos.ap-chengdu.myqcloud.com/picgo/202304041816787.png",width = "100%"),
          shinyDirButton(id = ns("MS2"), label = "Select MS2 folder" ,
                         title = "The MS2 file folder:",
                         buttonType = "default", class = NULL,
                         icon = bs_icon("folder"), multiple = FALSE),
          tags$span(textOutput(outputId = ns("MS2_path")), class = "text-wrap"),
          p('Only accept .mgf file. \nMake sure your file structure is consistent with the following image',
            style = "color: #7a8788;font-size: 12px; font-style:Italic"),
          img(src = "https://shawnmagic-1257599720.cos.ap-chengdu.myqcloud.com/picgo/202304041816643.png",width = "100%")
        ),
        accordion_panel(
          title = "Peak picking parameters",
          icon = bsicons::bs_icon("gear"),
          h3("MS1 import Parameters",style = "color:#darkgreen"),
          hr_main(),
          textInput(
            inputId = ns('ppm'),label = "ppm",value = 15,
          ),
          hr_head(),
          textInput(
            inputId = ns('p_min'),label = 'peakwidth min',value = 5
          ),
          hr_head(),
          textInput(
            inputId = ns('p_max'),label = 'peakwidth max',value = 30
          ),
          hr_head(),
          textInput(
            inputId = ns('snthresh'),label = 'snthresh',value = 10
          ),
          hr_head(),
          textInput(
            inputId = ns('pre_left'),label = 'prefilter peaks',value = 3
          ),
          hr_head(),
          textInput(
            inputId = ns('pre_right'),label = 'prefilter intensity',value = 500
          ),
          hr_head(),
          selectInput(
            inputId = ns('fitgauss'),label = 'fitgauss',choices = c("TRUE","FALSE"),selected = "FALSE",multiple = F
          ),
          hr_head(),
          textInput(
            inputId = ns('integrate'),label = 'integrate',value = 2
          ),
          hr_head(),
          textInput(
            inputId = ns('noise'),label = 'noise',value = 500
          ),
          hr_head(),
          textInput(
            inputId = ns('mzdiff'),label = 'mzdiff',value = 0.01
          ),
          hr_head(),
          textInput(
            inputId = ns('threads'),label = 'threads',value = 6
          ),
          hr_head(),
          textInput(
            inputId = ns('binSize'),label = 'binSize',value = 0.025
          ),
          hr_head(),
          textInput(
            inputId = ns('bw'),label = 'bw',value = 5
          ),
          hr_head(),
          textInput(
            inputId = ns('min_fraction'),label = 'min_fraction',value = 0.5
          ),
          hr_head(),
          selectInput(
            inputId = ns('out_put_peak'),label = 'figure output',choices = c("TRUE","FALSE"),selected = "TRUE",multiple = F
          ),
          hr_head(),
          selectInput(
            inputId = ns('fill_peaks'),label = 'fill_peaks',choices = c("TRUE","FALSE"),selected = "FALSE",multiple = F
          ),
          h3("MS2 import Parameters",style = "color:#darkgreen"),
          hr_main(),
          textInput(
            inputId = ns('column'),label = 'column',value = 'rp'
          ),
          hr_head(),
          textInput(
            inputId = ns('ms1.ms2.match.rt.tol'),label = 'ms1.ms2.match.rt.tol',value = 15
          ),
          hr_head(),
          textInput(
            inputId = ns('ms1.ms2.match.mz.tol'),label = 'ms1.ms2.match.mz.tol',value = 30
          )
        )
      ),
      page_fluid(
        nav_panel(
          title = "File check",
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
            ),
            nav_panel(
              "MS2",
              card_title(".mgf file list (MS2)"),
              DT::dataTableOutput(ns("tbl_ms2"))
            )
          ),
          tags$h3("Optimize peak picking parameters",style = 'color: #008080'),
          navset_card_tab(
            height = 700,
            full_screen = TRUE,
            title = "optimize peak picking parameters (option)",
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
                  actionButton(ns('action3'),'Start',icon = icon("computer-mouse")),
                ),
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
                      actionButton(ns('action4'),'Start',icon = icon("computer-mouse")),
                    ),
                    DT::dataTableOutput(ns("parameters_opt")),
                    radioButtons(inputId = ns("para_choise"),label = "use optimized parameters",choices = c("yes","no"),selected = "yes")
                  )
            )
          ),
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
#' @noRd


data_import_raw_server <- function(id,volumes,prj_init,data_import_rv) {
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

    #> Import MS2 folder: from raw file
    observe({
      shinyDirChoose(input, "MS2", roots = volumes, session = session)
      if(!is.null(input$MS2)){
        # browser()
        ms2_folder_selected<-parseDirPath(roots = volumes,  input$MS2)
        output$MS2_path <- renderText(ms2_folder_selected)
      }})

    #> Import MS2 folder: from table
    observe({
      shinyDirChoose(input, "MS2_table", roots = volumes, session = session)
      if(!is.null(input$MS2_table)){
        # browser()
        ms2_folder_selected <- parseDirPath(roots = volumes,  input$MS2_table)
        output$MS2_path_table <- renderText(ms2_folder_selected)
      }})

    #> Import MS1 object pos: from object
    observe({
      shinyFileChoose(input, "Pos_obj_mass", roots = volumes, session = session)
      if(!is.null(input$Pos_obj_mass)){
        # browser()
        obj_pos_filepath <- parseFilePaths(roots = volumes,  input$Pos_obj_mass)
        output$obj_pos_filepath <- renderText(obj_pos_filepath$datapath)
      }})

    #> Import MS1 object neg: from object
    observe({
      shinyFileChoose(input, "Neg_obj_mass", roots = volumes, session = session)
      if(!is.null(input$Neg_obj_mass)){
        # browser()
        obj_neg_filepath <- parseFilePaths(roots = volumes,  input$Neg_obj_mass)
        output$obj_neg_filepath <- renderText(obj_neg_filepath$datapath)
      }})

    #> Import MS2 folder: from object
    observe({
      shinyDirChoose(input, "MS2_obj", roots = volumes, session = session)
      if(!is.null(input$MS2_obj)){
        # browser()
        ms2_folder_selected <- parseDirPath(roots = volumes,  input$MS2_obj)
        output$MS2_path_table_2 <- renderText(ms2_folder_selected)
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
        if(is.null(input$MS2)){return()}
        if(is.null(prj_init$sample_info)){return()}

        print('check point 01')


        #> MS1 data file
        para_data_check$ms1_folder_selected <- parseDirPath(volumes, input$MS1)
        para_data_check$MS1_path <- para_data_check$ms1_folder_selected |> as.character()
        para_data_check$QC_number.n <- list.files(paste0(para_data_check$MS1_path,"/NEG/QC"))
        para_data_check$QC_number.p <- list.files(paste0(para_data_check$MS1_path,"/POS/QC"))
        para_data_check$S_number.n <- list.files(paste0(para_data_check$MS1_path,"/NEG/Subject"))
        para_data_check$S_number.p <- list.files(paste0(para_data_check$MS1_path,"/POS/Subject"))




        #> MS2 data file
        para_data_check$ms2_folder_selected <- parseDirPath(volumes, input$MS2)
        para_data_check$MS2_path <- para_data_check$ms2_folder_selected |> as.character()
        para_data_check$QC_number.n2 <- list.files(paste0(para_data_check$MS2_path,"/NEG/QC"))
        para_data_check$QC_number.p2 <- list.files(paste0(para_data_check$MS2_path,"/POS/QC"))
        para_data_check$S_number.n2 <- list.files(paste0(para_data_check$MS2_path,"/NEG/Subject"))
        para_data_check$S_number.p2 <- list.files(paste0(para_data_check$MS2_path,"/POS/Subject"))
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

        print('check point 03')

        #> MS1 file tbl
        output$tbl_ms1 =
          renderDataTable_formated(actions = input$action1,
                                   condition1 = para_data_check$QC_number.n,
                                   condition2 = para_data_check$QC_number.p,
                                   condition3 = para_data_check$S_number.n,
                                   condition4 = para_data_check$S_number.p,
                                   tbl = temp_tbl_ms1,filename.a = "3.3.rawDataImport_summary_of_ms1_file")


        #> MS2 file tbl
        temp_tbl_ms2 = data.frame(
          FileName = c(para_data_check$QC_number.n2,
                       para_data_check$QC_number.p2,
                       para_data_check$S_number.n2,
                       para_data_check$S_number.p2),
          Type = rep(c('QC_neg','QC_pos','Subject_neg','Subject_pos'),
                     c(length(para_data_check$QC_number.n2),
                       length(para_data_check$QC_number.p2),
                       length(para_data_check$S_number.n2),
                       length(para_data_check$S_number.p2)))
        )


        output$tbl_ms2 =
          renderDataTable_formated(actions = input$action1,
                                   condition1 = para_data_check$QC_number.n,
                                   condition2 = para_data_check$QC_number.p,
                                   condition3 = para_data_check$S_number.n,
                                   condition4 = para_data_check$S_number.p,
                                   tbl = temp_tbl_ms2,filename.a = "3.3.rawDataImport_summary_of_ms2_file")


        output$file_check1 = renderUI({
          isolate(HTML(
            paste0(
              '<div class="info-block">',
              '  <div>',
              '    <span class="info-label">The number of QC files:</span>',
              '    <span class="info-value">',
              '      Positive model: <font color="red">(', para_data_check$QC_number.p |> length(), ' | ', para_data_check$QC_number.p2 |> length(), ')</font>',
              '      Negative model: <font color="red">(', para_data_check$QC_number.n |> length(), ' | ', para_data_check$QC_number.n2 |> length(), ')</font>',
              '    </span>',
              '  </div>',
              '  <div>',
              '    <span class="info-label">The number of Subject files:</span>',
              '    <span class="info-value">',
              '      Positive model: <font color="red">(', para_data_check$S_number.p |> length(), ' | ', para_data_check$S_number.p2 |> length(), ')</font>',
              '      Negative model: <font color="red">(', para_data_check$S_number.n |> length(), ' | ', para_data_check$S_number.n2 |> length(), ')</font>',
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
        if(file.exists(paste0(para_data_check$MS1_path,"/POS/QC/ppmCut.xlsx")) &
           file.exists(paste0(para_data_check$MS1_path,"/POS/QC/ppmCut.xlsx"))
           ){

          withProgress(message = 'Test ppm cutoff',value = 0,
                       expr = {
                         for (i in 1:3) {
                           incProgress(1/3,detail = pro_optimize_step1[i])
                           if(i == 1) {
                             data_para_opt$ppmCut.p = readxl::read_xlsx(paste0(para_data_check$MS1_path,"/POS/QC/ppmCut.xlsx")) %>% pull(ppmCut) %>% as.numeric()
                           } else if(i == 2) {
                             data_para_opt$ppmCut.n = readxl::read_xlsx(paste0(para_data_check$MS1_path,"/NEG/QC/ppmCut.xlsx")) %>% pull(ppmCut) %>% as.numeric()
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
                               directory = paste0(para_data_check$MS1_path,"/POS/QC/"),
                               massSDrange = massSDrange,
                               smooth = smooth,
                               cutoff = cutoff,
                               filenum = filenum,
                               thread = input$thread.1 %>% as.numeric()
                             )
                           } else if(i == 2) {
                             data_para_opt$step1.n = paramounter_part1(
                               directory = paste0(para_data_check$MS1_path,"/NEG/QC/"),
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
        if(file.exists(paste0(para_data_check$MS1_path,"/POS/QC/parameters.xlsx")) &
           file.exists(paste0(para_data_check$MS1_path,"/POS/QC/parameters.xlsx"))
        ) {
          withProgress(message = 'optimize parameter',value = 0,
                       expr = {
                         for (i in 1:3) {
                           incProgress(1/3,detail = pro_optimize_step2[i])
                           if(i == 1) {
                             data_para_opt$step2.p = readxl::read_xlsx(paste0(para_data_check$MS1_path,"/POS/QC/parameters.xlsx"))
                           } else if(i == 2) {
                             data_para_opt$step2.n = readxl::read_xlsx(paste0(para_data_check$MS1_path,"/NEG/QC/parameters.xlsx"))
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

        withProgress(message = 'optimize parameter',value = 0,
                     expr = {
                       for (i in 1:3) {
                         incProgress(1/3,detail = pro_optimize_step2[i])
                         if (i == 1) {
                           data_para_opt$step2.p = paramounter_part2(
                             directory = paste0(para_data_check$MS1_path,"/POS/QC/"),
                             massSDrange = massSDrange,
                             smooth = smooth,
                             cutoff = cutoff,
                             filenum = filenum,
                             thread = input$thread.2 %>% as.numeric(),
                             ppmCut = data_para_opt$ppmCut.p
                           ) %>% dplyr::rename("Positive" = "Value")

                         } else if(i == 2) {
                           data_para_opt$step2.n = paramounter_part2(
                             directory = paste0(para_data_check$MS1_path,"/NEG/QC/"),
                             massSDrange = massSDrange,
                             smooth = smooth,
                             cutoff = cutoff,
                             filenum = filenum,
                             ppmCut = data_para_opt$ppmCut.n,
                             thread = input$thread.2 %>% as.numeric()
                           ) %>% dplyr::rename("Negative" = "Value")
                         } else if (i == 3) {
                           writexl::write_xlsx(data_para_opt$step2.p,paste0(para_data_check$MS1_path,"/POS/QC/parameters.xlsx"))
                           writexl::write_xlsx(data_para_opt$step2.n,paste0(para_data_check$MS1_path,"/NEG/QC/parameters.xlsx"))
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

    #>peak picking
    #data_import_rv <- reactiveValues(data = NULL)
    observeEvent(
      input$action2,
      {
        if(is.null(input$MS1)){return()}
        if(is.null(para_data_check$MS1_path)){return()}
        para_choise = para_choise()

        pro_step = c('running positive model ...',
                     'running negative model ...',
                     'reading MS2 data',
                     'All finish!')

        data_import_rv$parameters =
          data.frame(
            para = c("ppm","threads","snthresh","noise","min_fraction","p_min","p_max","pre_left","pre_right","fill_peaks","fitgauss",
                     "integrate","mzdiff","binSize","bw","out_put_peak","column","ms1.ms2.match.rt.tol","ms1.ms2.match.mz.tol"),
            default = c(input$ppm,input$threads,input$snthresh,input$noise,
                        input$min_fraction,input$p_min,input$p_max,input$pre_left,
                        input$pre_right,input$fill_peaks,input$fitgauss,input$integrate,
                        input$mzdiff,input$binSize,input$bw,input$out_put_peak,input$column,
                        input$ms1.ms2.match.rt.tol,input$ms1.ms2.match.mz.tol)
          )


        if(para_choise == "yes") {
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
        print(data_import_rv$parameters)


        #> function
        process_data_fun = function(path,polarity,parameters){
          if(ncol(parameters) == 3) {
            n = 3
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
          data_import_rv$ms1.ms2.match.rt.tol = as.numeric(data_import_rv$parameters[18,n])
          data_import_rv$ms1.ms2.match.mz.tol = as.numeric(data_import_rv$parameters[19,n])
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
            group_for_figure = "QC"
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
                           rm(object)
                           load(paste0(para_data_check$MS1_path,"/NEG/Result/object"))
                           data_import_rv$object_neg <- object
                           rm(object)
                           if(length(data_import_rv$object_pos@ms2_data) == 0) {
                             data_import_rv$object_pos <-
                               mutate_ms2(
                                 object = data_import_rv$object_pos,
                                 polarity = 'positive',
                                 column = data_import_rv$column,
                                 ms1.ms2.match.rt.tol = data_import_rv$ms1.ms2.match.rt.tol,
                                 ms1.ms2.match.mz.tol = data_import_rv$ms1.ms2.match.mz.tol,
                                 path = paste0(para_data_check$MS2_path,"/POS/")
                               )
                             save_massobj(
                               polarity = 'positive',
                               file_path = paste0(prj_init$wd,"/Result/POS/Objects/"),
                               stage = 'step1',obj = data_import_rv$object_pos)
                           }
                           if(length(data_import_rv$object_neg@ms2_data) == 0) {
                             data_import_rv$object_neg <-
                               mutate_ms2(
                                 object = data_import_rv$object_neg,
                                 polarity = 'negative',
                                 column = data_import_rv$column,
                                 ms1.ms2.match.rt.tol = data_import_rv$ms1.ms2.match.rt.tol,
                                 ms1.ms2.match.mz.tol = data_import_rv$ms1.ms2.match.mz.tol,
                                 path = paste0(para_data_check$MS2_path,"/NEG/")
                               )
                             save_massobj(
                               polarity = 'negative',
                               file_path = paste0(prj_init$wd,"/Result/NEG/Objects/"),
                               stage = 'step1',
                               obj = data_import_rv$object_neg)
                           }
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

        #> add ms2 data
        output$para_clean_tbl = renderDataTable_formated(
          actions = input$action2,
          condition1 = input$MS1,
          condition2 = para_data_check$MS1_path,
          tbl = data_import_rv$parameters,filename.a = "3.3.rawDataImport_summary_of_parameters"
        )

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

