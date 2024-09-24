#' project init UI
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyFiles shinyDirButton shinyFilesButton
#' @importFrom DT dataTableOutput
#' @noRd

project_init_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Initialize project',
    icon = bsicons::bs_icon("play-circle"),
    navset_card_tab(
      title = "Start your project.",
      header = "Step 1",
      sidebar = accordion(
        accordion_panel(
          title = "Working directory",
          icon = bsicons::bs_icon("menu-app"),
          shinyDirButton(id = ns("prj_wd"),
                         label = "Set working directory" ,
                         title = "Set working directory:",
                         buttonType = "default", class = NULL,
                         icon = bsicons::bs_icon("folder"), multiple = FALSE),
          tags$span(textOutput(outputId = ns("raw_wd_path")), class = "text-wrap"),

          fileInput(
            inputId = ns('SampleInfo'),
            label = 'Sample Information',
            multiple = FALSE,
            accept = '.csv'
          ),

          p('Make sure your file structure is consistent with the following image',style = "color: #7a8788;font-size: 12px; font-style:Italic"),
          img(src = "https://shawnmagic-1257599720.cos.ap-chengdu.myqcloud.com/picgo20240319180805.png",width = "100%"),

          selectInput(
            inputId = ns("sample_id_raw"),label = "sample id",choices = c("sample_id",'injection.order',"class","group"),
            selected = "sample_id",multiple = FALSE
          ),
          selectInput(
            inputId = ns("injection.order_raw"),label = "injection order",choices = c("sample_id",'injection.order',"class","group"),
            selected = "injection.order",multiple = FALSE
          ),
          selectInput(
            inputId = ns("class_raw"),label = "class",choices = c("sample_id",'injection.order',"class","group","batch"),
            selected = "class",multiple = FALSE
          ),
          selectInput(
            inputId = ns("group_raw"),label = "group",choices = c("sample_id",'injection.order',"class","group","batch"),
            selected = "group",multiple = FALSE
          ),
          selectInput(
            inputId = ns("batch_raw"),label = "batch",choices = c("sample_id",'injection.order',"class","group","batch"),
            selected = "batch",multiple = FALSE
          ),
          radioButtons(
            inputId = ns("init_ion"),label = "Ion model",choices = c("Positive","Negative","Both"),
            selected = "Both"
          )
        ),
        accordion_panel(
          title = "Resuming task",
          icon = bsicons::bs_icon("repeat"),
          selectInput_div(
            inputId = ns("init_steps"),label = "Choose steps",
            choices = c("Remove noisey feature","Remove outlier","impute missing value","Normalization","Annotation","Annotation filtering","Data integrate","DAM and rest"),
            selected = NULL,multiple = F,
            title = "Choose steps"
          ),
          shinyFilesButton(
            id = ns('saved_obj_pos'),buttonType = "default",title = "load mass dataset object (positive) ",
            label = 'Positive object',
            class = NULL,
            icon = bsicons::bs_icon("database"), multiple = FALSE
          ),
          tags$span(textOutput(outputId = ns("saved_obj_pos")), class = "text-wrap"),
          p('Make sure your input .rda file is AUTO saved file by THIS SHINY APP.',style = "color: #7a8788;font-size: 12px; font-style:Italic") ,
          shinyFilesButton(
            id = ns('saved_obj_neg'),buttonType = "default",title = "load mass dataset object (positive) ",
            label = 'Negative object',
            class = NULL,
            icon = bsicons::bs_icon("database"), multiple = FALSE
          ),
          tags$span(textOutput(outputId = ns("saved_obj_neg")),  class = "text-wrap"),
          p('Make sure your input .rda file is AUTO saved file by THIS SHINY APP.',style = "color: #7a8788;font-size: 12px; font-style:Italic"),
          shinyFilesButton(
            id = ns('init_dblist'),buttonType = "default",title = "load saved database list file (option) ",
            label = 'load database file ',
            class = NULL,
            icon = bsicons::bs_icon("folder"), multiple = FALSE
          ),
          tags$span(textOutput(outputId = ns("init_dblist")),  class = "text-wrap"),
          p('Make sure your input .rda file is AUTO saved file by Annotation step, only needed in re-analysis annotation filtering step.',style = "color: #7a8788;font-size: 12px; font-style:Italic"),
        )
      ),
      nav_panel(
        title = "Setting working directory",
        icon = bsicons::bs_icon("power"),
        actionButton(inputId = ns('action_init'),'Initialize project',icon = icon("play"), style = "width: 200px;"),
        tags$h3("Summary of input file",style = 'color: #008080'),
        hr_head(),
        htmlOutput(outputId = ns("file_check_init")),
        card(
          full_screen = T,
          height = 350,
          card_header(
            "Sample information"
          ),
          DT::dataTableOutput(ns("tbl_sample_info"))
        )
      ),
      nav_panel(
        title = "Resuming analysis from the unfinished steps",
        icon = bsicons::bs_icon("repeat"),
        layout_column_wrap(
          width = 1/2,
          height = 300,
          card(
            full_screen = T,
            height = 350,
            card_header(
              "Positive model"
            ),
            verbatimTextOutput(ns("res_pos_mod"))
          ),
          card(
            full_screen = T,
            height = 350,
            card_header(
              "Negative model"
            ),
            verbatimTextOutput(ns("res_neg_mod"))
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
#' @importFrom shinyFiles shinyDirChoose parseDirPath parseFilePaths getVolumes shinyFileChoose
#' @importFrom massprocesser process_data
#' @importFrom massdataset mutate_ms2
#' @param id module of server
#' @param volumes shinyFile volumes
#' @param prj_init reactivevalues of project init.
#' @noRd


project_init_server <- function(id,volumes,prj_init) {
  moduleServer(id, function(input, output, session) {
    #> set working directory and import sample information

    observe({
      shinyDirChoose(input = input,id = "prj_wd", roots =  volumes, session = session)
      if(!is.null(input$prj_wd)){
        # browser()
        ms1_folder_selected<- parseDirPath(roots = volumes, input$prj_wd)
        output$raw_wd_path <- renderText(ms1_folder_selected)

      }})

    #> Import sample info from raw file
    sample_info_raw <- reactive({
      file1 <- input$SampleInfo
      if(is.null(file1)){return()}
      read.csv(file = file1$datapath,
               sep=",",header = T,stringsAsFactors = F)
    })
    #> update selectInput contents by read sample infomation table.

    sample_info_col = reactive({
      colnames(sample_info_raw() |> as.data.frame())
    })

    observe({
      updateSelectInput(session, "sample_id_raw",choices = sample_info_col(),selected = sample_info_col()[1])
      updateSelectInput(session, "injection.order_raw",choices = sample_info_col(),selected = sample_info_col()[2])
      updateSelectInput(session, "class_raw",choices = sample_info_col(),selected = sample_info_col()[3])
      updateSelectInput(session, "group_raw",choices = sample_info_col(),selected = sample_info_col()[4])
      updateSelectInput(session, "batch_raw",choices = sample_info_col(),selected = sample_info_col()[5])
    })

    #> load positive object
    observe({
      shinyFileChoose(input, "saved_obj_pos", roots = volumes, session = session)
      if(!is.null(input$saved_obj_pos)){
        # browser()
        temp_pos_file <-parseFilePaths(roots = volumes,  input$saved_obj_pos)
        output$saved_obj_pos <- renderText(temp_pos_file$datapath)
      }})
    #> load negative object
    observe({
      shinyFileChoose(input, "saved_obj_neg", roots = volumes, session = session)
      if(!is.null(input$saved_obj_neg)){
        # browser()
        temp_neg_file <-parseFilePaths(roots = volumes,  input$saved_obj_neg)
        output$saved_obj_neg<- renderText(temp_neg_file$datapath)
      }})

    #> load saved database
    observe({
      shinyFileChoose(input, "init_dblist", roots = volumes, session = session)
      if(!is.null(input$init_dblist)){
        # browser()
        dblist_path <-parseFilePaths(roots = volumes,  input$init_dblist)
        output$init_dblist<- renderText(dblist_path$datapath)
      }})
    #> start
    #    prj_init <- reactiveValues(data = NULL)

    observeEvent(
      input$action_init,
      {
        if(is.null(input$prj_wd)){return()}
        if(is.null(input$SampleInfo)){return()}
        prj_init$ion_model <- as.character(input$init_ion)
        #> working dir path
        prj_init$wd_path <- parseDirPath(volumes, input$prj_wd)
        prj_init$wd <- prj_init$wd_path |> as.character()
        #> sample_info file
        prj_init$sample_id_n = as.character(input$sample_id_raw)
        prj_init$injection.order_n = as.character(input$injection.order_raw)
        prj_init$class_n = as.character(input$class_raw)
        prj_init$group_n = as.character(input$group_raw)
        prj_init$batch_n = as.character(input$batch_raw)
        prj_init$sample_info = as.data.frame(sample_info_raw())

        prj_init$sample_info =
          prj_init$sample_info |>
          dplyr::rename(
            "sample_id" = prj_init$sample_id_n,
            "injection.order" = prj_init$injection.order_n,
            "class" = prj_init$class_n,
            "group" = prj_init$group_n,
            "batch" = prj_init$batch_n
          ) |> mutate(batch = as.character(batch))

        prj_init$steps = input$init_steps |> as.character()

        #> Sample info

        output$tbl_sample_info =
          renderDataTable_formated(actions = input$action_init,
                                   condition1 = input$SampleInfo,
                                   condition2 = prj_init$sample_info,
                                   tbl = prj_init$sample_info,filename.a = "3.2.Prj_init_sample_info_check")
        #> load positive object
        temp_pos_file = parseFilePaths(roots = volumes, input$saved_obj_pos)
        temp_pos_path = as.character(temp_pos_file$datapath)
        temp_neg_file = parseFilePaths(roots = volumes, input$saved_obj_neg)
        temp_neg_path = as.character(temp_neg_file$datapath)
        temp_dblist_file = parseFilePaths(roots = volumes, input$init_dblist)
        temp_dblist_path = as.character(temp_dblist_file$datapath)
        print(temp_dblist_path)
        if(length(temp_pos_path) == 0 | length(temp_neg_path) == 0)
        {
          init_status1 = "no old .rda file uploaded, skip this step";
          init_status2 = "no old .rda file uploaded, skip this step";
          init_status = paste0('<font color = blue> <b>The mass_dataset object: </b> </font><br/>Positive file path: <font color=red> ',init_status1,' </font><br/>Nagetive file path: <font color=red> ',init_status2,'</font><br/>')
          init_re_start_step = '<font color = blue> <b>Restart your jobs from: </b> </font> <font color=orange> Start from raw data. </font><br/>'
        } else if(str_detect(temp_pos_path,"\\.rda",negate = T) | str_detect(temp_neg_path,"\\.rda",negate = T))
        {
          init_status1 = "wrong old .rda file uploaded, please check your input";
          init_status2 = "wrong old .rda file uploaded, please check your input";
          init_status = paste0('<font color = blue> <b>The mass_dataset object: </b> </font><br/>Positive file path: <font color=red> ',init_status1,' </font><br/>Nagetive file path: <font color=red> ',init_status2,'</font><br/>')
          init_re_start_step = '<font color = blue> <b>Restart your jobs from: </b> </font> <font color=red> ERROR: wrong format. </font><br/>'
        } else if(str_detect(temp_pos_path,"\\.rda",negate = F) & str_detect(temp_neg_path,"\\.rda",negate = F))
        {
          temp_init_pos <- load(file = temp_pos_path)
          temp_init_neg <- load(file = temp_neg_path)
          prj_init$object_negative.init = get(temp_init_neg)
          prj_init$object_positive.init = get(temp_init_pos)
          if(class(prj_init$object_negative.init) == "mass_dataset" & class(prj_init$object_positive.init) == "mass_dataset"){
            init_status1 = temp_pos_path
            init_status2 = temp_neg_path
            init_status = paste0('<font color = blue> <b>The mass_dataset object: </b> </font><br/>Positive file path: <font color=orange> ',init_status1,' </font><br/>Nagetive file path: <font color=orange> ',init_status2,'</font><br/>')
            init_re_start_step = paste0('<font color = blue> <b>Restart your jobs from: </b> </font> <font color=orange>' ,prj_init$steps, '</font><br/>')

          } else {
            init_status1 = "wrong file"
            init_status2 = "wrong file"
            init_status = paste0('<font color = blue> <b>The mass_dataset object: </b> </font><br/>Positive file path: <font color=red> ',init_status1,' </font><br/>Nagetive file path: <font color=red> ',init_status2,'</font><br/>')
            init_re_start_step = '<font color = blue> <b>Restart your jobs from: </b> </font> <font color=red> ERROR: Wrong format of your input .rda file, please make sure your input is generated by THIS shiny app or tidyMass work flow! </font><br/>'
            prj_init$object_negative.init <- NULL
            prj_init$object_positive.init <- NULL
          }

        }

        if(length(temp_dblist_path) == 0 ) {
          if(prj_init$steps == "Annotation filtering") {
            init_db_file_check = '<font color = blue> <b>Annotation database: </b> </font> <font color=red> ERROR: Please upload auto-saved annotation database file (.dblist) in Annotation step. </font><br/>'
          } else {
            init_db_file_check = '<font color = blue> <b>Annotation database: </b> </font> <font color=red> No need to upload this file, pass. </font><br/>'
          }
        } else if(str_detect(string = temp_dblist_path,pattern = "dblist",negate = T)) {
          if(prj_init$steps == "Annotation filtering") {
            init_db_file_check = '<font color = blue> <b>Annotation database: </b> </font> <font color=red> ERROR: Please upload auto-saved annotation database file (.dblist) in Annotation step. </font><br/>'
          } else {
            init_db_file_check = '<font color = blue> <b>Annotation database: </b> </font> <font color=red> No need to upload this file, pass. </font><br/>'
          }
        } else if(str_detect(temp_dblist_path,"auto_saved.dblist")){
          temp_dblist = load(file = temp_dblist_path)
          prj_init$dblist = get(temp_dblist)
          init_db_file_check = paste0('<font color = blue> <b>Annotation database: </b> </font> <font color=orange>' ,paste0(names(prj_init$dblist),collapse = "|"), '</font><br/>')

        }

        output$file_check_init = renderUI({
          isolate(HTML(
            paste0(
              '<link rel="stylesheet" type="text/css" href="style.css" />',

              '<div class="info-block">',
              '  <div>',
              '    <span class="info-label">The working directory:</span>',
              '    <span class="info-value">', prj_init$wd, '</span>',
              '  </div>',
              '  <div>',
              '    <span class="info-label">Selected Ion model:</span>',
              '    <span class="info-value">', prj_init$ion_model, '</span>',
              '  </div>',
              '  <div>',
              '    <span class="info-label">The sample information:</span>',
              '    <span class="info-value">', paste(sample_info_col(), collapse = ", "), '</span>',
              '  </div>',
              '  <div>', init_re_start_step, '</div>',
              '  <div>', init_status, '</div>',
              '  <div>', init_db_file_check, '</div>',
              '</div>'
            )
          ))
        })
        #> status
        #> information of mass datasets
        output$res_pos_mod = renderPrint({
          if(is.null(prj_init$object_positive.init)){return()}
          print(prj_init$object_positive.init)
        })
        output$res_neg_mod = renderPrint({
          if(is.null(prj_init$object_negative.init)){return()}
          print(prj_init$object_negative.init)
        })

      }
    )
  })
}






