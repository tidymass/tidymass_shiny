#' import from tbl data of UI
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyFiles shinyDirButton
#' @importFrom DT dataTableOutput
#' @noRd


data_import_massdataset_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Start with mass_dataset',
    icon = bs_icon("upload"),
    layout_sidebar(
      sidebar = sidebar(
        shinyFilesButton(
          id = ns('Pos_obj_mass'),buttonType = "default",title = "load mass dataset object (positive) ",
          label = 'positive ',
          class = NULL,
          icon = bs_icon("folder"), multiple = FALSE
        ),
        tags$span(textOutput(outputId = ns("obj_pos_filepath")), class = "text-wrap"),
        shinyFilesButton(
          id = ns('Neg_obj_mass'),buttonType = "default",title = "load mass dataset object (negative) ",
          label = 'negative',
          class = NULL,
          icon = bs_icon("folder"), multiple = FALSE
        ),
        tags$span(textOutput(outputId = ns("obj_neg_filepath")), class = "text-wrap"),

        actionButton(ns('action1.3'),'Check input',icon = icon("play")),
      ),
    page_fluid(
      nav_panel(
        title = "File check",
        icon = bsicons::bs_icon("inbox"),
        tags$h3("Summary of input file",style = 'color: #008080'),
        hr_head(),
        htmlOutput(ns("obj_mass_res_path2")),
        hr_head(),
        navset_card_tab(
          height = 350,
          full_screen = T,
          title = "data preview",
          nav_panel(
            title =  "variable information",
            card(
              full_screen = T,
              height = 350,
              DT::dataTableOutput(ns("obj_variable_info")),
            )
          ),
          nav_panel(
            title =  "expression data",
            card(
              full_screen = T,
              height = 350,
              DT::dataTableOutput(ns("obj_expmat"))
            )
          )
        ),
        navset_card_tab(
          height = 350,
          full_screen = T,
          title = "Progress record of mass_dataset",
          nav_panel(
            title = "Positive",
            card(
              full_screen = T,
              height = 350,
              verbatimTextOutput(ns("obj_porgress.pos_info"))
            )
          ),
          nav_panel(
            title = "Negative",
            card(
              full_screen = T,
              height = 350,
              verbatimTextOutput(ns("obj_porgress.neg_info"))
            )
          )
        )
      ),
      nav_panel(
        title = "Import from mass_dataset",
        icon = bsicons::bs_icon("table"),
        layout_column_wrap(
          width = 1/2,
          height = 350,
          card(
            full_screen = T,
            height = 350,
            card_header(
              "Positive"
            ),
            verbatimTextOutput(ns("obj_mass_check.pos_tbl2"))
          ),
          card(
            full_screen = T,
            height = 350,
            card_header(
              "negative"
            ),
            verbatimTextOutput(ns("obj_mass_check.neg_tbl2"))
          )
        )
      )
    )
    ),
  )

}


#' import from tbl data of server
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs toggle runjs useShinyjs
#' @importFrom shinyFiles shinyDirChoose parseDirPath parseFilePaths
#' @importFrom dplyr select mutate case_when pull mutate_if filter inner_join
#' @importFrom stringr str_detect
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom massdataset mutate_ms2 activate_mass_dataset extract_expression_data extract_variable_info
#' @importFrom magrittr %>%
#' @param id module of server
#' @param volumes shinyFiles volumes
#' @param prj_init use project init variables.
#' @param data_import_rv reactivevalues mass_dataset export
#' @param data_export_rv reactivevalues mass_dataset export
#' @noRd


data_import_massdataset_server <- function(id,volumes,prj_init,data_import_rv,data_export_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #> sidebar2 - from massdataset
    observeEvent(input$toggleSidebar, {
      shinyjs::toggle(id = "Sidebar")
    })

    #> Import MS1 object pos: from object
    observe({
      shinyFileChoose(input, "Pos_obj_mass", roots = volumes, session = session)
      if(!is.null(input$Pos_obj_mass)){
        # browser()
        obj_pos_filepath<-parseFilePaths(roots = volumes,  input$Pos_obj_mass)
        output$obj_pos_filepath <- renderText(obj_pos_filepath$datapath)
      }})

    #> Import MS1 object neg: from object
    observe({
      shinyFileChoose(input, "Neg_obj_mass", roots = volumes, session = session)
      if(!is.null(input$Neg_obj_mass)){
        # browser()
        obj_neg_filepath<-parseFilePaths(roots = volumes,  input$Neg_obj_mass)
        output$obj_neg_filepath <- renderText(obj_neg_filepath$datapath)
      }})
    ## 3.5 import from mass_dataset class -------------------------------------------

    #> File check
    para_obj_check <- reactiveValues(data = NULL)

    #> File check event
    observeEvent(
      input$action1.3,
      {

        if(is.null(input$Pos_obj_mass)){return()}
        if(is.null(input$Neg_obj_mass)){return()}
        temp_pos_file = parseFilePaths(volumes, input$Pos_obj_mass)
        temp_neg_file = parseFilePaths(volumes, input$Neg_obj_mass)

        tmp_pos = load(file = as.character(temp_pos_file$datapath))
        tmp_neg = load(file = as.character(temp_neg_file$datapath))

        para_obj_check$object.pos = get(tmp_pos)
        para_obj_check$object.neg = get(tmp_neg)

        #> variable information
        para_obj_check$vari_pos = para_obj_check$object.pos %>% extract_variable_info()
        para_obj_check$vari_neg = para_obj_check$object.neg %>% extract_variable_info()

        para_obj_check$vari_tbl = rbind(para_obj_check$vari_pos,para_obj_check$vari_neg)
        output$obj_variable_info = renderDataTable_formated(
          actions = input$action1.3,
          condition2 = para_obj_check$vari_tbl,
          condition3 = input$Pos_obj_mass,
          condition4 = input$Neg_obj_mass,filename.a = "3.5.mass_datasetimport_vari_info_check",
          tbl = para_obj_check$vari_tbl
        )


        #> expression profile
        para_obj_check$exp_pos = para_obj_check$object.pos %>% extract_expression_data() %>% rownames_to_column("variable_id")
        para_obj_check$exp_neg = para_obj_check$object.neg %>% extract_expression_data() %>% rownames_to_column("variable_id")

        para_obj_check$exp_tbl = rbind(para_obj_check$exp_pos,para_obj_check$exp_neg)

        output$obj_expmat = renderDataTable_formated(
          actions = input$action1.3,
          condition1 = para_obj_check$vari_tbl,
          condition2 = input$Pos_obj_mass,
          condition3 = input$Neg_obj_mass,
          filename.a = "3.5.mass_datasetimport_vari_info_check",
          tbl = para_obj_check$exp_tbl
        )

        #> progress
        #> information of mass datasets

        output$obj_porgress.pos_info = renderPrint({
          print(data_import_rv$object_pos %>%  massdataset::extract_process_info())
        })

        output$obj_porgress.neg_info = renderPrint({
          print(data_import_rv$object_neg %>%  massdataset::extract_process_info())
        })
        if(is.null(input$Pos_obj_mass)){return()}
        if(is.null(input$Neg_obj_mass)){return()}
        ## save object
        data_import_rv$object_pos =
          para_obj_check$object.pos
        save_massobj(
          polarity = 'positive',
          file_path = paste0(prj_init$wd,"/Result/POS/Objects/"),
          stage = 'step1',
          obj = data_import_rv$object_pos)
        data_import_rv$object_neg =
          para_obj_check$object.neg
        save_massobj(
          polarity = 'negative',
          file_path = paste0(prj_init$wd,"/Result/NEG/Objects/"),
          stage = 'step1',
          obj = data_import_rv$object_neg)

        #> file path of original massdata set object
        output$obj_mass_res_path2 = renderUI({
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

        #> information of mass datasets

        output$obj_mass_check.pos_tbl2 = renderPrint({
          print(data_import_rv$object_pos)
        })

        output$obj_mass_check.neg_tbl2 = renderPrint({
          print(data_import_rv$object_neg)
        })
      }
    )

  })
}

