#' Missing value imputation
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


mv_impute_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Missing value imputation',
    icon = bs_icon("grid-fill"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Parameters",
          icon = bsicons::bs_icon("gear"),
          selectInput(
            inputId = ns('impute_mv_method'),
            label = "method ",multiple = F,
            choices = c("knn", "rf", "mean", "median", "zero", "minimum", "bpca", "svdImpute",
                        "ppca"),
            selected = 'knn'
          ),
          tags$h4("for knn",style = 'color: #008080'),
          hr_bar(),
          textInput(
            inputId = ns('impute_mv_k'),
            label = "k",
            value = 10
          ),
          sliderInput(
            inputId = ns('impute_mv_rowmax'),
            label = "rowmax",
            min = 0,
            max = 1,
            value = 0.5,step = 0.05
          ),
          sliderInput(
            inputId = ns('impute_mv_colmax'),
            label = "colmax",
            min = 0,
            max = 1,
            value = 0.8,step = 0.05
          ),
          textInput(
            inputId = ns('impute_mv_maxp'),
            label = "maxp",
            value = 1500
          ),
          textInput(
            inputId = ns('impute_mv_rng.seed'),
            label = "rng.seed",
            value = 362436069
          ),
          tags$h4("for missForest (rf)",style = 'color: #008080'),
          hr_bar(),
          textInput(
            inputId = ns('impute_mv_maxiter'),
            label = "maxiter",
            value = 10
          ),
          textInput(
            inputId = ns('impute_mv_ntree'),
            label = "ntree",
            value = 100
          ),
          radioButtons(
            inputId = ns('impute_mv_decreasing'),
            label = "decreasing",choices = c("TRUE","FALSE"),
            selected = "FALSE"
          ),
          tags$h4("for ppca",style = 'color: #008080'),
          hr_bar(),
          textInput(
            inputId = ns('impute_mv_npcs'),
            label = "nPcs",
            value = 2
          ),
          textInput(
            inputId = ns('impute_mv_maxsteps'),
            label = "maxSteps",
            value = 100
          ),
          textInput(
            inputId = ns('impute_mv_threshold'),
            label = "threshold",
            value = 0.0001
          ),
          actionButton(
            inputId = ns("impute_start"),
            label = "Start",icon = icon("play")
          )
        )),
      page_fluid(
        nav_panel(title = "Missing value imputation",
                  navset_card_tab(
                    title = "Expression data preview",
                    height = 400,
                    full_screen = TRUE,
                    nav_panel(
                      "Positive",
                      dataTableOutput(ns("impute_tbl_pos"))
                    ),
                    nav_panel(
                      "Negative",
                      dataTableOutput(ns("impute_tbl_neg"))
                    )
                  ),
                  navset_card_tab(
                    title = "Status",
                    height = 400,
                    full_screen = TRUE,
                    nav_panel(
                      "Positive",
                      verbatimTextOutput(ns("obj_impute.pos"))
                    ),
                    nav_panel(
                      "Negative",
                      verbatimTextOutput(ns("obj_impute.neg"))
                    )
                  )
        )
      )
    )
  )
}


#' Missing value imputation
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
#' @noRd


mv_impute_server <- function(id,volumes,prj_init,data_import_rv,data_clean_rv,data_export_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    p2_impute_mv <- reactiveValues(data = NULL)

    analy_para = reactive({
      list(
        method = input$impute_mv_method %>% as.character(),
        k = input$impute_mv_k %>% as.numeric(),
        rowmax = input$impute_mv_rowmax %>% as.numeric(),
        colmax = input$impute_mv_colmax %>% as.numeric(),
        maxp = input$impute_mv_maxp %>% as.numeric(),
        rng.seed = input$impute_mv_rng.seed %>% as.numeric(),
        maxiter = input$impute_mv_maxiter %>% as.numeric(),
        ntree = input$impute_mv_ntree %>% as.numeric(),
        decreasing = input$impute_mv_decreasing %>% as.character(),
        npcs = input$impute_mv_npcs %>% as.numeric(),
        maxsteps = input$impute_mv_maxsteps %>% as.numeric(),
        threshold = input$impute_mv_threshold %>% as.numeric()
      )
    })
    observeEvent(
      input$impute_start,
      {
        if(is.null(prj_init$sample_info)) {return()}
        if(!is.null(prj_init$object_negative.init) & !is.null(prj_init$object_positive.init) & prj_init$steps == "impute missing value"){
          p2_impute_mv$object_neg.outlier = prj_init$object_negative.init
          p2_impute_mv$object_pos.outlier = prj_init$object_positive.init
        } else {
          if(is.null(data_clean_rv$object_pos.outlier)) {return()}
          if(is.null(data_clean_rv$object_neg.outlier)) {return()}
          p2_impute_mv$object_neg.outlier = data_clean_rv$object_neg.outlier
          p2_impute_mv$object_pos.outlier = data_clean_rv$object_pos.outlier
        }

        para = analy_para()

        if(para$decreasing == "TRUE") {
          p2_impute_mv$object_pos.impute <-
            p2_impute_mv$object_pos.outlier %>%
            impute_mv(method = para$method,
                      rowmax = para$rowmax,k = para$k,colmax = para$colmax,maxp = para$maxp,rng.seed = para$rng.seed,
                      maxiter = para$maxiter,ntree = para$ntree,decreasing = TRUE,
                      nPcs = para$npcs,maxSteps = para$maxsteps,threshold = para$threshold)
          p2_impute_mv$object_neg.impute <-
            p2_impute_mv$object_neg.outlier %>%
            impute_mv(method = para$method,
                      rowmax = para$rowmax,k = para$k,colmax = para$colmax,maxp = para$maxp,rng.seed = para$rng.seed,
                      maxiter = para$maxiter,ntree = para$ntree,decreasing = TRUE,
                      nPcs = para$npcs,maxSteps = para$maxsteps,threshold = para$threshold)
        } else {
          p2_impute_mv$object_pos.impute <-
            p2_impute_mv$object_pos.outlier %>%
            impute_mv(method = para$method,
                      rowmax = para$rowmax,k = para$k,colmax = para$colmax,maxp = para$maxp,rng.seed = para$rng.seed,
                      maxiter = para$maxiter,ntree = para$ntree,decreasing = FALSE,
                      nPcs = para$npcs,maxSteps = para$maxsteps,threshold = para$threshold)
          p2_impute_mv$object_neg.impute <-
            p2_impute_mv$object_neg.outlier %>%
            impute_mv(method = para$method,
                      rowmax = para$rowmax,k = para$k,colmax = para$colmax,maxp = para$maxp,rng.seed = para$rng.seed,
                      maxiter = para$maxiter,ntree = para$ntree,decreasing = FALSE,
                      nPcs = para$npcs,maxSteps = para$maxsteps,threshold = para$threshold)
        }
        #> acc tbl
        temp_acc_mat.pos <- p2_impute_mv$object_pos.impute %>%
          extract_expression_data() %>%
          rownames_to_column("variable_id")
        output$impute_tbl_pos = renderDataTable_formated(
          actions = input$impute_start,
          condition1 = p2_impute_mv$object_pos.impute,
          filename.a = "3.6.4.ImputAccumulationMatrix.pos",
          tbl = temp_acc_mat.pos
        )


        temp_acc_mat.neg <- p2_impute_mv$object_neg.impute %>%
          extract_expression_data() %>%
          rownames_to_column("variable_id")
        output$impute_tbl_neg = renderDataTable_formated(
          actions = input$impute_start,
          condition1 = p2_impute_mv$object_neg.impute,
          filename.a = "3.6.4.ImputAccumulationMatrix.neg",
          tbl = temp_acc_mat.neg
        )

        data_clean_rv$object_pos.impute = p2_impute_mv$object_pos.impute
        data_clean_rv$object_neg.impute = p2_impute_mv$object_neg.impute

        save_massobj(
          polarity = 'positive',
          file_path = paste0(prj_init$wd,"/Result/POS/Objects/"),
          stage = 'impute',
          obj = p2_impute_mv$object_pos.impute)

        save_massobj(
          polarity = 'negative',
          file_path = paste0(prj_init$wd,"/Result/NEG/Objects/"),
          stage = 'impute',
          obj = p2_impute_mv$object_neg.impute)

        #> information of mass datasets
        output$obj_impute.pos = renderPrint({
          print(p2_impute_mv$object_pos.impute)
        })
        output$obj_impute.neg = renderPrint({
          print(p2_impute_mv$object_neg.impute)
        })
      }
    )


  })
}

