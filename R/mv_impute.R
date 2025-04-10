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
          title = "Methods",
          icon = bsicons::bs_icon("gear"),
          selectInput(
            inputId = ns('impute_mv_method'),
            label = "method ",multiple = F,
            choices = c("knn", "rf", "mean", "median", "zero", "minimum", "bpca", "svdImpute",
                        "ppca"),
            selected = 'knn'
          )),
        accordion_panel(
          title = "For KNN",
          icon = bsicons::bs_icon("gear"),
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
          )
          ),
        accordion_panel(
          title = "For missForest (rf)",
          icon = bsicons::bs_icon("gear"),
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
          )
          ),
        accordion_panel(
          title = "For bpca",
          icon = bsicons::bs_icon("gear"),
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
          )
          ),
          actionButton(
            inputId = ns("impute_start"),
            label = "Start",icon = icon("play"),width = "100%"
          )
        ),
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
#' @param data_clean_rv reactivevalues p2 dataclean
#' @param data_export_rv reactivevalues mass_dataset export
#' @noRd
mv_impute_server <- function(id, volumes, prj_init, data_clean_rv, data_export_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    p2_impute_mv <- reactiveValues(data = NULL)

    # Ion mode check function ----
    check_ion_modes <- function(data_rv, prj) {
      list(
        has_pos = !is.null(data_rv$object_pos_outlier) || !is.null(prj$object_positive.init),
        has_neg = !is.null(data_rv$object_neg_outlier) || !is.null(prj$object_negative.init)
      )
    }
    # parameters ----
    analy_para <- reactive({
      list(
        method = input$impute_mv_method,
        k = as.numeric(input$impute_mv_k),
        rowmax = as.numeric(input$impute_mv_rowmax),
        colmax = as.numeric(input$impute_mv_colmax),
        maxp = as.numeric(input$impute_mv_maxp),
        rng.seed = as.numeric(input$impute_mv_rng.seed),
        maxiter = as.numeric(input$impute_mv_maxiter),
        ntree = as.numeric(input$impute_mv_ntree),
        decreasing = as.logical(input$impute_mv_decreasing),
        npcs = as.numeric(input$impute_mv_npcs),
        maxsteps = as.numeric(input$impute_mv_maxsteps),
        threshold = as.numeric(input$impute_mv_threshold)
      )
    })



    # imputation functions ----
    perform_imputation <- function(object, para) {
      tryCatch({
        impute_mv(
          object,
          method = para$method,
          rowmax = para$rowmax,
          k = para$k,
          colmax = para$colmax,
          maxp = para$maxp,
          rng.seed = para$rng.seed,
          maxiter = para$maxiter,
          ntree = para$ntree,
          decreasing = para$decreasing,
          nPcs = para$npcs,
          maxSteps = para$maxsteps,
          threshold = para$threshold
        )
      }, error = function(e) {
        shinyalert("Imputation Error",
                   paste("Failed to perform imputation:", e$message),
                   type = "error")
        NULL
      })
    }



    # core processing ----
    observeEvent(input$impute_start, {

      # check data
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
      # Check if data initialization exists
      if(is.null(data_clean_rv$object_pos_outlier) && is.null(data_clean_rv$object_neg_outlier)){
        if (!is.null(prj_init$object_negative.init) || !is.null(prj_init$object_positive.init)) {
          # Data initialized but current step is invalid
          if (prj_init$steps != "impute missing value") {
            shinyalert(
              "Step Error",
              "Invalid workflow sequence detected.\nPlease restart from the 'IMPUTE MISSING VALUE' step.",
              type = "error"
            )
            return()
          }
        }
      }


      # Load data based on processing step
      if(prj_init$steps == "impute missing value") {
        if(modes$has_pos) p2_impute_mv$object_pos <- prj_init$object_positive.init
        if(modes$has_neg) p2_impute_mv$object_neg <- prj_init$object_negative.init
      } else {
        p2_impute_mv$object_pos <- data_clean_rv$object_pos_outlier
        p2_impute_mv$object_neg <- data_clean_rv$object_neg_outlier
      }


      # processing core data
      withProgress(message = 'Imputing missing values...', value = 0.5, {
        if(modes$has_pos) {
          para = analy_para()
          p2_impute_mv$object_pos_impute <- perform_imputation(object = p2_impute_mv$object_pos, para = para)
          incProgress(0.2, detail = "Positive mode completed")
        }

        if(modes$has_neg) {
          para = analy_para()
          p2_impute_mv$object_neg_impute <- perform_imputation(object = p2_impute_mv$object_neg, para = para)
          incProgress(0.2, detail = "Negative mode completed")
        }
      })


      # check result
      if((modes$has_pos && is.null(p2_impute_mv$object_pos_impute)) ||
         (modes$has_neg && is.null(p2_impute_mv$object_neg_impute))) {
        shinyalert("Processing Error",
                   "Failed to generate imputation results. Please check parameters.",
                   type = "error")
        return()
      }


      # update massdataset
      data_clean_rv$object_pos_impute <- p2_impute_mv$object_pos_impute
      data_clean_rv$object_neg_impute <- p2_impute_mv$object_neg_impute

      # save massdataset
      tryCatch({
        if(modes$has_pos) {
          object_pos_impute <- p2_impute_mv$object_pos_impute
          output$impute_tbl_pos = renderDataTable_formated(
            action = input$impute_start,
            condition1 = object_pos_impute,
            tbl = object_pos_impute %>% extract_expression_data() %>% rownames_to_column("variable_id"),
            filename.a = "04.expr_mat_imputated_pos.csv"
          )
          save(object_pos_impute,
               file = file.path(prj_init$mass_dataset_dir, "04.object_pos_impute.rda"))
        }
        if(modes$has_neg) {
          object_neg_impute <- p2_impute_mv$object_neg_impute
          output$impute_tbl_neg = renderDataTable_formated(
            action = input$impute_start,
            condition1 = object_neg_impute,
            tbl = object_neg_impute %>% extract_expression_data() %>% rownames_to_column("variable_id"),
            filename.a = "04.expr_mat_imputated_neg.csv"
          )
          save(object_neg_impute,
               file = file.path(prj_init$mass_dataset_dir, "04.object_neg_impute.rda"))
        }
      }, error = function(e) {
        shinyalert("Save Error",
                   paste("Failed to save results:", e$message),
                   type = "error")
      })




      # show process
      output$obj_impute.pos  = check_massdata_info(
        object = p2_impute_mv$object_pos_impute,
        mode = "positive"
      )

      output$obj_impute.neg  = check_massdata_info(
        object = p2_impute_mv$object_neg_impute,
        mode = "negative"
      )



      # success alert
      shinyalert(
        title = "Imputation Completed",
        text = paste(
          "Successfully processed:",
          ifelse(modes$has_pos, "\n- Positive mode", ""),
          ifelse(modes$has_neg, "\n- Negative mode", "")
        ),
        type = "success"
      )
    })
  })
}

