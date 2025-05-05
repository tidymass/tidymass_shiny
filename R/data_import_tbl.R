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


data_import_tbl_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Start with table file',
    icon = bs_icon("upload"),
    layout_sidebar(
      sidebar = sidebar(
        fileInput(
          inputId = ns('expmat'),
          label = 'Peak picking table',
          multiple = FALSE,
          accept = '.csv'
        ),
        radioButtons(
          inputId = ns("RT_tbl"),
          label = 'Retention time in',choices = c('minute','second'),
          selected = 'second'
        ),
        selectInput(
          inputId = ns("exp_vari_id"),label = "variable id",choices = c('variable_id','mz','rt','ion'),
          selected = 'variable_id'
        ),
        selectInput(
          inputId = ns("exp_mz"),label = "mz",choices = c('variable_id','mz','rt','ion'),
          selected = 'mz'
        ),
        selectInput(
          inputId = ns("exp_rt"),label = "retention time",choices = c('variable_id','mz','rt','ion'),
          selected = 'rt'
        ),
        selectInput(
          inputId = ns("exp_ion"),label = "polarity",choices = c('variable_id','mz','rt','ion'),
          selected = 'ion'
        )
      ),
      page_fluid(
        nav_panel(
          title = "File check",
          icon = bsicons::bs_icon("inbox"),
          actionButton(ns('action1.1'),'Input file summary',icon = icon("computer-mouse"),width = "15%"),
          tags$h3("Summary of input file",style = 'color: black'),
          htmlOutput(ns("file_check2")),
          navset_card_tab(
            height = 350,
            full_screen = TRUE,
            title = "MS data summary",
            nav_panel(
              "variable_info",
              card_title("variable information"),
              DT::dataTableOutput(ns("tbl_variable_info")),
            ),
            nav_panel(
              "expression",
              card_title("expression table"),
              DT::dataTableOutput(ns("tbl_expmat"))
            )
          ),
          actionButton(ns('action2.1'),'Generate massdataset object',icon = icon("computer-mouse"),width = "25%"),
          tags$h3("Output file path",style = 'color: black'),
          htmlOutput(ns("obj_mass_res_path")),
          navset_card_tab(
            height = 350,
            full_screen = TRUE,
            title = "Status",
            nav_panel(
              "Positive",
              card_title("Positive model"),
              verbatimTextOutput(ns("obj_mass_check.pos_tbl"))
            ),
            nav_panel(
              "negative",
              card_title("Negative model"),
              verbatimTextOutput(ns("obj_mass_check.neg_tbl"))
            )
          )
        )
      )
    )
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
#' @importFrom stringr str_detect regex
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom massdataset create_mass_dataset mutate_ms2
#' @importFrom magrittr %>%
#' @param id module of server
#' @param volumes shinyFiles volumes
#' @param prj_init use project init variables.
#' @param data_import_rv reactivevalues mass_dataset export
#' @noRd


data_import_tbl_server <- function(id,volumes,prj_init,data_import_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ## 3.4 import from tbl ---------------------------------------------------------
    observeEvent(input$toggleSidebar1, {
      shinyjs::toggle(id = "Sidebar1")
    })
    #> variable table
    temp_expmat_vari_tbl <- reactive({
      file1 <- input$expmat
      if(is.null(file1)){return()}
      read.csv(file = file1$datapath,
               sep=",",header = T,stringsAsFactors = F)
    })

    vari_info_col1 = reactive({
      colnames(temp_expmat_vari_tbl() %>%  as.data.frame())
    })
    #> match variable_info table

    observe({
      updateSelectInput(session, "exp_vari_id",choices = vari_info_col1(),selected = vari_info_col1()[1])
      updateSelectInput(session, "exp_mz",choices = vari_info_col1(),selected = vari_info_col1()[2])
      updateSelectInput(session, "exp_rt",choices = vari_info_col1(),selected = vari_info_col1()[3])
      updateSelectInput(session, "exp_ion",choices = vari_info_col1(),selected = vari_info_col1()[4])
    })


    #> File check
    para_tbl_check <- reactiveValues(data = NULL)

    #> File check event
    observeEvent(
      input$action1.1,
      {
        if(is.null(input$expmat)){return()}

        #> variable information
        para_tbl_check$RT_tbl = as.character(input$RT_tbl)
        para_tbl_check$temp_vari_exp =
          temp_expmat_vari_tbl() %>%
          as.data.frame()
        #> match variable information
        para_tbl_check$variable_id_n = as.character(input$exp_vari_id)
        para_tbl_check$mz_n = as.character(input$exp_mz)
        para_tbl_check$rt_n = as.character(input$exp_rt)
        para_tbl_check$ion_n = as.character(input$exp_ion)

        #> format files
        para_tbl_check$temp_vari_exp =
          para_tbl_check$temp_vari_exp %>%
          dplyr::rename(
            "variable_id" = para_tbl_check$variable_id_n,
            "mz" = para_tbl_check$mz_n,
            "rt" = para_tbl_check$rt_n,
            "ion" = para_tbl_check$ion_n
          )
        #> minute to second
        if(para_tbl_check$RT_tbl == "minute") {
          para_tbl_check$vari_info =
            para_tbl_check$temp_vari_exp %>%
            select(variable_id,mz,rt,ion) %>%
            mutate(rt = rt*60)
        } else {
          para_tbl_check$vari_info =
            para_tbl_check$temp_vari_exp %>%
            select(variable_id,mz,rt,ion)
        }

        if(str_detect(para_tbl_check$vari_info %>%  pull(ion) %>%  unique(),regex("\\+|pos|\\-|neg",ignore_case = T))[1]){
          para_tbl_check$ion_judge = "Pass"
          para_tbl_check$vari_info =
            para_tbl_check$vari_info %>%
            mutate(ion = case_when(
              str_detect(ion,regex("\\+|pos",ignore_case = T)) ~ "pos",
              str_detect(ion,regex("\\-|neg",ignore_case = T)) ~ "neg"
            ))
          para_tbl_check$polarity = para_tbl_check$vari_info %>% pull(ion) %>% unique()

        } else {
          para_tbl_check$ion_judge = "Faild, make sure there is polarity tags in the variable information table such as +,-,pos,neg"
        }

        output$tbl_variable_info = renderDataTable_formated(
          actions = input$action1.1,
          condition1 = input$expmat,
          condition2 = para_tbl_check$vari_info,
          tbl = para_tbl_check$vari_info,filename.a = "3.4.tblImport_vari_info_check"
        )


        #> expmat
        para_tbl_check$temp_exp =
          para_tbl_check$temp_vari_exp %>%
          select(-mz,-rt,-ion) %>%
          column_to_rownames("variable_id") %>%
          mutate_if(is.character,as.numeric)

        output$tbl_expmat = renderDataTable_formated(
          actions = input$action1.1,
          condition1 = input$expmat,
          condition2 = para_tbl_check$temp_exp,
          tbl = para_tbl_check$temp_exp  %>%  rownames_to_column("variable_id"),
          filename.a = "3.4.tblImport_expmat_check"
        )


        #> check expmat and sample information
        para_tbl_check$vari_sample_info = prj_init$sample_info %>%  pull(sample_id) %>%  sort()
        para_tbl_check$vari_vari_info = colnames(para_tbl_check$temp_exp ) %>%  sort()

        diff1 <- setdiff(para_tbl_check$vari_sample_info, para_tbl_check$vari_vari_info)
        diff2 <- setdiff(para_tbl_check$vari_vari_info, para_tbl_check$vari_sample_info)

        if (length(diff1) + length(diff2) > 0) {
          # details print
          mismatch_info <- paste(
            if (length(diff1) > 0) paste("Missing in data:", paste(diff1, collapse = ", ")),
            if (length(diff2) > 0) paste("Extra in data:", paste(diff2, collapse = ", ")),
            sep = "<br>"
          )
          para_tbl_check$sample_match <- paste0("<span style='color:red;'>Mismatch found!</span><br>", mismatch_info)
        } else {
          para_tbl_check$sample_match <- "<span style='color:green;'>✓ All samples match</span>"
        }


        para_tbl_check$sample_subject <- prj_init$sample_info %>%
          filter(class == "Subject") %>%
          nrow()

        para_tbl_check$sample_QC <- prj_init$sample_info %>%
          filter(class == "QC") %>%
          nrow()

        alert_content <- paste0(
          "<div style='text-align: left; font-size: 14px;'>",
          "<h4 style='color: #2c3e50; margin-top: 0;'>Data Validation Report</h4>",

          # 样本匹配模块
          "<div style='margin-bottom: 12px;'>",
          "<b>🔍 Sample ID Matching:</b><br>",
          para_tbl_check$sample_match,
          "</div>",

          # 极性检查模块
          "<div style='margin-bottom: 12px;'>",
          "<b>⚡ Polarity Check:</b><br>",
          "<span style='color:", ifelse(grepl("pass", para_tbl_check$ion_judge), "#27ae60", "#e74c3c"), "'>",
          para_tbl_check$ion_judge,
          "</span></div>",

          # 样本统计模块
          "<div style='border-top: 1px solid #eee; padding-top: 8px;'>",
          "<b>📊 Sample Statistics:</b><br>",
          "• QC Samples: ", para_tbl_check$sample_QC, "<br>",
          "• Subject Samples: ", para_tbl_check$sample_subject, "<br>",
          "• Total Features: ", nrow(para_tbl_check$temp_exp),
          "</div>",

          "</div>"
        )

        # 显示弹窗
        shinyalert(
          title = "Validation Results",
          text = alert_content,
          html = TRUE,
          type = ifelse(grepl("Mismatch", para_tbl_check$sample_match), "error", "success"),
          closeOnEsc = TRUE,
          showConfirmButton = TRUE,
          confirmButtonText = "OK",
          animation = "pop",
          className = "validation-alert"
        )
      }
    )

    #> generate mass dataset
    # data_import_rv<- reactiveValues(data = NULL)

    observeEvent(
      input$action2.1,
      {
        req(input$expmat)
        current_polarity <- para_tbl_check$polarity

        pro_step_tbl <- c(
          if ("pos" %in% current_polarity) 'Create mass_dataset class: Positive model...',
          if ("neg" %in% current_polarity) 'Create mass_dataset class: Negative model...',
          'All finish'
        ) %>% compact()

        data_import_rv$tbl_ms2_mz_tol = input$tbl_ms2_mz_tol %>%  as.numeric()
        data_import_rv$tbl_ms2_rt_tol = input$tbl_ms2_rt_tol %>%  as.numeric()
        #functions
        withProgress(
          message = 'Creating mass_dataset',
          value = 0,
          expr = {
            # 正模式处理
            if ("pos" %in% current_polarity) {
              incProgress(1/length(pro_step_tbl),
                          detail = "Processing positive mode...")
              variable_pos =
                para_tbl_check$vari_info %>%
                filter(ion == "pos") %>%
                select(variable_id)
              ##> pos sample informations
              sample_info_pos = prj_init$sample_info
              ##> pos expression table
              expression_data_pos =
                para_tbl_check$temp_exp %>%
                rownames_to_column("variable_id") %>%
                inner_join(variable_pos) %>%
                column_to_rownames("variable_id") %>%
                select(sample_info_pos %>%  pull(sample_id))
              ##> pos variables informations
              variable_info_pos =
                para_tbl_check$vari_info %>%
                filter(ion == "pos")
              ##> pos mass datasets
              data_import_rv$object_pos_raw =
                create_mass_dataset(
                  expression_data = expression_data_pos,
                  sample_info = sample_info_pos,
                  variable_info = variable_info_pos
                )
              object_pos_raw <- data_import_rv$object_pos_raw
              save(object_pos_raw,
                   file = file.path(prj_init$mass_dataset_dir, "01.object_pos_raw.rda"))
            }

            # 负模式处理
            if ("neg" %in% current_polarity) {
              incProgress(1/length(pro_step_tbl),
                          detail = "Processing negative mode...")
              variable_neg =
                para_tbl_check$vari_info %>%
                filter(ion == "neg") %>%
                select(variable_id)
              ##> neg expression table
              ##> neg sample informations
              sample_info_neg = prj_init$sample_info
              expression_data_neg =
                para_tbl_check$temp_exp %>%
                rownames_to_column("variable_id") %>%
                inner_join(variable_neg) %>%
                column_to_rownames("variable_id") %>%
                select(sample_info_neg %>%  pull(sample_id))
              ##> neg variables informations
              variable_info_neg =
                para_tbl_check$vari_info %>%
                filter(ion == "neg")
              ##> neg mass datasets
              data_import_rv$object_neg_raw =
                create_mass_dataset(
                  expression_data = expression_data_neg,
                  sample_info = sample_info_neg,
                  variable_info = variable_info_neg
                )
              object_neg_raw <- data_import_rv$object_neg_raw
              save(object_neg_raw,
                   file = file.path(prj_init$mass_dataset_dir, "01.object_neg_raw.rda"))
            }


            incProgress(1/length(pro_step_tbl), detail = "Finalizing...")
          }
        )


        # generate dynamic results
        alert_content <- paste0(
          "<div style='text-align: left; font-size: 14px;'>",
          "<h4 style='color: #2c3e50; margin-top: 0;'>Analysis Results</h4>"
        )

        # add pos path
        if ("pos" %in% current_polarity && dir.exists(file.path(prj_init$mass_dataset_dir))) {
          alert_content <- paste0(
            alert_content,
            "<div style='margin-bottom: 10px;'>",
            "<span style='display: inline-block; width: 120px; color: #3498db;'>Positive mode:</span>",
            "<a href='", paste0("file://", gsub(" ", "%20", prj_init$mass_dataset_dir)),
            "' style='color: #27ae60; text-decoration: underline;' target='_blank'>View POS results</a>",
            "</div>"
          )
        }

        # add neg path
        if ("neg" %in% current_polarity && dir.exists(file.path(prj_init$mass_dataset_dir))) {
          alert_content <- paste0(
            alert_content,
            "<div style='margin-bottom: 10px;'>",
            "<span style='display: inline-block; width: 120px; color: #3498db;'>Negative mode:</span>",
            "<a href='", paste0("file://", gsub(" ", "%20", prj_init$mass_dataset_dir)),
            "' style='color: #e74c3c; text-decoration: underline;' target='_blank'>View NEG results</a>",
            "</div>"
          )
        }

        # add summary
        alert_content <- paste0(
          alert_content,
          "<div style='border-top: 1px solid #eee; padding-top: 8px; margin-top: 12px;'>",
          "<b>Processed modes:</b> ", paste(current_polarity, collapse = " + "), "<br>",
          "<b>Total variables:</b> ", nrow(para_tbl_check$temp_exp),
          "</div></div>"
        )


        shinyalert(
          title = "Analysis Results",
          text = alert_content,
          html = TRUE,
          type = ifelse(length(current_polarity) > 0, "success", "warning"),
          closeOnEsc = TRUE,
          showConfirmButton = TRUE,
          confirmButtonText = "OK",
          animation = "pop",
          className = "result-alert"
        )
        output$obj_mass_check.pos_tbl = check_massdata_info(
          object = data_import_rv$object_pos_raw,
          mode = "positive"
        )

        output$obj_mass_check.neg_tbl = check_massdata_info(
          object = data_import_rv$object_neg_raw,
          mode = "negative"
        )


      }
    )
  })
}

