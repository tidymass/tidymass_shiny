#' Metabolite annotation filtering
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


annotation_filter_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Annotation filtering',
    icon = bs_icon("basket"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Remove redundancy",
          icon = bsicons::bs_icon("stars"),
          radioButtons(
            inputId = ns("af_multi_anno"),
            label = "Multiple annotations",
            choices = c("keep all", "keep top total score", "keep the first one"),
            selected = "keep the first one"
          ),
          radioButtons(
            inputId = ns("af_redundancy"),
            label = "Remove redundancy",
            choices = c("keep all", "keep the first one"),
            selected = "keep all"
          ),
          radioButtons(
            inputId = ns("annotation_levels"),
            label = "Annotation levels",
            choices = c("keep all", "keep level 1 and 2"),
            selected = "keep all"
          )
        ),
        accordion_panel(
          title = "Adduct for level 3 annotation",
          icon = bsicons::bs_icon("gear"),
          selectInput_div(
            inputId = ns('af_column'),
            label = "column type",
            choices = c("rp", "hilic"),
            selected = "rp",
            multiple = F,
            title = "column type"
          ),
          selectInput_div(
            inputId = ns('af_Adduct_pos'),
            label = "Addcut positive model",
            choices = "(M+H)+",
            selected = "(M+H)+",
            multiple = T,
            title = "Addcut based on column type"
          ),
          selectInput_div(
            inputId = ns('af_Adduct_neg'),
            label = "Addcut negative model",
            choices = "(M-H)-",
            selected = "(M-H)-",
            multiple = T,
            title = "Addcut based on column type"
          ),
          radioButtons(
            inputId = ns("feature_remove"),
            label = "Method",
            choices = c("Only annotated features","Only features with MS2 spectra","Both","Keep unknown features"),
            selected = "Both"
          ),
          actionButton(inputId = ns(af_start),label = 'Start',icon = icon("play"))
        )
      ),
      page_fluid(
        nav_panel(
          title = "Annotation filtering",
          navset_card_tab(
            title = "Annotation table",
            height = 400,
            full_screen = TRUE,
            nav_panel("Positive", DT::dataTableOutput(
              outputId = ns("Annotation_filtering_pos")
            )),
            nav_panel("Negative", DT::dataTableOutput(
              outputId = ns("Annotation_filtering_neg")
            )),
            nav_panel(
              shiny::icon("circle-info"),
              markdown("description of noise remove method.")
            )
          ),
          navset_card_tab(
            title = "Check compound annotation in other online database",
            height = 400,
            full_screen = TRUE,
            nav_panel("links"),
            nav_panel(
              shiny::icon("circle-info"),
              markdown("description of noise remove method.")
            )
          ),
          navset_card_tab(
            title = "MS/MS",
            height = 350,
            full_screen = TRUE,

            sidebar = accordion(
              open = FALSE,
              accordion_panel(
                title = 'Parameters',
                materialSwitch(inputId = ns("plot_format"),label = "Interactive plot", status = "primary"),
                radioButtons(
                  inputId = ns("show_mz"),
                  label = "show mz value on plot",
                  choices = c("TRUE","FALSE"),
                  selected = "TRUE"
                ),
                radioButtons(
                  inputId = ns("show_detail"),
                  label = "show match details",
                  choices = c("TRUE","FALSE"),
                  selected = "TRUE"
                )
              ),
              accordion_panel(
                title = 'Download',
                icon = bs_icon('download'),
                textInput(
                  inputId = ns("fig1_height"),label = "Height",value = 7
                ),
                textInput(
                  inputId = ns("fig1_width"),label = "width",value = 7
                ),
                selectInput(
                  inputId = ns("fig1_format"),label = "format",
                  choices = c("jpg","pdf","png","tiff"),
                  selected = "pdf",selectize = F
                ),
                downloadButton(outputId = ns("fig1_download"),label = "Download",icon = icon("download"))
              )
            ),
            nav_panel("Positive",
                      layout_columns(
                        col_widths = c(6,6),
                        dataTableOutput(ns('MS2_pos_tbl')),
                        plotOutput(ns("MS2_pos"))
                      )
                      ),
            nav_panel("Negative",
                      layout_columns(
                        col_widths = c(6,6),
                        dataTableOutput(ns('MS2_neg_tbl')),
                        plotOutput(ns("MS2_neg"))
                      )
            )
          ),
          navset_card_tab(
            title = "Status",
            height = 400,
            full_screen = TRUE,
            nav_panel("Positive", verbatimTextOutput(ns("obj_af.pos"))),
            nav_panel("Negative", verbatimTextOutput(ns("obj_af.neg")))
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
#' @param p2_af_filter reactivevalues anno filtering
#' @param data_export_rv reactivevalues mass_dataset export
#' @noRd


annotation_filter_server <-
  function(id,
           volumes,
           prj_init,
           data_import_rv,
           data_clean_rv,
           data_export_rv,
           p2_af_filter) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      ### 3.6.7 Annotation filtering-----------------------------------------------------

      p2_af_column <- reactive({
        input$af_column %>% as.character()
      })

      ##> addcuts

      observe({
        if (p2_af_column() == "rp") {
          add_pos <- c(
            "(M+H)+","(M+H-H2O)+","(M+H-2H2O)+","(M+NH4)+","(M+Na)+","(M-H+2Na)+","(M-2H+3Na)+","(M+K)+",
            "(M-H+2K)+","(M-2H+3K)+","(M+CH3CN+H)+","(M+CH3CN+Na)+","(2M+H)+","(2M+NH4)+","(2M+Na)+","(M+HCOO+2H)"
          )
          add_neg <- c(
            "(M-H)-","(M-H2O-H)-","(M+Na-2H)-","(M+K-2H)-","(M+NH4-2H)-","(2M-H)-","(M+F)-"
          )
        } else {
          add_pos <- c(
            "(M+H)+","(M+H-H2O)+","(M+H-2H2O)+","(M+NH4)+","(M+Na)+","(M-H+2Na)+","(M-2H+3Na)+",
            "(M+K)+","(M-H+2K)+","(M-2H+3K)+","(M+CH3CN+H)+","(M+CH3CN+Na)+","(2M+H)+","(2M+NH4)+",
            "(2M+Na)+","(2M+K)+","(M+CH3COO+2H)"
          )
          add_neg <- c("(M-H)-","(M-H2O-H)-","(M+Na-2H)-","(M+K-2H)-","(M+NH4-2H)-","(2M-H)-","(M+CH3COO)-")
        }

        updateSelectInput(session, "af_Adduct_pos", choices = add_pos,selected = add_pos[1])
        updateSelectInput(session, "af_Adduct_neg",choices = add_neg,selected = add_neg[1])
      })


# parameters --------------------------------------------------------------

      para_anal = reactive({
        list(
          multi_anno = input$af_multi_anno %>% as.character(),
          redundancy = input$af_redundancy,
          annotation_levels = input$annotation_levels,
          column = input$af_column,
          Adduct_pos = input$af_Adduct_pos %>% as.character(),
          Adduct_neg = input$af_Adduct_neg %>% as.character(),
          feature_remove = input$feature_remove
        )
      })

      para_plot = reactive({
        list(
          show_mz = input$show_mz,
          show_detail = input$show_detail
        )
      })


      observeEvent(
        input$af_start,
        {
          if(!is.null(prj_init$object_negative.init) & !is.null(prj_init$object_positive.init) & !is.null(prj_init$dblist) & prj_init$steps == "Annotation filtering"){
            p2_af_filter$object_neg.anno = prj_init$object_negative.init
            p2_af_filter$object_pos.anno = prj_init$object_positive.init
            p2_af_filter$dblist = prj_init$dblist
          } else {
            if(is.null(data_clean_rv$object_neg.anno)){return()}
            if(is.null(data_clean_rv$object_pos.anno)){return()}
            if(is.null(data_clean_rv$db)){return()}
            p2_af_filter$object_neg.anno = data_clean_rv$object_neg.anno
            p2_af_filter$object_pos.anno = data_clean_rv$object_pos.anno
            p2_af_filter$dblist = data_clean_rv$db
          }
          ##> database

          p2_af_filter$db.name <- purrr::map_chr(.x = 1:length(p2_af_filter$dblist),.f = function(.x) {
            temp_db = p2_af_filter$dblist[[.x]]
            database.name <- paste(temp_db@database.info$Source, temp_db@database.info$Version, sep = "_")
          })
          ##> import parameters
          para = para_anal()
          af_Adduct_pos = para$Adduct_pos
          af_Adduct_neg = para$Adduct_neg

          ##> reform adduct
          if(length(af_Adduct_neg > 1)) {
            af_Adduct_neg = paste0(af_Adduct_neg,collapse = "|")
          }


          if(length(af_Adduct_pos > 1)) {
            af_Adduct_pos = paste0(af_Adduct_pos,collapse = "|")
          }

          ##> addcut filtring
          ##> pos
          p2_af_filter$object_pos.anno <-
            p2_af_filter$object_pos.anno %>%
            activate_mass_dataset("annotation_table") %>%
            mutate(filter_tag_addcut =
                     dplyr::case_when(
                       Level == 3 & str_detect(Adduct,re_form_reg(af_Adduct_pos)) ~ "retain",
                       Level == 1 | Level == 2 ~ "retain",
                       TRUE ~ "remove"
                     )
            )
          ##> neg
          p2_af_filter$object_neg.anno  <-
            p2_af_filter$object_neg.anno %>%
            activate_mass_dataset("annotation_table") %>%
            mutate(filter_tag_addcut =
                     dplyr::case_when(
                       Level == 3 & str_detect(Adduct,re_form_reg(af_Adduct_neg)) ~ "retain",
                       Level == 1 | Level == 2 ~ "retain",
                       TRUE ~ "remove"
                     )
            )
          ##> add tags
          p2_af_filter$object_pos_temp.af <- p2_af_filter$object_pos.anno
          p2_af_filter$object_neg_temp.af <- p2_af_filter$object_neg.anno

          ##> Annotation cleaning for multi-matched annotation
          if(para$multi_anno == "keep top total score") {
            p2_af_filter$object_neg_temp.af =
              p2_af_filter$object_neg_temp.af %>%
              activate_mass_dataset("annotation_table") %>%
              dplyr::group_by(variable_id) %>%
              dplyr::filter(Level == min(Level)) %>%
              dplyr::filter(Total.score == max(Total.score))
            p2_af_filter$object_pos_temp.af =
              p2_af_filter$object_pos_temp.af %>%
              activate_mass_dataset("annotation_table") %>%
              dplyr::group_by(variable_id) %>%
              dplyr::filter(Level == min(Level)) %>%
              dplyr::filter(Total.score == max(Total.score))
          } else if(para$multi_anno == "keep the first one") {
            p2_af_filter$object_neg_temp.af =
              p2_af_filter$object_neg_temp.af %>%
              activate_mass_dataset("annotation_table") %>%
              dplyr::group_by(variable_id) %>%
              dplyr::filter(Level == min(Level)) %>%
              dplyr::filter(Total.score == max(Total.score)) %>%
              dplyr::mutate(order = 1:length(variable_id)) %>%
              dplyr::filter(order == 1) %>% dplyr::select(-order)
            p2_af_filter$object_pos_temp.af =
              p2_af_filter$object_pos_temp.af %>%
              activate_mass_dataset("annotation_table") %>%
              dplyr::group_by(variable_id) %>%
              dplyr::filter(Level == min(Level)) %>%
              dplyr::filter(Total.score == max(Total.score)) %>%
              dplyr::mutate(order = 1:length(variable_id)) %>%
              dplyr::filter(order == 1) %>% dplyr::select(-order)
          }
          ##> Remove redundancy
          if(para$redundancy == "keep the first one") {
            p2_af_filter$object_neg_temp.af =
              p2_af_filter$object_neg_temp.af %>%
              activate_mass_dataset("annotation_table") %>%
              mutate(Compound.name.fix = str_split(Compound.name,";",Inf,T)[,1] %>% stringr::str_to_lower()) %>%
              group_by(Compound.name.fix) %>%
              slice_head(n = 1)
            p2_af_filter$object_pos_temp.af =
              p2_af_filter$object_pos_temp.af %>%
              activate_mass_dataset("annotation_table") %>%
              mutate(Compound.name.fix = str_split(Compound.name,";",Inf,T)[,1] %>% stringr::str_to_lower()) %>%
              group_by(Compound.name.fix) %>%
              slice_head(n = 1)
          }

          ##> plot
          p2_af_filter$pos_clean_anno =
            p2_af_filter$object_pos_temp.af %>%
            extract_annotation_table() %>%
            filter(filter_tag_addcut == "retain")

          p2_af_filter$neg_clean_anno =
            p2_af_filter$object_neg_temp.af %>%
            extract_annotation_table() %>%
            filter(filter_tag_addcut == "retain")

          p2_af_filter$pos_var_id =
            p2_af_filter$pos_clean_anno %>%
            filter(Level == 2) %>% pull(variable_id) %>% unique()

          p2_af_filter$neg_var_id =
            p2_af_filter$neg_clean_anno %>%
            filter(Level == 2) %>% pull(variable_id) %>% unique()

          output$Annotation_filtering_pos = renderDataTable_formated(
            actions = input$af_start,
            condition1 = p2_af_filter$object_pos_temp.af,
            condition2 = p2_af_filter$object_pos.anno,
            filename.a = "3.6.7.AnnoFiltering_pos",
            tbl = p2_af_filter$pos_clean_anno
          )

          p2_af_filter$temp_af_pos_tbl = p2_af_filter$pos_clean_anno %>% dplyr::filter(Level < 3) %>%
            select(variable_id,Compound.name,Lab.ID,Database)

          output$MS2_pos_tbl = renderDataTable_formated(
            actions = input$af_start,
            filename.a = "3.6.7.AnnoFiltering_pos_ms2",
            tbl = p2_af_filter$temp_af_pos_tbl
          )


          output$Annotation_filtering_neg = renderDataTable_formated(
            actions = input$af_start,
            condition1 = p2_af_filter$object_neg_temp.af,
            condition2 = p2_af_filter$object_neg.anno,
            filename.a = "3.6.7.AnnoFiltering_neg",
            tbl = p2_af_filter$neg_clean_anno
          )

          p2_af_filter$temp_af_neg_tbl = p2_af_filter$neg_clean_anno %>% dplyr::filter(Level < 3) %>%
            select(variable_id,Compound.name,Lab.ID,Database)

          output$MS2_neg_tbl= renderDataTable_formated(
            actions = input$af_start,
            filename.a = "3.6.7.AnnoFiltering_neg_ms2",
            tbl = p2_af_filter$temp_af_neg_tbl
          )


          ##> save object

          temp_anno.neg = p2_af_filter$object_neg_temp.af %>%
            extract_annotation_table()

          temp_anno.pos = p2_af_filter$object_pos_temp.af %>%
            extract_annotation_table()

          if(para$feature_remove == "Both") {
            p2_af_filter$object_neg.af = filter_annotations_massdataset(object = p2_af_filter$object_neg.anno,annotate_tbl = temp_anno.neg,method = 'both')
            p2_af_filter$object_pos.af = filter_annotations_massdataset(object = p2_af_filter$object_pos.anno,annotate_tbl = temp_anno.pos,method = 'both')
          } else if(para$feature_remove == "Only features with MS2 spectra") {
            p2_af_filter$object_neg.af = filter_annotations_massdataset(object = p2_af_filter$object_neg.anno,method = 'only ms2')
            p2_af_filter$object_pos.af = filter_annotations_massdataset(object = p2_af_filter$object_pos.anno,method = 'only ms2')
          } else if(para$feature_remove == "Only annotated features") {
            p2_af_filter$object_neg.af = filter_annotations_massdataset(object = p2_af_filter$object_neg.anno,annotate_tbl = temp_anno.neg,method = 'only annotation')
            p2_af_filter$object_pos.af = filter_annotations_massdataset(object = p2_af_filter$object_pos.anno,annotate_tbl = temp_anno.pos,method = 'only annotation')
          } else if(para$feature_remove == "Keep unknown features"){
            p2_af_filter$object_neg.af = p2_af_filter$object_neg_temp.af
            p2_af_filter$object_pos.af = p2_af_filter$object_pos_temp.af
          }

          data_clean_rv$object_neg.af = p2_af_filter$object_neg.af
          data_clean_rv$object_pos.af = p2_af_filter$object_pos.af

          save_massobj(
            polarity = 'positive',
            file_path = paste0(prj_init$wd,"/Result/POS/Objects/"),
            stage = 'af',
            obj = data_clean_rv$object_pos.af)

          save_massobj(
            polarity = 'negative',
            file_path = paste0(prj_init$wd,"/Result/NEG/Objects/"),
            stage = 'af',
            obj = data_clean_rv$object_neg.af)

          ##> status
          output$object_pos.af = renderPrint({
            print(data_clean_rv$object_pos.af)
          })
          output$object_neg.af = renderPrint({
            print(data_clean_rv$object_neg.af)
          })
        }

      )

      observeEvent(input$af_pos_show_plot, {
        tryCatch({
          if (is.null(p2_af_filter$object_pos_temp.af)) {
            return()
          }
          if (is.null(p2_af_filter$pos_clean_anno)) {
            return()
          }

          # plot pos
          show_mz = input$show_mz
          if (show_mz == "TRUE") { show_mz = TRUE } else { show_mz = FALSE }
          show_detail = input$show_detail
          if (show_detail == "TRUE") { show_detail = TRUE } else { show_detail = FALSE }

          # get index
          af_pos_row_idx = input$af_pos_ms2_tbl_rows_selected
          # extract info
          af_pos_row = p2_af_filter$temp_af_pos_tbl[af_pos_row_idx, ]
          p2_af_filter$pos_vari_id = af_pos_row[[1]]
          p2_af_filter$pos_db_name = af_pos_row[[4]]

          temp_idx.pos = match(p2_af_filter$pos_db_name, p2_af_filter$db.name)
          temp_db.pos = p2_af_filter$dblist[[temp_idx.pos]]

          # plot
          p2_af_filter$temp_ms2_match.pos = ms2_plot_mass_dataset_mz(
            object = p2_af_filter$object_pos_temp.af,
            polarity = "positive",
            variable_id = p2_af_filter$pos_vari_id,
            database = temp_db.pos,
            show_mz = show_mz,
            show_detail = show_detail
          )

          # vis
          output$pos_match_mz <- renderUI({
            if (is.null(p2_af_filter$temp_ms2_match.pos)) { return() }
            plot_type <- input$af_plt_format

            if (plot_type) {
              plotlyOutput(outputId = ns("plotly_pos_match_mz"))
            } else {
              plotOutput(outputId = ns("plot_pos_match_mz"))
            }
          })

          output$plot_pos_match_mz <- renderPlot({
            if (is.null(p2_af_filter$temp_ms2_match.pos)) { return() }
            p2_af_filter$temp_ms2_match.pos[[1]]
          })

          output$plotly_pos_match_mz <- renderPlotly({
            if (is.null(p2_af_filter$temp_ms2_match.pos)) { return() }
            plotly::ggplotly(p2_af_filter$temp_ms2_match.pos[[1]])
          })
        }, error = function(e) {
          message("Error occurred: ", e$message)
        })
      })


      observeEvent(input$af_neg_show_plot, {
        tryCatch({
          if (is.null(p2_af_filter$object_neg_temp.af)) {
            return()
          }
          if (is.null(p2_af_filter$neg_clean_anno)) {
            return()
          }

          # plot neg
          show_mz = input$show_mz
          if (show_mz == "TRUE") { show_mz = TRUE } else { show_mz = FALSE }
          show_detail = input$show_detail
          if (show_detail == "TRUE") { show_detail = TRUE } else { show_detail = FALSE }

          af_neg_row_idx = input$af_neg_ms2_tbl_rows_selected
          af_neg_row = p2_af_filter$temp_af_neg_tbl[af_neg_row_idx, ]
          p2_af_filter$neg_vari_id = af_neg_row[[1]]
          p2_af_filter$neg_db_name = af_neg_row[[4]]

          temp_idx.neg = match(p2_af_filter$neg_db_name, p2_af_filter$db.name)
          temp_db.neg = p2_af_filter$dblist[[temp_idx.neg]]

          p2_af_filter$temp_ms2_match.neg = ms2_plot_mass_dataset_mz(
            object = p2_af_filter$object_neg_temp.af,
            polarity = "negative",
            variable_id = p2_af_filter$neg_vari_id,
            database = temp_db.neg,
            show_mz = show_mz,
            show_detail = show_detail
          )

          # mv plot original neg
          output$neg_match_mz <- renderUI({
            plot_type <- input$af_plt_format

            if (plot_type) {
              plotlyOutput(outputId = ns("plotly_neg_match_mz"))
            } else {
              plotOutput(outputId = ns("plot_neg_match_mz"))
            }
          })

          output$plot_neg_match_mz <- renderPlot({
            if (is.null(p2_af_filter$temp_ms2_match.neg)) { return() }
            p2_af_filter$temp_ms2_match.neg[[1]]
          })

          output$plotly_neg_match_mz <- renderPlotly({
            if (is.null(p2_af_filter$temp_ms2_match.neg)) { return() }
            plotly::ggplotly(p2_af_filter$temp_ms2_match.neg[[1]])
          })

        }, error = function(e) {
          message("Error occurred: ", e$message)
        })
      })

      # download ----------------------------------------------------------------
      ###> fig1 =====
      output$fig1_download = downloadHandler(
        filename = function() {
          paste0(p2_af_filter$pos_vari_id,"_ms_ms_mirror_plot.", download_para()$fig1_format)
        },
        content = function(file) {
          # extract parameters
          para_d <- download_para()

          # draw condition
          p = p2_af_filter$temp_ms2_match.pos[[1]]
          # save plot
          ggsave(
            filename = file,
            plot = p,
            width = para_d$fig1_width,
            height = para_d$fig1_height,
            device = para_d$fig1_format
          )
        }
      )
      ###> fig2 ====
      output$fig2_download = downloadHandler(
        filename = function() {
          paste0(p2_af_filter$neg_vari_id,"_ms_ms_mirror_plot.", download_para()$fig2_format)
        },
        content = function(file) {
          # extract parameters
          para_d <- download_para()

          # draw condition
          p = p2_af_filter$temp_ms2_match.neg

          # save plot

          ggsave(
            filename = file,
            plot = p,
            width = para_d$fig2_width,
            height = para_d$fig2_height,
            device = para_d$fig2_format
          )
        }
      )
    })
  }
