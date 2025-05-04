#' Metabolite original analysis
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
#'
annotation_origin_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "Metabolites origin",
    icon = bs_icon("basket"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "From new uploaded data (option)",
          icon = bsicons::bs_icon("gear"),
          fileInput(
            inputId = ns("saved_obj"),
            label = "Mass_dataset",
            multiple = FALSE,
            buttonLabel = "Browse...",
            placeholder = "No file selected",
            accept = ".rda"
          ),
          fileInput(
            inputId = ns("dblist"),
            label = "Annotation database list",
            multiple = FALSE,
            buttonLabel = "Browse...",
            placeholder = "No file selected",
            accept = ".dblist"
          )
        ),
        accordion_panel(
          title = "Database check",
          icon = bsicons::bs_icon("check2"),
          actionButton(
            inputId = ns("checkdb"),
            label = tooltip(
              trigger = list(
                "Check database",
                bsicons::bs_icon("info-circle")
              ),
              "Check if the database contains metabolites origin information"
            ),
            icon = icon("check")
          )
        ),
        accordion_panel(
          title = "Start analysis",
          icon = bsicons::bs_icon("gear"),
          actionButton(
            inputId = ns("start_origin"),
            label = tooltip(
              trigger = list(
                "Start",
                bsicons::bs_icon("info-circle")
              ),
              "Check if the database contains metabolites origin information"
            ),
            icon = icon("play")
          )
        )
      ),
      page_fluid(
        nav_panel(
          title = "Intersections of metabolites from different sources",
          navset_card_tab(
            title = "upsetplot",
            height = 350,
            full_screen = TRUE,
            sidebar = accordion(
              open = FALSE,
              accordion_panel(
                title = "Parameters",
                textInput(
                  inputId = ns("min_size"),
                  label = tooltip(
                    trigger = list(
                      "min_size",
                      bsicons::bs_icon("info-circle")
                    ),
                    "Integer specifying the minimum size of a set to be included in the plot (default: 1)"
                  ),
                  value = 1
                ),
                textInput(
                  inputId = ns("counts"),
                  label = tooltip(
                    trigger = list(
                      "counts",
                      bsicons::bs_icon("info-circle")
                    ),
                    "Logical specifying whether to show counts in the plot (default: TRUE)"
                  ),
                  value = "TRUE"
                )
              ),
              accordion_panel(
                title = "Download",
                icon = bs_icon("download"),
                textInput(
                  inputId = ns("fig1_height"),
                  label = "Height",
                  value = 7
                ),
                textInput(
                  inputId = ns("fig1_width"),
                  label = "width",
                  value = 7
                ),
                selectInput(
                  inputId = ns("fig1_format"),
                  label = "format",
                  choices = c("jpg", "pdf", "png", "tiff"),
                  selected = "pdf",
                  selectize = FALSE
                ),
                downloadButton(
                  outputId = ns("fig1_download"),
                  label = "Download",
                  icon = icon("download")
                )
              )
            ),
            nav_panel(
              "upsetplot",
              plotOutput(outputId = ns("upsetplot"))
            )
          ),
          navset_card_tab(
            title = "Network plot",
            height = 450,
            full_screen = TRUE,
            sidebar = accordion(
              open = FALSE,
              accordion_panel(
                title = "Parameters",
                textInput(
                  inputId = ns("top_specific_source"),
                  label = tooltip(
                    trigger = list(
                      "top_specific_source",
                      bsicons::bs_icon("info-circle")
                    ),
                    "Integer specifying the maximum number of specific sources to show per source category (default: 5)"
                  ),
                  value = 1
                )
              ),
              accordion_panel(
                title = "Download",
                icon = bs_icon("download"),
                textInput(
                  inputId = ns("fig2_height"),
                  label = "Height",
                  value = 7
                ),
                textInput(
                  inputId = ns("fig2_width"),
                  label = "width",
                  value = 7
                ),
                selectInput(
                  inputId = ns("fig2_format"),
                  label = "format",
                  choices = c("jpg", "pdf", "png", "tiff"),
                  selected = "pdf",
                  selectize = FALSE
                ),
                downloadButton(
                  outputId = ns("fig2_download"),
                  label = "Download",
                  icon = icon("download")
                )
              )
            ),
            nav_panel(
              "Network",
              fluidRow(
                column(
                  width = 12,
                  actionButton(
                    inputId = ns("show_network"),
                    label = "Show network plot",
                    icon = icon("play"),
                    width = "25%"
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  div(
                    style = "max-height: 300px; overflow-y: auto;",
                    dataTableOutput(ns("anno_with_ori"))
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  plotOutput(ns("network_plt"), height = "450px")
                )
              )
            )
          )
        )
      )
    )
  )
}

#' Data metabolite origin analysis
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom dplyr select group_by slice_head
#' @importFrom metid analyze_metabolite_origins
#' @importFrom massdataset activate_mass_dataset extract_annotation_table
#' @param id module of server
#' @param volumes shinyFiles volumes
#' @param prj_init use project init variables.
#' @param data_import_rv reactivevalues mass_dataset export
#' @param data_clean_rv reactivevalues p2 dataclean
#' @param p2_af_filter reactivevalues anno filtering
#' @param data_export_rv reactivevalues mass_dataset export
#' @noRd
annotation_origin_server <- function(id, volumes, prj_init, data_import_rv,
                                     data_clean_rv, data_export_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_ori <- reactiveValues(
      temp_db = NULL,
      object_final = NULL,
      anno_with_ori = NULL
    )
    para_plot1 = reactive({
      list(
        min_size = input$min_size %>% as.numeric(),
        counts = input$counts %>% as.logical()
      )
    })
    para_plot2 = reactive({
      list(
        top_specific_source = input$top_specific_source %>% as.numeric()
      )
    })

    observeEvent(input$checkdb, {
      tryCatch({
        if (!is.null(input$dblist)) {
          data_ori$temp_db <- load_rdata(input$dblist$datapath)
        } else if (!is.null(data_clean_rv$db)) {
          data_ori$temp_db <- data_clean_rv$db
        } else {
          data_ori$temp_db <- prj_init$dblist
        }

        shinyalert(
          title = "Database Validated",
          text = paste(
            "Successfully loaded database with",
            length(data_ori$temp_db), "entries.\n",
            "Database Names:", paste(names(data_ori$temp_db), collapse = ", ")
          ),
          type = "success"
        )
      }, error = function(e) {
        shinyalert(
          title = "Validation Failed",
          text = paste("Error:", e$message),
          type = "error"
        )
      })
    })

    observeEvent(input$start_origin, {
      req(data_ori$temp_db)

      tryCatch({
        shinyalert(
          title = "Analysis in Progress",
          text = "Processing metabolite origin analysis...",
          type = "info",
          timer = 2000,
          showConfirmButton = FALSE
        )

        if (!is.null(input$saved_obj)) {
          data_ori$object_final <- load_rdata(input$saved_obj$datapath)
        } else {
          data_ori$object_final <- data_clean_rv$object_final
        }

        ##> extract Lab.ID as hook
        temp_labid_hook <- data_ori$object_final %>% extract_annotation_table() %>%
          dplyr::select(Lab.ID)
        ##> match metabolite origin from database
        anno_ori <- map_dfr(.x = data_ori$temp_db,.f = function(.x){
          temp_orign_db <- .x@spectra.info %>%
            dplyr::select(Lab.ID,starts_with("from")) %>%
            mutate_if(is.factor,as.character)
          temp_out <- inner_join(temp_labid_hook,temp_orign_db,by = "Lab.ID")
        }) %>% distinct()

        ##> add metabolite origin information to mass_dataset@annotation_table
        data_ori$object_final <-
          data_ori$object_final %>% activate_mass_dataset("annotation_table") %>%
          left_join(anno_ori)
        data_ori$object_final = metid::analyze_metabolite_origins(data_ori$object_final)
        ##> extract final annotation table
        data_ori$anno_with_ori <-  data_ori$object_final %>% extract_annotation_table() %>%
          group_by(variable_id) %>% slice_head(n = 1)

        output$anno_with_ori = renderDataTable_formated(
          actions = input$start_origin,
          condition1 = data_ori$anno_with_ori,
          filename.a = "3.6.6.annotation_neg",
          tbl = data_ori$anno_with_ori %>% dplyr::select(variable_id,Compound.name,Lab.ID)
        )

        output$upsetplot = renderPlot({
          if(is.null(data_ori$object_final)) {return()}
          para = para_plot1()
          metabolite_origin_upsetplot_fix(data_ori$object_final,min_size = para$min_size,counts = para$counts)
        })

        shinyalert(
          title = "Analysis Complete",
          text = paste(
            "Successfully analyzed",
            nrow(data_ori$anno_with_ori),
            "metabolites\n"
          ),
          type = "success"
        )
      }, error = function(e) {
        shinyalert(
          title = "Analysis Error",
          text = paste("Error during analysis:", e$message),
          type = "error"
        )
      })
    })

    observeEvent(input$show_network, {
      if (is.null(data_ori$anno_with_ori)) {
        return()
      }
      if (is.null(data_ori$object_final)) {
        return()
      }



      row_idx = input$anno_with_ori_rows_selected
      if (length(row_idx) == 0) {
        row_idx = 1
      }
      row_content = data_ori$anno_with_ori[row_idx, ]
      temp_id = row_content %>% pull(Lab.ID)
      metabolite_id = temp_id[1]

      output$network_plt <- renderPlot({
        para = para_plot2()
        if (is.null(data_ori$object_final)) { return() }
        metabolite_origin_network(object = data_ori$object_final,metabolite_id = metabolite_id)
      })

      # Show origin info in shinyalert
      req(row_content)
      df <- row_content

      # Identify 'from_' columns
      from_cols <- names(df)[grepl("^from_[^which]", names(df))]
      # Filter for 'Yes' values
      yes_cols <- from_cols[sapply(from_cols, function(col) df[[col]] == "Yes")]

      # Generate HTML content
      html_content <- ""
      for (col in yes_cols) {
        # Extract source name (e.g., 'protist' from 'from_protist')
        source_name <- sub("^from_", "", col)
        # Get corresponding 'from_which_' column
        which_col <- paste0("from_which_", source_name)

        # Get content for 'from_which_' column
        which_content <- df[[which_col]]
        if (which_content == "Unknown") {
          items <- "Unknown"
        } else {
          # Split by '{}' and clean up
          items <- unlist(strsplit(which_content, "\\{\\}"))
          items <- trimws(items) # Remove leading/trailing whitespace
        }

        # Format as HTML
        html_content <- paste0(
          html_content,
          "<h3 style='color: green; font-weight: bold;'>", toupper(source_name), "</h3>",
          paste0("<p>", items, "</p>", collapse = "")
        )
      }

      # Show shinyalert with HTML content
      if (html_content != "") {
        shinyalert(
          title = "Metabolite Origin",
          text = html_content,
          type = "info",
          html = TRUE,
          size = "m"
        )
      } else {
        shinyalert(
          title = "Metabolite Origin",
          text = "No origin information available.",
          type = "info",
          size = "m"
        )
      }
    })

    output$fig1_download <- downloadHandler(
      filename = function() {
        paste("origin_upset", input$fig1_format, sep = ".")
      },
      content = function(file) {
        ggsave(
          file,
          plot = metabolite_origin_upsetplot_fix(data_ori$object_final),
          device = input$fig1_format,
          width = as.numeric(input$fig1_width),
          height = as.numeric(input$fig1_height)
        )
      }
    )

    output$fig2_download <- downloadHandler(
      filename = function() {
        paste("origin_network", input$fig2_format, sep = ".")
      },
      content = function(file) {
        selected_id <- data_ori$anno_with_ori[
          input$anno_with_ori_rows_selected, "Lab.ID"]
        ggsave(
          file,
          plot = metabolite_origin_network(
            data_ori$object_final, selected_id),
          device = input$fig2_format,
          width = as.numeric(input$fig2_width),
          height = as.numeric(input$fig2_height)
        )
      }
    )
  })
}
