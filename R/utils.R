#' getvolumnes for shinyFiles in different OS.
#'
#' Creates a custom styled horizontal rule.
#' @param style A string indicating the style of the horizontal rule.
#' @return An shinyFiles object for volume.
#' @importFrom shinyFiles getVolumes
#' @noRd

get_volumes <- function() {
  if (Sys.info()["sysname"] == "Windows") {
    return(get_volumes_win())
  } else {
    return(shinyFiles::getVolumes()())
  }
}


#' Custom Horizontal Rule
#'
#' Creates a custom styled horizontal rule.
#' @param style A string indicating the style of the horizontal rule.
#' @return An HTML horizontal rule element with custom style.
#' @importFrom shiny tags
#' @noRd

custom_hr <- function(style) {
  tags$hr(style = style)
}

#' Main Horizontal Rule
#'
#' This function creates the main styled horizontal rule used in the UI.
#' @return An HTML horizontal rule element for main style.
#' @noRd

hr_main <- function() {
  custom_hr("border-top: 6px double #008080; border-bottom: 3px solid #008080;")
}

#' Bar Horizontal Rule
#'
#' This function creates a bar styled horizontal rule.
#' @return An HTML horizontal rule element for bar style.
#' @noRd

hr_bar <- function() {
  custom_hr("border-top: 3px dotted #008080;")
}

#' Head Horizontal Rule
#'
#' This function creates a header styled horizontal rule.
#' @return An HTML horizontal rule element for header style.
#' @noRd

hr_head <- function() {
  custom_hr("border: 0; padding-top: 1.5px; background: linear-gradient(to right, transparent, #008080, transparent);")
}


#' ggplot2 themes
#'
#' ggplot2 theme for plots.
#' @return An ggplot2 theme object.
#' @importFrom ggplot2 theme element_rect element_text
#' @noRd
theme1 <- function(){
  temp = theme(
    panel.border = element_rect(linewidth  = 1.5),
    axis.title = element_text(size = 14,color = 'black'),
    axis.text = element_text(size = 12,color = 'black')
  )
  return(temp)
}

#' website logo
#'
#' MetMiner logo.
#' @return shinyDashboardLogoDIY object.
#' @param version version of metminer
#' @importFrom dashboardthemes shinyDashboardLogoDIY
#' @noRd
#'
customLogo <- function(version) {
  shinyDashboardLogoDIY(
    boldText = "Zhang Lab",
    mainText = "MetMiner",
    textSize = 14,
    badgeText = version,
    badgeTextColor = "white",
    badgeTextSize = 2,
    badgeBackColor = "#40E0D0",
    badgeBorderRadius = 3
  )
}

#' adduct extract
#'
#' to extract all adducts from tidymass.
#' @return A string contains clean adducts.
#' @param string adducts string
#' @importFrom stringr str_replace_all
#' @noRd
#'

re_form_reg = function(string){
  string %>% str_replace_all("\\(","\\\\(") %>% str_replace_all("\\)","\\\\)") %>% str_replace_all("\\-","\\\\-")%>% str_replace_all("\\+","\\\\+")
}

#' reformed textInput
#'
#' to extract all adducts from tidymass.
#' @return A string contains clean adducts.
#' @param inpuitID see `shiny::textInput`
#' @param label see `shiny::textInput`
#' @param value see `shiny::textInput`
#' @param placeholder see `shiny::textInput`
#' @param title hover_text of this input box
#' @importFrom stringr str_replace_all
#' @noRd
#'

textInput_div = function(inputId,label,value,placeholder,title) {
  div(
    textInput(
      inputId = inputId,
      label = label,
      value = value,
      placeholder = placeholder
    ),
    title = title
  )
}

#' reformed selectInput_div
#'
#' to extract all adducts from tidymass.
#' @return A string contains clean adducts.
#' @param inpuitID see `shiny::selectInput`
#' @param label see `shiny::selectInput`
#' @param choices see `shiny::selectInput`
#' @param selected see `shiny::selectInput`
#' @param multiple see `shiny::selectInput`
#' @param title hover_text of this input box
#' @importFrom stringr str_replace_all
#' @noRd
#'

selectInput_div = function(inputId,label,choices,selected,multiple,title) {
  div(
    selectInput(
      inputId = inputId,
      label = label,
      choices = choices,
      selected = selected,
      multiple = multiple,
    ),
    title = title
  )
}

#' fetch kegg pathway via ont
#'
#' Get pathway id via KEGG API.
#' @return A dataframe to get term2name.
#' @param ont species id of KEGG database.
#' @importFrom stringr str_replace str_extract
#' @importFrom dplyr mutate
#' @importFrom RCurl getURL
#' @export
#'

get_kegg_pathway_ont <- function(ont) {
  temp_t2n_url = getURL(paste0("https://rest.kegg.jp/list/pathway/",ont))
  TERM2NAME = read.table(text = temp_t2n_url,sep = "\t",header = F)  |>
    setNames(c("TERM","NAME")) |>
    mutate(TERM = str_replace(TERM,ont,"map")) |>
    mutate(NAME = str_extract(NAME, "^.*?(?= -)"))
  return(TERM2NAME)
}


#' fetch kegg compound id belongs to corresponding pathway
#'
#' match kegg cid 2 pathway.
#' @return A dataframe to get term2gene
#' @param compound_id kegg cid.
#' @importFrom stringr str_extract_all
#' @importFrom RCurl getURL
#' @export
#'

get_compound_info <- function(compound_id) {
  base_url <- "https://rest.kegg.jp/get/cpd:"

  url <- paste0(base_url, compound_id)


  result <- tryCatch({
    response <- RCurl::getURL(url)
    pathways <- stringr::str_extract_all(response, "map[0-9]+")[[1]]

    # check
    if (length(pathways) == 0) {
      stop("No pathways found")
    }

    TERM2GENE <- data.frame(
      TERM = pathways,
      GENE = compound_id
    )

    return(TERM2GENE)
  }, error = function(e) {
    # return NA
    message("Error occurred: ", e$message)
    return(data.frame(
      TERM = NA,
      GENE = compound_id
    ))
  })

  return(result)
}


#' export mass_dataset
#'
#' match kegg cid 2 pathway.
#' @return A dataframe to get term2gene
#' @param compound_id kegg cid.
#' @importFrom stringr str_extract_all
#' @importFrom RCurl getURL
#' @export
#'

export_data_for_shiny <- function(object,format,file_path,file_name) {
  check_object_class(object = object, class = "mass_dataset")
  format = match.arg(format)
  if(format == "mass_dataset") {
    save(object,file = file_path)
  }

}

#' validate mass_dataset file
#'
#'
#' @return A character
#' @param path object file path
#' @param expcted_polarity expected polarity of uploaded object.
#' @param object_label name of uploaded object
#' @importFrom stringr str_detect
#' @export
#'
validate_file <- function(path, expected_polarity,object_label) {
  tryCatch({
    # Validation
    if (is.null(path)) {
      return(list(success = FALSE, message = "Path is null"))
    }
    if (!stringr::str_detect(path, "\\.rda$")) {
      return(list(success = FALSE, message = paste("Wrong file format:", object_label)))
    }

    # Load object
    obj_name <- load(path)
    obj <- get(obj_name)

    # Check object class
    if (!inherits(obj, "mass_dataset")) {
      return(list(success = FALSE, message = paste("Wrong object class:", object_label)))
    }

    # Check polarity
    polarity <- tryCatch(
      obj@process_info$process_data@parameter$polarity,
      error = function(e) NA_character_
    )
    if (is.na(polarity) || polarity != expected_polarity) {
      return(list(success = FALSE, message = paste0("Wrong polarity: ", object_label, "\nExpected: ", expected_polarity,
                                                    "\nTested: ", ifelse(is.na(polarity), "NA", polarity))))
    }

    list(success = TRUE, message = NULL)
  }, error = function(e) {
    list(success = FALSE, message = paste("Wrong file uploaded:", object_label))
  })
}


#' validate sample files
#'
#'
#' @return A character
#' @param mode object file path
#' @param QC_num.p QC file number in positive model
#' @param S_num.p Subject file number in positive model
#' @param QC_num.n QC file number in negative model
#' @param S_num.n Subject file number in negative model
#' @param sample_info_temp sample information
#' @return Return a logical value indicating whether the input file matches the sample ID in the sample information.
#' @importFrom stringr str_detect
#' @importFrom shinyalert shinyalert
#' @export
#'
# File correspondence validation
validate_sample_files <- function(
  mode = "POS",
  QC_num.p,
  S_num.p,
  QC_num.n,
  S_num.n,
  sample_info_temp
  ) {
  qc_files <- if (mode == "POS") QC_num.p else QC_num.n
  subj_files <- if (mode == "POS")  S_num.p else S_num.n
  all_files <- c(qc_files, subj_files)

  # Remove file extensions
  clean_files <- gsub("\\.mzXML$", "", all_files, ignore.case = TRUE)

  # Check row count match
  if (nrow(sample_info_temp) != length(all_files)) {
    shinyalert("Error",
               paste("Sample info row count (", nrow(sample_info_temp),
                     ") doesn't match", mode, "files count (", length(all_files), ")"),
               type = "error")
    return(FALSE)
  }

  # Check ID consistency
  mismatch_ids <- setdiff(sample_info_temp$sample_id, clean_files)
  if (length(mismatch_ids) > 0) {
    shinyalert("Error",
               HTML(paste("Sample ID mismatch in", mode, "mode:<br>",
                          paste(mismatch_ids, collapse = "<br>"))),
               type = "error")
    return(FALSE)
  }
  return(TRUE)
}



#' Create empty plot with message
#'
#'
#' @return A ggplot object
#' @param print_messages messages show in plot
#' @return Return a logical value indicating whether the input file matches the sample ID in the sample information.
#' @importFrom ggplot2 annotate theme_void theme element_rect element_blank ggplot
#' @export
#'
#'
gg_message_plot = function(print_messages){
  p = ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = print_messages,
             size = 12, color = "#7f8c8d") +
    theme_void() +
    theme(plot.background = element_rect(fill = "#f5f6fa", color = NA),
          panel.border = element_blank())
  return(p)
}


#' Summarize mass_dataset Object Information
#'
#' Generates a formatted report summarizing key metadata and processing history
#' of a `mass_dataset` object. Designed for integration with Shiny's `renderPrint`.
#'
#' @param object A S4 object of class `mass_dataset` containing metabolomics data
#' @param mode Ionization mode, either `"positive"` or `"negative"` (case-insensitive)
#' @param show_processing Logical indicating whether to display processing history.
#'   Requires `object@process_info` slot. Default: `TRUE`
#' @param show_qc Logical indicating whether to perform QC sample detection.
#'   Checks `sample_type` or `sample_id` columns. Default: `TRUE`
#' @param color Logical enabling ANSI color codes in output.
#'   Set to `FALSE` for Shiny/RMarkdown. Default: `TRUE`
#'
#' @return A character string containing formatted summary report, suitable for
#'   rendering with `cat()` in Shiny `renderPrint` contexts
#'
#' @examples
#' \dontrun{
#' library(massdataset)
#'
#'
#' data(example_dataset)
#'
#' # report in positive model
#' cat(summarize_massdataset(example_dataset, mode = "positive"))
#'
#' # In shiny serve
#' output$summary <- renderPrint({
#'   cat(summarize_massdataset(object(), color = FALSE)
#' })
#' }
#'
#' @importFrom methods slotNames
#' @importFrom utils head tail
#' @importFrom stringr str_to_title

summarize_massdataset <- function(
    object,
    mode = c("positive", "negative"),
    show_processing = TRUE,
    show_qc = TRUE,
    color = TRUE
) {
  # check parameters
  if (missing(object)) stop("No mass_dataset object provided")
  if (!inherits(object, "mass_dataset")) {
    stop("Input must be a mass_dataset S4 object")
  }
  mode <- match.arg(mode)

  # colors
  col_title <- if (color) "\033[1m\033[34m" else ""
  col_reset <- if (color) "\033[0m" else ""
  col_green <- if (color) "\033[32m" else ""
  col_yellow <- if (color) "\033[33m" else ""

  # process init
  output <- character(0)

  # title
  output <- c(output,
              sprintf("%s── massdataset Object Summary (%s Mode) ──%s",
                      col_title, str_to_title(mode), col_reset),
              ""
  )

  # core metadata -------------------------------------------------------------
  output <- c(output,
              sprintf("%sCore Components:%s", col_title, col_reset),
              sprintf("├─ Expression Data: %s × %s variables",
                      format(nrow(object@expression_data), big.mark = ","),
                      format(ncol(object@expression_data), big.mark = ",")),
              sprintf("├─ Sample Info: %s samples × %s metadata",
                      format(nrow(object@sample_info), big.mark = ","),
                      ncol(object@sample_info)),
              sprintf("├─ Variable Info: %s features × %s annotations",
                      format(nrow(object@variable_info), big.mark = ","),
                      ncol(object@variable_info)),
              sprintf("└─ MS2 Spectra: %s",
                      ifelse(length(object@ms2_data) > 0,
                             sprintf("%s spectra", format(length(object@ms2_data), big.mark = ",")),
                             "Not available")),
              ""
  )

  # Processing historical analysis -----------------------------------------------------------
  if (show_processing && length(object@process_info) > 0) {
    proc_list <- object@process_info

    output <- c(output,
                sprintf("%sProcessing History:%s", col_title, col_reset),
                sprintf("├─ Total steps: %d", length(proc_list)))

                # summary steps
                step_names <- names(proc_list)
                unique_steps <- unique(step_names)
                freq_table <- table(step_names)

                # summary step frequencies
                output <- c(output, "├─ Step frequencies:")
                for (step in unique_steps) {
                  output <- c(output,
                              sprintf("│  ├─ %s (×%d)", step, freq_table[[step]])
                  )
                }

                # print details
                last_step <- proc_list[[length(proc_list)]]
                output <- c(output,
                            "└─ Last operation details:",
                            sprintf("   ├─ Step name: %s", names(proc_list)[length(proc_list)]),
                            sprintf("   ├─ Function: %s::%s",
                                    last_step@pacakge_name,
                                    last_step@function_name),
                            sprintf("   ├─ Time: %s",
                                    format(as.POSIXct(last_step@time), "%Y-%m-%d %H:%M:%OS3")),
                            "   └─ Parameters:"
                )

                # data format
                if (length(last_step@parameter) > 0) {
                  params <- last_step@parameter
                  max_len <- max(nchar(names(params)))
                  for (i in seq_along(params)) {
                    param_name <- sprintf(paste0("%-", max_len, "s"), names(params)[i])
                    param_value <- if (length(params[[i]]) > 50) {
                      paste0(substr(params[[i]], 1, 47), "...")
                    } else {
                      params[[i]]
                    }
                    output <- c(output,
                                sprintf("      %s : %s", param_name, param_value)
                    )
                  }
                } else {
                  output <- c(output, "      No parameters recorded")
                }
                output <- c(output, "")

    }


  # Check QCs -------------------------------------------------------------
  if (show_qc) {
    qc_samples <- if ("sample_type" %in% colnames(object@sample_info)) {
      sum(grepl("QC", object@sample_info$sample_type, ignore.case = TRUE))
    } else {
      sum(grepl("QC", object@sample_info$sample_id, ignore.case = TRUE))
    }

    qc_msg <- if (qc_samples > 0) {
      sprintf("%s✔ Contains %d QC samples%s", col_green, qc_samples, col_reset)
    } else {
      sprintf("%s⚠ No QC samples detected%s", col_yellow, col_reset)
    }
    output <- c(output, qc_msg)
  }

  # export
  paste(output, collapse = "\n")
}


#' Generate Formatted Data Summary Report for Shiny
#'
#' Creates a reusable renderPrint component for validating and displaying mass_dataset objects
#'
#' @param object A reactive expression or object to validate (mass_dataset expected)
#' @param mode Ionization mode, either "positive" or "negative" (case-insensitive)
#' @param show_processing Logical to display processing history (default: TRUE)
#' @param show_qc Logical to check QC samples existence (default: TRUE)
#' @param color Enable ANSI colors (disable in Shiny, default: FALSE)
#'
#' @return A renderPrint closure ready for Shiny output assignment
#'
#' @examples
#' \dontrun{
#' # In Shiny server:
#' output$neg_report <- check_massdata_info(
#'   data_import_rv$object_neg_mv,
#'   mode = "negative"
#' )
#' }
#' @export
check_massdata_info <- function(object,
                                mode = c("positive", "negative"),
                                show_processing = TRUE,
                                show_qc = TRUE,
                                color = FALSE) {
  # Validate mode input
  mode <- match.arg(mode, c("positive", "negative"))

  # Return renderPrint closure
  renderPrint({
    # Get actual object value
    obj <- if (is.reactive(object)) object() else object

    # Case 1: Null object
    if (is.null(obj)) {
      cat(sprintf("\n⛔ No %s ion mode data detected\n", mode))
    #  cat("Please perform data processing first\n")
      return(invisible(NULL))
    }

    # Case 2: Invalid object type
    if (!inherits(obj, "mass_dataset")) {
      cat("\n⚠️ Object Validation Failed\n")
      cat("Expected: mass_dataset S4 object\n")
      cat("Actual: ", paste(class(obj), collapse = " > "), "\n")
      return(invisible(NULL))
    }

    # Case 3: Valid object - generate summary
    cat(summarize_massdataset(
      object = obj,
      mode = mode,
      show_processing = show_processing,
      show_qc = show_qc,
      color = color
    ))
  })
}

#' Process Outliers in Mass Spectrometry Data
#'
#' @param object A mass_dataset object
#' @param mv_method Detection method ("By tidymass" or "By myself")
#' @param by_witch Parameters for automated detection (regex patterns)
#' @param outlier_samples Manually specified outlier samples
#' @param outlier_table Precomputed outlier table (for "By tidymass" method)
#'
#' @return A list containing:
#' - $object: Filtered mass_dataset object
#' - $outlier_ids: Identified outlier sample IDs
#' - $message: Processing status messages
#' @export
process_outliers <- function(object,
                             mv_method = c("By tidymass", "By myself"),
                             by_witch = NULL,
                             outlier_samples = NULL,
                             outlier_table = NULL) {
  # check parameters
  mv_method <- match.arg(mv_method)
  if (!inherits(object, "mass_dataset")) {
    return(list(
      error = "Invalid input: object must be a mass_dataset",
      object = NULL,
      outlier_ids = NULL
    ))
  }

  tryCatch({
    if (mv_method == "By tidymass") {
      outlier_ids <- outlier_table %>%
        rownames_to_column("sample_id") %>%
        tidyr::pivot_longer(
          cols = -sample_id,
          names_to = "condition",
          values_to = "judge"
        ) %>%
        dplyr::filter(stringr::str_detect(condition, paste(by_witch, collapse = "|"))) %>%
        dplyr::group_by(sample_id) %>%
        dplyr::summarise(is_outlier = all(judge == TRUE)) %>%
        dplyr::filter(is_outlier) %>%
        dplyr::pull(sample_id)
    } else {
      outlier_ids <- outlier_samples
    }
    if (length(outlier_ids) > 0 && !"none" %in% outlier_ids) {
      filtered_object <- object %>%
        massdataset::activate_mass_dataset(what = "sample_info") %>%
        dplyr::filter(!sample_id %in% outlier_ids)
      return(list(
        object = filtered_object,
        outlier_ids = outlier_ids,
        message = paste("Removed", length(outlier_ids), "outliers")
      ))
    } else {
      return(list(
        object = object,
        outlier_ids = NULL,
        message = "No outliers removed"
      ))
    }
  }, error = function(e) {
    return(list(
      error = paste("Processing failed:", e$message),
      object = NULL,
      outlier_ids = NULL
    ))
  })
}

