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
