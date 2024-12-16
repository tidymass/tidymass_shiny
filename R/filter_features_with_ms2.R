#' filter features with ms2
#'
#' Remove features without MS2 matches.
#' @return A ggplot object of missing value.
#' @param object a mass_dataset class
#' @param annotate_tbl filtered annotation table.
#' @param method filter method,"only ms2","only annotation","both"
#' @importFrom massdataset activate_mass_dataset extract_ms2_data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter across left_join distinct mutate group_by slice_head ungroup
#' @references tiydMass object massdataset
#'
#' @export
#'

filter_annotations_massdataset = function(object,annotate_tbl = NULL,method = c("only ms2","only annotation","both")) {
  ##> check objects
  if(class(object) != 'mass_dataset') {
    stop("need mass_dataset object generated from tidyMass.\n")
  } else if (length(object@ms2_data) == 0) {
    stop("No MS2 in you object.\n")
  }
  ##> chech method
  method = match.arg(method)
  ##> extract features with ms2 spectra
  temp_ms2 <- object %>% extract_ms2_data()
  names(temp_ms2) = 'files'## simplify ms2 name
  temp_filter_tags = data.frame(
    variable_id = temp_ms2$files@variable_id,
    filter_tag = "with_ms2"
  )
  if(method == "only ms2") {
    temp_filter_tags = temp_filter_tags
  } else if (method == "only annotation" & !is.null(annotate_tbl)) {
    temp_filter_tags = annotate_tbl %>%
      dplyr::select(variable_id) %>%
      dplyr::distinct() %>%
      dplyr:: mutate(filter_tag = "with_annotation")
  } else if (method == "both" & !is.null(annotate_tbl)){
    temp_filter_anno = annotate_tbl %>%
      dplyr::select(variable_id) %>%
      dplyr::distinct() %>%
      dplyr::mutate(filter_tag = "with_annotation")
    temp_filter_tags =
      rbind(temp_filter_tags,temp_filter_anno) %>%
      dplyr::group_by(variable_id) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup()
  } else {
    stop("No annotation table.")
  }

  object = object %>%
    activate_mass_dataset('variable_info') %>%
    dplyr::left_join(temp_filter_tags,by = 'variable_id') %>%
    dplyr::filter(str_detect(filter_tag,"with"))

  return(object)
}
