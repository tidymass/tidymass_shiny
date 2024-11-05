#' find noise
#'
#' Detected errors or batch effects of your untargetd metabolomics data.
#' @return A ggplot object of missing value.
#' @param object a mass_dataset class
#' @param tag Remove noise based on column names of `sample_info` in object
#' @param qc_na_freq missing value ratio of QC sample
#' @param S_na_freq missing value ratio of tag groups
#' @importFrom massdataset extract_sample_info activate_mass_dataset mutate_variable_na_freq extract_variable_info
#' @importFrom magrittr %>%
#' @importFrom dplyr rename mutate case_when filter across left_join anti_join pull
#' @importFrom purrr map reduce
#' @references based on  massdataset::mutate_variable_na_freq
#'
#' @noRd
#' @export
#'
find_noise = function(object,tag = "class",qc_na_freq = 0.2,S_na_freq = 0.5) {

  if(tag == "class") {
    temp_sample_info = object %>%
      extract_sample_info() %>%
      mutate(key = class)
  } else {
    temp_sample_info = object %>%
      extract_sample_info() %>%
      dplyr::rename("tag" = tag) %>%
      dplyr::rename("xx" = "class") %>%
      dplyr::mutate(key = case_when(
        xx == "QC" ~ "QC",
        TRUE ~ tag
      ))
  }

  temp_keys = temp_sample_info %>% pull(key) %>% unique()

  object <-
    object %>%
    activate_mass_dataset("sample_info") %>%
    left_join(temp_sample_info %>% select(sample_id,key),by = "sample_id")
  #> na_freq
  for (i in 1:length(temp_keys)) {
    temp_id = object %>%
      activate_mass_dataset(what = "sample_info") %>%
      filter(key == temp_keys[i]) %>%
      pull(sample_id)

    if (i == 1) {
      object <- object %>%
        mutate_variable_na_freq(according_to_samples = temp_id) %>%
        activate_mass_dataset(what = "variable_info")
    } else {
      object <- object %>%
        mutate_variable_na_freq(according_to_samples = temp_id)

      colnames_vari = object %>%
        extract_variable_info() %>%
        colnames()


      na_freq_cols = colnames_vari[grep("na_freq", colnames_vari)]
      na_freq_filter = purrr::map(na_freq_cols, ~ object %>% pull(.x) <= S_na_freq)
      na_freq_check = purrr::reduce(na_freq_filter, `|`)
      object_mv = object %>%
        activate_mass_dataset(what = "variable_info") %>%
        filter(na_freq <= qc_na_freq & na_freq_check)
    }
  }


  vari_ori <- object %>% extract_variable_info()
  vari_filter <- object_mv %>% extract_variable_info()
  vari_noisy = anti_join(vari_ori ,vari_filter, by = "variable_id")
  out = list(
    noisy_tbl = vari_noisy,
    object_mv = object_mv
  )
  return(out)

}


