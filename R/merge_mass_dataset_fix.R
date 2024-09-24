#' massdataset merge_mass_dataset fix bugs
#'
#' @return massdataset merge_mass_dataset fix bugs.
#' @param x see merge_mass_dataset
#' @param y see merge_mass_dataset
#' @param sample_direction see merge_mass_dataset
#' @param variable_direction see merge_mass_dataset
#' @param sample_by see merge_mass_dataset
#' @param variable_by see merge_mass_dataset
#' @references massdataset::merge_mass_dataset
#' @importFrom magrittr %>%
#' @importFrom dplyr filter left_join right_join inner_join full_join distinct
#' @importFrom tibble rownames_to_column column_to_rownames
#' @import massdataset
#' @references massdataset::merge_mass_dataset
#'
#' @export
#'
merge_mass_dataset_fix <-
  function(x,y,
           sample_direction = c("left", "right", "full", "inner"),
           variable_direction = c("left", "right", "full", "inner"),
           sample_by = c("sample_id"),
           variable_by = c("variable_id", "mz", "rt")
           ) {
    sample_direction <- match.arg(sample_direction)
    variable_direction <- match.arg(variable_direction)

    #####sample merge
    sample_info_x <- x@sample_info
    sample_info_y <- y@sample_info

    sample_info_note_x <- x@sample_info_note
    sample_info_note_y <- y@sample_info_note

    merger_number <-
      length(x@process_info$merge_mass_dataset) +
      length(y@process_info$merge_mass_dataset) +
      2

    colnames(sample_info_y) <-
      colnames(sample_info_y) %>%
      lapply(function(x) {
        if (x %in% colnames(sample_info_x)) {
          if (!x %in% sample_by) {
            x = paste(x, merger_number, sep = "_")
          }
        }
        x
      }) %>%
      unlist()

    sample_info_note_y$name <- colnames(sample_info_y)

    ####left join
    if (sample_direction == "left") {
      sample_info <-
        sample_info_x %>%
        dplyr::left_join(sample_info_y,
                         by = sample_by)
    }

    if (sample_direction == "right") {
      sample_info <-
        sample_info_x %>%
        dplyr::right_join(sample_info_y,
                          by = sample_by)
    }

    if (sample_direction == "full") {
      sample_info <-
        sample_info_x %>%
        dplyr::full_join(sample_info_y,
                         by = sample_by)
    }

    if (sample_direction == "inner") {
      sample_info <-
        sample_info_x %>%
        dplyr::inner_join(sample_info_y,
                          by = sample_by)
    }

    sample_info_note <-
      rbind(sample_info_note_x,
            sample_info_note_y) %>%
      dplyr::distinct(name, .keep_all = TRUE)

    #####variable merge
    variable_info_x <- x@variable_info
    variable_info_y <- y@variable_info

    variable_info_note_x <- x@variable_info_note
    variable_info_note_y <- y@variable_info_note

    colnames(variable_info_y) <-
      colnames(variable_info_y) %>%
      lapply(function(x) {
        if (x %in% colnames(variable_info_x)) {
          if (!x %in% variable_by) {
            x = paste(x, merger_number, sep = "_")
          }
        }
        x
      }) %>%
      unlist()

    variable_info_note_y$name <- colnames(variable_info_y)

    ####left join
    if (variable_direction == "left") {
      variable_info <-
        variable_info_x %>%
        dplyr::left_join(variable_info_y,
                         by = variable_by)
    }

    if (variable_direction == "right") {
      variable_info <-
        variable_info_x %>%
        dplyr::right_join(variable_info_y,
                          by = variable_by)
    }

    if (variable_direction == "full") {
      variable_info <-
        variable_info_x %>%
        dplyr::full_join(variable_info_y,
                         by = variable_by)
    }

    if (variable_direction == "inner") {
      variable_info <-
        variable_info_x %>%
        dplyr::inner_join(variable_info_y,
                          by = variable_by)
    }

    variable_info_note <-
      rbind(variable_info_note_x,
            variable_info_note_y) %>%
      dplyr::distinct(name, .keep_all = TRUE)

    ####expression_data
    expression_data_x <- x@expression_data
    expression_data_y <- y@expression_data

    expression_data <-
      expression_data_x %>%
      tibble::rownames_to_column(var = "variable_id") %>%
      dplyr::full_join(
        expression_data_y %>%
          tibble::rownames_to_column(var = "variable_id"),
        by = c("variable_id", intersect(
          colnames(expression_data_x),
          colnames(expression_data_y)
        ))
      ) %>%
      tibble::column_to_rownames(var = "variable_id")

    expression_data <-
      expression_data[variable_info$variable_id, sample_info$sample_id]


    ####annotation_table
    annotation_table_x <- x@annotation_table
    annotation_table_y <- y@annotation_table

    if (nrow(annotation_table_x) == 0 &
        nrow(annotation_table_y) == 0) {
      annotation_table <- annotation_table_x
    }

    if (nrow(annotation_table_x) != 0 &
        nrow(annotation_table_y) == 0) {
      annotation_table <- annotation_table_x
    }

    if (nrow(annotation_table_x) == 0 &
        nrow(annotation_table_y) != 0) {
      annotation_table <- annotation_table_y
    }

    if (nrow(annotation_table_x) != 0 &
        nrow(annotation_table_y) != 0) {
      annotation_table <-
        rbind(annotation_table_x,
              annotation_table_y) %>%
        dplyr::filter(variable_id %in% variable_info$variable_id) %>%
        dplyr::distinct(.keep_all = TRUE)
    }

    object <- new(
      Class = "mass_dataset",
      expression_data = expression_data,
      ms2_data = c(x@ms2_data, y@ms2_data), #bug fix
      sample_info = sample_info,
      variable_info = variable_info,
      sample_info_note = sample_info_note,
      variable_info_note = variable_info_note,
      process_info = c(x@process_info, y@process_info),
      version = as.character(utils::packageVersion(pkg = "massdataset")),
      annotation_table = annotation_table
    )

    process_info <- object@process_info

    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "merge_mass_dataset",
      parameter = list(
        sample_direction = sample_direction,
        variable_direction = variable_direction,
        sample_by = sample_by,
        variable_by = variable_by
      ),
      time = Sys.time()
    )

    if (all(names(process_info) != "merge_mass_dataset")) {
      process_info$merge_mass_dataset <- parameter
    } else{
      process_info$merge_mass_dataset <-
        c(process_info$merge_mass_dataset, parameter)
    }

    object@process_info <- process_info

    return(object)
  }
