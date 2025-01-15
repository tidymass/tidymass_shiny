#' 3d pca plot
#'
#' interactive pca plot.
#' @return A plotly object
#' @param object tidymass-class object
#' @param title plot title
#' @param color_by which column (sample_info) is used to color samples
#' @param x_axis Principal component for x axis, defalut is PC1
#' @param y_axis Principal component for y axis, defalut is PC2
#' @param z_axis Principal component for z axis, defalut is PC3
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom dplyr mutate select pull
#' @importFrom purrr map_chr
#' @export
#'

massqc_pca_3d <-
  function(
    object,
    title = "3D PCA",
    color_by,
    x_axis = "PC1",
    y_axis = "PC2",
    z_axis = "PC3"
  ) {
    if(!is(object = object, class2 = "mass_dataset")){
      stop("obejct should be mass_dataset class.\n")
    }
    if (sum(is.na(object@expression_data)) > 0) {
      warning("MVs in you object,
            \nwill remove variables > 50% and imputate with zero.\n")
      object <-
        object %>%
        massdataset::mutate_variable_na_freq()
      object <-
        object %>%
        massdataset::activate_mass_dataset(what = "variable_info") %>%
        dplyr::filter(na_freq < 0.5)
    }
    sample_info <- object@sample_info
    sample_info <- sample_info %>%
      mutate(hover_text = purrr::map_chr(1:n(), function(i) {
        paste(purrr::map_chr(colnames(sample_info), function(col) {
          paste(col, ":", sample_info[i, col], sep = "")
        }), collapse = "<br>")
      }))
    expression_data <- object@expression_data

    expression_data <-
      expression_data %>%
      apply(1, function(x) {
        x[is.na(x)] = min(x[!is.na(x)])
        x
      }) %>%
      t()
    if (missing(color_by)) {
      color_by <- "no"
    } else{
      if (all(colnames(object@sample_info) != color_by)) {
        stop("no ", color_by, " in sample_info, please check.\n")
      }
    }
    if (all(names(object@process_info) != "scale")) {
      warning("no scale for this dataset, try to scale() before pca.\n")
    }
    pca_object <- prcomp(x = t(as.matrix(expression_data)),center = FALSE,scale. = FALSE)
    temp_variances <- pca_object$sdev^2
    temp_explained_variance <- (temp_variances / sum(temp_variances) * 100) %>% setNames(paste0("PC",1:length(.)))
    pca_rotation = pca_object$x %>% as.data.frame()
    if(nrow(pca_rotation) < 3) {
      stop('not enough dimensions for 3D PCA plot')
    }
    temp_x = pca_rotation %>% pull(x_axis)
    temp_y = pca_rotation %>% pull(y_axis)
    temp_z = pca_rotation %>% pull(z_axis)
    temp_x_title = paste0(x_axis,"(",temp_explained_variance[names(temp_explained_variance) == x_axis] %>% as.numeric() %>% round(.,2),"%)")
    temp_y_title = paste0(y_axis,"(",temp_explained_variance[names(temp_explained_variance) == y_axis] %>% as.numeric() %>% round(.,2),"%)")
    temp_z_title = paste0(z_axis,"(",temp_explained_variance[names(temp_explained_variance) == z_axis] %>% as.numeric() %>% round(.,2),"%)")
    temp_color = sample_info %>% pull(color_by)
    plot = plotly::plot_ly() %>%
      plotly::add_trace(
        x = temp_x,y = temp_y,z = temp_z,
        type = 'scatter3d',mode = 'markers',color = temp_color,
        hovertext = sample_info$hover_text
      ) %>%
      plotly::layout(
        title = list(text = title),
        legend = list(title = list(text = color_by)),
        scene = list(
          xaxis = list(title = temp_x_title),
          yaxis = list(title = temp_y_title),
          zaxis = list(title = temp_z_title)
        )
      )

    return(plot)
  }
