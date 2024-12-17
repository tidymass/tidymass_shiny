#' Mirror plot between feature MS2 spectra and MS2 database spectra
#'
#' @return A ggplot object of mirror plot.
#' @param object see ms2_plot_mass_dataset.
#' @param variable_id see ms2_plot_mass_dataset.
#' @param variable_index see ms2_plot_mass_dataset.
#' @param polarity see ms2_plot_mass_dataset.
#' @param ms1.match.ppm see ms2_plot_mass_dataset.
#' @param ms2.match.ppm see ms2_plot_mass_dataset.
#' @param mz.ppm.thr see ms2_plot_mass_dataset.
#' @param database see ms2_plot_mass_dataset.
#' @param interactive_plot see ms2_plot_mass_dataset.
#' @param show_mz show m/z on plot.
#' @param show_detail show match details on plot.
#' @importFrom massdataset activate_mass_dataset extract_ms2_data
#' @importFrom magrittr %>%
#' @importFrom purrr walk map
#' @importFrom dplyr filter
#' @importFrom ggplot2 annotate
#' @importFrom plotly ggplotly
#' @import metid
#' @references metid::ms2_plot_mass_dataset metminer::ms2_plot_mass_dataset_mz
#'
#' @export
#'

ms2_plot_mass_dataset_mz = function (object,
                                     variable_id,
                                     variable_index,
                                     polarity = c("positive", "negative"),
                                     ms1.match.ppm = 25,
                                     ms2.match.ppm = 30,
                                     mz.ppm.thr = 400, database,
                                     interactive_plot = FALSE,
                                     show_mz = TRUE,
                                     show_detail = TRUE)
{
  polarity = match.arg(polarity)
  massdataset::check_object_class(object = object, class = "mass_dataset")
  if (nrow(object@annotation_table) == 0) {
    stop("No annotation in object.\n")
  } else {
    if (all(object@annotation_table$Level != 1) & all(object@annotation_table$Level !=2)) {
      stop("No annotations with MS2.\n")
    }
  }
  if (!is.numeric(ms1.match.ppm)) {
    stop("ms1.match.ppm should be numeric.\n")
  } else {
    if (ms1.match.ppm <= 0 | ms1.match.ppm >= 500) {
      stop("ms1.match.ppm should > 0 and < 500\n")
    }
  }
  if (!is.numeric(ms2.match.ppm)) {
    stop("ms2.match.ppm should be numeric.\n")
  } else {
    if (ms2.match.ppm <= 0 | ms2.match.ppm >= 500) {
      stop("ms2.match.ppm should > 0 and < 500\n")
    }
  }
  if (missing(database)) {
    stop("No database is provided.\n")
  }
  if (!is(database, "databaseClass")) {
    stop("database should be databaseClass object.\n")
  }
  database.name <- paste(database@database.info$Source, database@database.info$Version, sep = "_")
  if (missing(variable_id) & missing(variable_index)) {
    stop("provide variable_id or variable_index.\n")
  }
  if (!missing(variable_id)) {
    purrr::walk(variable_id, .f = function(temp_variable_id) {
      if (!temp_variable_id %in% object@variable_info$variable_id) {
        stop(paste(temp_variable_id, "is not in variable_info.\n"))
      }
    })
    variable_index = match(variable_id, object@variable_info$variable_id)
  } else {
    purrr::walk(variable_index, .f = function(temp_variable_index) {
      if (temp_variable_index <= 0 | temp_variable_index >
          nrow(object@variable_info)) {
        stop("variable_index ", temp_variable_index," should be range from 1 to ", nrow(object@variable_info))
      }
    })
  }
  variable_id <- object@variable_info$variable_id[variable_index] %>%
    unique()
  temp_variable_id <- variable_id
  temp_annotation_table <-
    object@annotation_table %>%
    dplyr::filter(variable_id == temp_variable_id) %>%
    dplyr::filter(!is.na(SS)) %>%
    dplyr::filter(Database == database.name)
  if (nrow(temp_annotation_table) == 0) {
    message(paste(temp_variable_id, "has no annotation with MS2."))
    return(NULL)
  }
  all_plot <- purrr::map(as.data.frame(t(temp_annotation_table)),.f = function(x) {
    temp_idx <- which(object@ms2_data[[x[2]]]@ms2_spectrum_id == x[3])[1]
    spectrum1 <- object@ms2_data[[x[2]]]@ms2_spectra[[temp_idx]]
    spectrum2 <- get_ms2_spectrum(lab.id = x[8], polarity = polarity,database = database, ce = x[14])
    if (is.null(spectrum2)) {
      message("database may be wrong.")
      plot <- ms2_plot_fix(spectrum1 = spectrum1, show_mz = show_mz,
                           spectrum1_name = x[1], spectrum2_name = x[4],
                           ppm.tol = ms1.match.ppm, mz.ppm.thr = ms2.match.ppm,
                           interactive_plot = FALSE)
    }
    else {
      plot <- ms2_plot_fix(spectrum1 = spectrum1, show_mz = show_mz,
                           spectrum2 = spectrum2, spectrum1_name = x[1],
                           spectrum2_name = x[4], ppm.tol = ms1.match.ppm,
                           mz.ppm.thr = ms2.match.ppm, interactive_plot = FALSE)
    }
    temp_info = paste(colnames(temp_annotation_table),x, sep = ":")
    if(show_detail){
      plot <- plot + ggplot2::annotate(geom = "text", x = -Inf,
                                       y = Inf, label = paste(temp_info, collapse = "\n"),
                                       hjust = 0, vjust = 1)
    }

    if (interactive_plot) {
      plot <- plotly::ggplotly(plot)
    }
    plot
  })
  names(all_plot) <- paste(temp_annotation_table$variable_id,
                           seq_len(nrow(temp_annotation_table)), sep = "_")
  return(all_plot)
}



#' Mirror plot between feature MS2 spectra and MS2 database spectra
#'
#' @return A ggplot object of mirror plot.
#' @param spectrum1 see ms2_plot.
#' @param spectrum2 see ms2_plot.
#' @param spectrum1_name see ms2_plot.
#' @param spectrum2_name see ms2_plot.
#' @param range.mz see ms2_plot.
#' @param ppm.tol see ms2_plot.
#' @param mz.ppm.thr see ms2_plot.
#' @param xlab see ms2_plot.
#' @param ylab see ms2_plot.
#' @param col1 see ms2_plot.
#' @param col2 see ms2_plot.
#' @param title.size see ms2_plot.
#' @param lab.size see ms2_plot.
#' @param axis.text.size see ms2_plot.
#' @param legend.title.size see ms2_plot.
#' @param legend.text.size see ms2_plot.
#' @param show_mz show m/z on plot.
#' @param interactive_plot see ms2_plot.
#' @importFrom massdataset activate_mass_dataset extract_ms2_data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @importFrom plotly ggplotly
#' @import metid
#' @references metid::ms2_plot_mass_dataset
#'
#' @noRd
#'

ms2_plot_fix <- function(spectrum1,
                         spectrum2,
                         spectrum1_name = "spectrum1",
                         spectrum2_name = "spectrum2",
                         range.mz,
                         ppm.tol = 30,
                         mz.ppm.thr = 400,
                         xlab = "Mass to charge ratio (m/z)",
                         ylab = "Relative intensity",
                         col1 = "red",
                         col2 = "black",
                         title.size = 15,
                         lab.size = 15,
                         axis.text.size = 15,
                         legend.title.size = 15,
                         legend.text.size = 15,
                         show_mz = FALSE,
                         interactive_plot = FALSE) {
  if (missing(spectrum1) & missing(spectrum2)) {
    stop("No spectrum1 and spectrum2")
  }
  if (!missing(spectrum1)) {
    spectrum1 <-
      apply(spectrum1, 2, function(x) {
        as.numeric(x)
      }) %>%
      as.data.frame()

    spectrum1[, 2] <- spectrum1[, 2] / max(spectrum1[, 2])
  }

  if (!missing(spectrum2)) {
    spectrum2 <-
      apply(spectrum2, 2, function(x) {
        as.numeric(x)
      }) %>%
      as.data.frame()
    spectrum2[, 2] <- spectrum2[, 2] / max(spectrum2[, 2])
  }

  ## two spectrum

  if (!missing(spectrum1) & !missing(spectrum2)) {
    if (missing(range.mz)) {
      range.mz <- c(
        min(spectrum1[, 1], spectrum2[, 1]),
        max(spectrum1[, 1], spectrum2[, 1])
      )
    }

    matched.spec <- ms2Match(spectrum1,
                             spectrum2,
                             ppm.tol = ppm.tol,
                             mz.ppm.thr = mz.ppm.thr
    )

    matched.idx <- which(matched.spec[, "Lib.intensity"] > 0 &
                           matched.spec[, "Exp.intensity"] > 0)

    plot <- ggplot(matched.spec) +
      geom_segment(
        mapping = aes(
          x = Exp.mz,
          y = Exp.intensity - Exp.intensity,
          xend = Exp.mz,
          yend = Exp.intensity
        ),
        colour = col2
      ) +
      geom_point(
        data = matched.spec[matched.idx, , drop = FALSE],
        mapping = aes(x = Exp.mz, y = Exp.intensity),
        colour = col2
      ) +
      xlim(range.mz[1], range.mz[2]) +
      labs(x = xlab, y = ylab) +
      scale_y_continuous(
        limits = c(-1, 1),
        breaks = c(-1, -0.5, 0, 0.5, 1),
        labels = c("1", "0.5", "0", "0.5", "1")
      ) +
      theme_bw() +
      theme(
        # axis.line = element_line(arrow = arrow()),
        plot.title = element_text(
          color = "black",
          size = title.size,
          face = "plain",
          hjust = 0.5
        ),
        axis.title = element_text(
          color = "black",
          size = lab.size,
          face = "plain"
        ),
        axis.text = element_text(
          color = "black",
          size = axis.text.size,
          face = "plain"
        ),
        legend.title = element_text(
          color = "black",
          size = legend.title.size,
          face = "plain"
        ),
        legend.text = element_text(
          color = "black",
          size = legend.text.size,
          face = "plain"
        )
      )

    plot <- plot +
      annotate(
        geom = "text",
        x = Inf,
        y = Inf,
        label = spectrum1_name,
        color = col2,
        hjust = 1,
        vjust = 1
      ) +
      annotate(
        geom = "text",
        x = Inf,
        y = -Inf,
        label = spectrum2_name,
        color = col1,
        hjust = 1,
        vjust = -1
      )

    plot <- plot +
      geom_segment(
        data = matched.spec,
        mapping = aes(
          x = Lib.mz,
          y = Lib.intensity - Lib.intensity,
          xend = Lib.mz,
          yend = -Lib.intensity
        ),
        colour = col1
      ) +
      geom_point(
        data = matched.spec[matched.idx, , drop = FALSE],
        mapping = aes(x = Lib.mz, y = -Lib.intensity),
        colour = col1
      )
    if (interactive_plot) {
      plot <- plotly::ggplotly(plot)
    }
    if(show_mz) {
      plot = plot +
        geom_text_repel(
          data = matched.spec[matched.idx, , drop = FALSE],
          mapping = aes(x = Exp.mz,y = Exp.intensity,label = abs(round(Exp.mz,5))),
          colour = col2
        )+
        geom_text_repel(
          data = matched.spec[matched.idx, , drop = FALSE],
          mapping = aes(x = Lib.mz,y = -Lib.intensity,label = abs(round(Lib.mz,5))),
          colour = col1
        )
    }
    return(plot)
  }

  if (!missing(spectrum1) & missing(spectrum2)) {
    spectrum <- spectrum1
  }

  if (!missing(spectrum2) & missing(spectrum1)) {
    spectrum <- spectrum2
  }

  if (missing(range.mz)) {
    range.mz <- c(min(spectrum[, 1]), max(spectrum[, 1]))
  }

  plot <- ggplot(spectrum) +
    geom_segment(
      mapping = aes(
        x = mz,
        y = 0,
        xend = mz,
        yend = intensity
      ),
      colour = col1
    ) +
    xlim(range.mz[1], range.mz[2]) +
    labs(x = xlab, y = ylab) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    theme_bw() +
    theme(
      # axis.line = element_line(arrow = arrow()),
      plot.title = element_text(
        color = "black",
        size = title.size,
        face = "plain",
        hjust = 0.5
      ),
      axis.title = element_text(
        color = "black",
        size = lab.size,
        face = "plain"
      ),
      axis.text = element_text(
        color = "black",
        size = axis.text.size,
        face = "plain"
      ),
      legend.title = element_text(
        color = "black",
        size = legend.title.size,
        face = "plain"
      ),
      legend.text = element_text(
        color = "black",
        size = legend.text.size,
        face = "plain"
      )
    )
  if (interactive_plot) {
    plot <- plotly::ggplotly(plot)
  }
  plot
}
