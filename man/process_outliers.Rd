% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{process_outliers}
\alias{process_outliers}
\title{Process Outliers in Mass Spectrometry Data}
\usage{
process_outliers(
  object,
  mv_method = c("By tidymass", "By myself"),
  by_witch = NULL,
  outlier_samples = NULL,
  outlier_table = NULL
)
}
\arguments{
\item{object}{A mass_dataset object}

\item{mv_method}{Detection method ("By tidymass" or "By myself")}

\item{by_witch}{Parameters for automated detection (regex patterns)}

\item{outlier_samples}{Manually specified outlier samples}

\item{outlier_table}{Precomputed outlier table (for "By tidymass" method)}
}
\value{
A list containing:
- $object: Filtered mass_dataset object
- $outlier_ids: Identified outlier sample IDs
- $message: Processing status messages
}
\description{
Process Outliers in Mass Spectrometry Data
}
