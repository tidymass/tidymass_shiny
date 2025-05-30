% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_overview.R
\name{data_overview_server}
\alias{data_overview_server}
\title{import from tbl data of server
The application server-side}
\usage{
data_overview_server(id, volumes, prj_init, data_import_rv)
}
\arguments{
\item{id}{module of server}

\item{volumes}{shinyFiles volumes}

\item{prj_init}{use project init variables.}

\item{data_import_rv}{reactivevalues mass_dataset export
Reactive observer to dynamically update selectInput choices based on
available data sources. Prioritizes user-uploaded raw data over initialized
project data.
Ensures default selections exist in available choices}

\item{input, output, session}{Internal parameters for {shiny}.
DO NOT REMOVE.}

\item{preferred}{Preferred default value}

\item{available_choices}{Character vector of available options}
}
\description{
import from tbl data of server
The application server-side
}
