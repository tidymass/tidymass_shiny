#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_themer
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  bs_themer()
  # Call module server functions
  if(Sys.info()["sysname"] == "Windows") {
    volumes = getVolumes_win()
  } else {
    volumes = shinyFiles::getVolumes()()
  }
  #> project init
  prj_init <- reactiveValues(data = NULL) # project init
  project_init_server(id = "project_init_id",volumes = volumes,prj_init)
  #> data import
  data_import_rv <- reactiveValues(data = NULL)
  data_import_raw_server(
    id = "data_import_raw_id",
    volumes = volumes,
    prj_init = prj_init,
    data_import_rv = data_import_rv
  )
  ##> from peak picking table
  data_import_tbl_server(
    id = "data_import_tbl_id",
    volumes = volumes,
    prj_init = prj_init,
    data_import_rv = data_import_rv
  )

  ##> from peak picking table
  data_import_massdataset_server(
    id = "data_import_massdataset_id",
    volumes = volumes,
    prj_init = prj_init,
    data_import_rv = data_import_rv
  )

  #> Data clean
  data_clean_rv <- reactiveValues(data = NULL)
  data_overview_server(
    id = "data_overview_id",
    volumes = volumes,
    prj_init = prj_init,
    data_import_rv = data_import_rv,
    data_clean_rv = data_clean_rv
  )
}
