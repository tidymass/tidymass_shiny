#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_navbar(
      theme = bs_theme(bootswatch = "lumen"),
      title = "Tidymass",
#      homepage_ui("home_id"),
      project_init_ui("project_init_id"),
      nav_menu(
        title = 'Data import',icon = bs_icon("upload"),
        data_import_raw_ui("data_import_raw_id"),
        data_import_tbl_ui("data_import_tbl_id"),
        data_import_massdataset_ui("data_import_massdataset_id")
      ),
      nav_menu(
        title = 'Data Cleaning',icon = bs_icon("filter"),
        data_overview_ui("data_overview_id"),
      )

    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Tidymass"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
