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
      theme = bs_theme(bootswatch = "cerulean"),
      title = "TidyMass",
      project_init_ui("project_init_id"),
      nav_menu(
        title = 'Data import', icon = bs_icon("upload"),
        data_import_raw_ui("data_import_raw_id"),
        data_import_tbl_ui("data_import_tbl_id"),
        data_import_massdataset_ui("data_import_massdataset_id")
      ),
      data_overview_ui("data_overview_id"),
      nav_menu(
        title = 'Data Cleaning', icon = bs_icon("wind"),
        remove_noise_ui("remove_noise_features_id"),
        remove_outlier_ui("remove_outlier_id"),
        mv_impute_ui("mv_impute_id"),
        data_normalize_ui("data_normalize_id")
      ),
      nav_menu(
        title = 'Annotation', icon = bs_icon('person-vcard'),
        feature_annotation_ui("feature_annotation_id"),
        annotation_filter_ui("annotation_filter_id"),
        annotation_origin_ui("annotation_origin_id")
      ),
      nav_menu(
        title = 'Statistical analysis', icon = bs_icon('person-vcard'),
        dam_ui("dam_id"),
        enrichment_ui("enrichment_id")
      ),
      nav_menu(
        title = 'Tidymass shiny toolkits', icon = bs_icon('person-vcard'),
        fpa_ui("Feature-based_Pathway_Analysis_id"),
        database_ui("database_id"),
        kegg_pathway_ui("kegg_pathway_id"),
        id_convert_ui("id_convert_id")
      ),
      nav_item(proxy_module_ui("proxy"))
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
  # use_external_css_file("style.css")

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "TidyMass"
    ),

    tags$style(HTML("
      .proxy-control {
        display: flex;
        align-items: center;
        gap: 8px;
        padding: 5px 10px;
      }
      .proxy-status {
        padding: 10px;
        border-radius: 5px;
        margin: 10px 0;
      }
      .proxy-success {
        background-color: #d4edda;
        color: #155724;
        border: 1px solid #c3e6cb;
      }
      .proxy-failure {
        background-color: #f8d7da;
        color: #721c24;
        border: 1px solid #f5c6cb;
      }
    "))
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()

  )
}
