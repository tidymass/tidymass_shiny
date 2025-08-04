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
      id = "main_navbar",  # Added ID for navbar control
      theme = bs_theme(bootswatch = "cerulean"),
      title = "TidyMass",
      # Homepage tab - new addition
      nav_panel(
        title = "Home",
        value = "home",
        icon = bs_icon("house"),
        homepage_ui("homepage_id")
      ),

      # Existing tabs
      nav_panel(
        title = "Project Init",
        value = "project_init",
        icon = bs_icon("folder-plus"),
        project_init_ui("project_init_id")
      ),
      nav_menu(
        title = 'Data import', icon = bs_icon("upload"),
        data_import_raw_ui("data_import_raw_id"),
        data_import_tbl_ui("data_import_tbl_id"),
        data_import_massdataset_ui("data_import_massdataset_id")
      ),
      nav_panel(
        title = "Data Overview",
        value = "data_overview",
        icon = bs_icon("eye"),
        data_overview_ui("data_overview_id")
      ),
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
        title = 'Statistical analysis', icon = bs_icon('calculator'),
        dam_ui("dam_id"),
        enrichment_ui("enrichment_id")
      ),
      nav_menu(
        title = 'Tidymass toolkits', icon = bs_icon('tools'),
        fpa_ui("Feature-based_Pathway_Analysis_id"),
        database_ui("database_id"),
        kegg_pathway_ui("kegg_pathway_id"),
        id_convert_ui("id_convert_id")
      ),
      footer = flexible_download_widget_ui("download_widget")
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
    favicon(ext = "ico"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "TidyMass"
    ),

    # Add Font Awesome
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    ),

    tags$style(HTML("
      /* Homepage styles */
      .homepage-header {
        background: linear-gradient(135deg, #2c3e50, #1a2980);
        color: white;
        padding: 40px 0;
        text-align: center;
        margin-bottom: 30px;
      }
      .feature-card {
        transition: transform 0.3s, box-shadow 0.3s;
        height: 100%;
        margin-bottom: 20px;
        border-radius: 8px;
        overflow: hidden;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .feature-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 10px 20px rgba(0,0,0,0.15);
      }
      .feature-card .card-body {
        padding: 20px;
      }
      .citation-box {
        background-color: #f8f9fa;
        border-left: 4px solid #3498db;
        padding: 15px;
        margin: 20px 0;
        font-size: 0.9em;
        border-radius: 4px;
      }
      .img-container {
        text-align: center;
        margin: 20px 0;
      }
      .img-container img {
        max-width: 100%;
        border-radius: 8px;
        box-shadow: 0 5px 15px rgba(0,0,0,0.1);
      }
      .btn-action {
        background: linear-gradient(to right, #3498db, #2c3e50);
        color: white;
        border: none;
        padding: 10px 20px;
        font-weight: bold;
        border-radius: 30px;
        margin: 10px 5px;
        transition: all 0.3s ease;
      }
      .btn-action:hover {
        transform: scale(1.05);
        box-shadow: 0 5px 15px rgba(0,0,0,0.2);
      }
      .resource-link {
        display: block;
        padding: 10px;
        border-radius: 5px;
        background: #f8f9fa;
        margin: 5px 0;
        text-align: center;
        transition: all 0.3s ease;
      }
      .resource-link:hover {
        background: #e9ecef;
        text-decoration: none;
      }

    "))
  )
}
