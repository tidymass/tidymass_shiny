#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_themer
#' @noRd
app_server <-
  function(input, output, session) {

    # Your application server logic
    bslib::bs_themer()
    # Call module server functions
    # Determine volumes based on system type

    # Check run in docker
    in_docker <- function() {
      # 方法1: 检查.dockerenv标志文件
      if (file.exists("/.dockerenv")) return(TRUE)

      # 方法2: 检查cgroup信息
      if (file.exists("/proc/self/cgroup")) {
        cgroup <- readLines("/proc/self/cgroup", warn = FALSE)
        if (any(grepl("docker", cgroup))) return(TRUE)
        if (any(grepl("kubepods", cgroup))) return(TRUE)  # Kubernetes环境
        }
      return(FALSE)
      }

    if (in_docker()) {
      volumes <- shinyFiles::getVolumes()()
    } else if (Sys.info()["sysname"] == "Windows") {
      volumes <- get_volumes_win()
    } else if (Sys.info()["sysname"] == "Linux") {
      # 非 Docker 的 Linux 环境
      shiny_home <- Sys.getenv("HOME", unset = "/home/shiny")
      volumes <- c(shiny_home = shiny_home)
    } else if (Sys.info()["sysname"] == "Darwin") {
      user_home <- Sys.getenv("HOME")
      volumes <- c(home = user_home)
    } else {
      volumes <- shinyFiles::getVolumes()()
    }

    #> project init
    prj_init <- reactiveValues(data = NULL) # project init
    project_init_server(id = "project_init_id", volumes = volumes, prj_init)
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
      data_import_rv = data_import_rv
    )

    remove_noise_server(
      id = "remove_noise_features_id",
      volumes = volumes,
      prj_init = prj_init,
      data_import_rv = data_import_rv
    )

    remove_outlier_server(
      id = "remove_outlier_id",
      volumes = volumes,
      prj_init = prj_init,
      data_import_rv = data_import_rv,
      data_clean_rv = data_clean_rv
    )

    mv_impute_server(
      id = "mv_impute_id",
      volumes = volumes,
      prj_init = prj_init,
      data_clean_rv = data_clean_rv
    )

    data_normalize_server(
      id = "data_normalize_id",
      volumes = volumes,
      prj_init = prj_init,
      data_import_rv = data_import_rv,
      data_clean_rv = data_clean_rv
    )
    ## data_anno
    data_anno <- reactiveValues(data = NULL)
    feature_annotation_server(
      id = "feature_annotation_id",
      volumes = volumes,
      prj_init = prj_init,
      data_import_rv = data_import_rv,
      data_clean_rv = data_clean_rv,
      data_anno = data_anno
    )
    p2_af_filter <- reactiveValues(data = NULL)
    annotation_filter_server(
      id = "annotation_filter_id",
      volumes = volumes,
      prj_init = prj_init,
      data_import_rv = data_import_rv,
      data_clean_rv = data_clean_rv,
      p2_af_filter = p2_af_filter
    )

    annotation_origin_server(
      id = "annotation_origin_id",
      volumes = volumes,
      prj_init = prj_init,
      data_import_rv = data_import_rv,
      data_clean_rv = data_clean_rv)
    p3_dam <- reactiveValues(data = NULL)
    dam_server(
      id = "dam_id",
      volumes = volumes,
      prj_init = prj_init,
      data_import_rv = data_import_rv,
      data_clean_rv = data_clean_rv,
      p3_dam = p3_dam
    )
    data_enrich <- reactiveValues(data = NULL)
    enrichment_server(
      id = "enrichment_id",
      volumes = volumes,
      prj_init = prj_init,
      data_import_rv = data_import_rv,
      data_clean_rv = data_clean_rv,
      data_enrich = data_enrich
    )
    fpa_server(
      id = "Feature-based_Pathway_Analysis_id"
    )
    database_server(
      id = "database_id",volumes = volumes
    )
    kegg_pathway_server(
      id = "kegg_pathway_id",volumes = volumes
    )
    id_convert_server(
      id = "id_convert_id"
    )
    proxy_status <- proxy_module_server("proxy")

  }
