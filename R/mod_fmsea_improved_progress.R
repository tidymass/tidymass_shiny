#' 优化后的fMSEA模块 - 改进的进度显示系统
#' @import shiny
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @importFrom shinyFiles shinyFilesButton parseFilePaths
#' @noRd

# --- 进度状态指示器UI组件 ---
progress_indicator_ui <- function(ns, step_id, title, description) {
  div(
    id = ns(paste0("progress_", step_id)),
    class = "progress-step-container",
    style = "margin: 10px 0; padding: 10px; border: 1px solid #e9ecef; border-radius: 8px; background: #f8f9fa;",

    div(
      style = "display: flex; align-items: center; margin-bottom: 8px;",

      # 状态图标
      div(
        id = ns(paste0("status_icon_", step_id)),
        style = "width: 24px; height: 24px; margin-right: 10px; display: flex; align-items: center; justify-content: center;",
        icon("circle", class = "text-muted") # 默认状态
      ),

      # 步骤标题
      div(
        style = "flex: 1;",
        h6(title, style = "margin: 0; font-weight: 600;"),
        p(description, style = "margin: 0; font-size: 0.85em; color: #666;")
      ),

      # 时间显示
      div(
        id = ns(paste0("time_display_", step_id)),
        style = "font-size: 0.8em; color: #888; text-align: right;",
        ""
      )
    ),

    # 详细进度条
    div(
      id = ns(paste0("progress_bar_", step_id)),
      style = "display: none;",
      div(
        class = "progress",
        style = "height: 8px; margin-bottom: 5px;",
        div(
          class = "progress-bar progress-bar-striped progress-bar-animated bg-primary",
          style = "width: 0%;"
        )
      ),
      div(
        id = ns(paste0("progress_detail_", step_id)),
        style = "font-size: 0.8em; color: #666;",
        ""
      )
    ),

    # 错误信息显示
    div(
      id = ns(paste0("error_msg_", step_id)),
      style = "display: none; margin-top: 8px;",
      class = "alert alert-danger",
      style = "padding: 8px; margin: 5px 0; font-size: 0.85em;",
      ""
    )
  )
}

# --- 增强的状态更新JavaScript ---
progress_js <- function(ns) {
  tags$script(HTML(sprintf('
    // 进度状态更新函数
    window.updateProgressStatus = function(stepId, status, progress, detail, timeInfo, errorMsg) {
      var iconId = "%s" + stepId;
      var progressBarId = "%s" + stepId;
      var progressDetailId = "%s" + stepId;
      var timeDisplayId = "%s" + stepId;
      var errorMsgId = "%s" + stepId;
      var containerID = "%s" + stepId;

      var iconElement = document.getElementById(iconId);
      var progressBarContainer = document.getElementById(progressBarId);
      var progressDetailElement = document.getElementById(progressDetailId);
      var timeDisplayElement = document.getElementById(timeDisplayId);
      var errorMsgElement = document.getElementById(errorMsgId);
      var containerElement = document.getElementById(containerID);

      // 更新状态图标
      if (iconElement) {
        if (status === "waiting") {
          iconElement.innerHTML = \'<i class="fas fa-circle text-muted"></i>\';
          containerElement.style.background = "#f8f9fa";
          containerElement.style.borderColor = "#e9ecef";
        } else if (status === "running") {
          iconElement.innerHTML = \'<i class="fas fa-spinner fa-spin text-primary"></i>\';
          containerElement.style.background = "#e3f2fd";
          containerElement.style.borderColor = "#2196f3";
        } else if (status === "completed") {
          iconElement.innerHTML = \'<i class="fas fa-check-circle text-success"></i>\';
          containerElement.style.background = "#e8f5e8";
          containerElement.style.borderColor = "#28a745";
        } else if (status === "error") {
          iconElement.innerHTML = \'<i class="fas fa-exclamation-circle text-danger"></i>\';
          containerElement.style.background = "#ffeaea";
          containerElement.style.borderColor = "#dc3545";
        }
      }

      // 更新进度条
      if (progressBarContainer) {
        if (status === "running" && progress >= 0) {
          progressBarContainer.style.display = "block";
          var progressBar = progressBarContainer.querySelector(".progress-bar");
          if (progressBar) {
            progressBar.style.width = progress + "%%";
          }
          if (progressDetailElement && detail) {
            progressDetailElement.textContent = detail;
          }
        } else {
          progressBarContainer.style.display = "none";
        }
      }

      // 更新时间信息
      if (timeDisplayElement && timeInfo) {
        timeDisplayElement.innerHTML = timeInfo;
      }

      // 更新错误信息
      if (errorMsgElement) {
        if (status === "error" && errorMsg) {
          errorMsgElement.style.display = "block";
          errorMsgElement.textContent = errorMsg;
        } else {
          errorMsgElement.style.display = "none";
        }
      }
    };

    // 格式化时间函数
    window.formatElapsedTime = function(startTime) {
      var elapsed = (Date.now() - startTime) / 1000;
      var minutes = Math.floor(elapsed / 60);
      var seconds = Math.floor(elapsed %% 60);
      return minutes > 0 ? minutes + "m " + seconds + "s" : seconds + "s";
    };
  ', ns("status_icon_"), ns("progress_bar_"), ns("progress_detail_"),
     ns("time_display_"), ns("error_msg_"), ns("progress_"))))
}

# --- 改进的UI ---
fmsea_ui_improved <- function(id) {
  ns <- NS(id)

  tagList(
    # 添加自定义CSS
    tags$head(
      tags$style(HTML("
        .progress-step-container {
          transition: all 0.3s ease;
        }
        .progress-step-container:hover {
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        }
        .cancel-btn {
          position: absolute;
          top: 5px;
          right: 5px;
          padding: 2px 6px;
          font-size: 0.7em;
        }
      "))
    ),

    # 进度控制JavaScript
    progress_js(ns),

    nav_panel(
      title = 'fMSEA Analysis',
      icon = bsicons::bs_icon("diagram-3"),
      layout_sidebar(
        sidebar = accordion(
          open = c("Data Upload", "Step 1: Annotation", "Step 2: Analysis"),

          # --- 1. Data Upload ---
          accordion_panel(
            title = "Data Upload",
            icon = bsicons::bs_icon("upload"),

            shinyFiles::shinyFilesButton(id = ns('feature_table_file'),
                                         label = 'Feature Table (.rda)',
                                         title = "Select Feature Table",
                                         multiple = FALSE,
                                         buttonType = "default"),
            div(textOutput(ns("feature_table_path")),
                style = "font-size: 0.75em; color: #666; margin: 5px 0; padding: 5px; background-color: #f8f9fa; border-radius: 3px;"),

            shinyFiles::shinyFilesButton(id = ns('ms1_db_file'),
                                         label = 'MS1 Database (.rda)',
                                         title = "Select MS1 Database",
                                         multiple = FALSE,
                                         buttonType = "default"),
            div(textOutput(ns("ms1_db_path")),
                style = "font-size: 0.75em; color: #666; margin: 5px 0; padding: 5px; background-color: #f8f9fa; border-radius: 3px;"),

            shinyFiles::shinyFilesButton(id = ns('pathway_db_file'),
                                         label = 'Pathway Database (.rda)',
                                         title = "Select Pathway Database",
                                         multiple = FALSE,
                                         buttonType = "default"),
            div(textOutput(ns("pathway_db_path")),
                style = "font-size: 0.75em; color: #666; margin: 5px 0; padding: 5px; background-color: #f8f9fa; border-radius: 3px;"),

            hr(),

            p("Load Existing Results:", style = "font-size: 0.85em; font-weight: bold; margin-bottom: 5px;"),
            shinyFiles::shinyFilesButton(id = ns('results_rda_file'),
                                         label = 'Load results.rda',
                                         title = "Select Previous Results",
                                         multiple = FALSE,
                                         buttonType = "info"),
            div(textOutput(ns("results_rda_path")),
                style = "font-size: 0.75em; color: #666; margin: 5px 0; padding: 5px; background-color: #f8f9fa; border-radius: 3px;")
          ),

          # --- 2. Step 1 Parameters ---
          accordion_panel(
            title = "Step 1: Annotation",
            icon = bsicons::bs_icon("1-circle"),

            # 参数分组
            div(
              class = "card",
              style = "margin-bottom: 15px;",
              div(
                class = "card-header",
                style = "padding: 8px 12px; background: #f1f3f4;",
                h6("Matching Parameters", style = "margin: 0; font-size: 0.9em;")
              ),
              div(
                class = "card-body",
                style = "padding: 12px;",

                fluidRow(
                  column(6, selectInput(ns("column"), "Column Type",
                                        choices = c("rp", "hilic"),
                                        selected = "rp")),
                  column(6, selectInput(ns("database_type"), "Database",
                                        choices = c("KEGG", "HMDB"),
                                        selected = "KEGG"))
                ),

                fluidRow(
                  column(6,
                    numericInput(ns("ms1_match_ppm"), "MS1 PPM",
                                 value = 15, min = 1, max = 50, step = 0.1),
                    helpText("Mass tolerance (1-50 ppm)", style = "font-size: 0.75em; margin-top: -8px;")
                  ),
                  column(6,
                    numericInput(ns("mfc_rt_tol"), "RT Tolerance (s)",
                                 value = 10, min = 1, max = 300, step = 1),
                    helpText("Retention time tolerance", style = "font-size: 0.75em; margin-top: -8px;")
                  )
                ),

                numericInput(ns("isotope_number"), "Isotope Number",
                             value = 3, min = 1, max = 10, step = 1),
                helpText("Number of isotopes to consider", style = "font-size: 0.75em; margin-top: -8px;")
              )
            ),

            # 运行按钮和取消按钮
            div(
              style = "position: relative;",
              actionButton(ns("run_step1"), "Run Step 1: Annotation",
                           class = "btn-primary",
                           width = "100%"),

              # 取消按钮（初始隐藏）
              actionButton(ns("cancel_step1"), "Cancel",
                           class = "btn btn-outline-danger cancel-btn",
                           style = "display: none;")
            ),

            # 进度状态显示
            div(style = "margin-top: 15px;",
                progress_indicator_ui(ns, "step1", "Feature Annotation",
                                      "Match features with metabolite database"))
          ),

          # --- 3. Step 2 Parameters ---
          accordion_panel(
            title = "Step 2: fMSEA Analysis",
            icon = bsicons::bs_icon("2-circle"),

            # 分析参数分组
            div(
              class = "card",
              style = "margin-bottom: 15px;",
              div(
                class = "card-header",
                style = "padding: 8px 12px; background: #f1f3f4;",
                h6("Analysis Parameters", style = "margin: 0; font-size: 0.9em;")
              ),
              div(
                class = "card-body",
                style = "padding: 12px;",

                fluidRow(
                  column(6,
                    numericInput(ns("threads"), "Threads",
                                 value = 3, min = 1, max = 16, step = 1),
                    helpText("CPU cores to use", style = "font-size: 0.75em; margin-top: -8px;")
                  ),
                  column(6,
                    numericInput(ns("perm_num"), "Permutations",
                                 value = 1000, min = 100, max = 10000, step = 100),
                    helpText("Number of permutations", style = "font-size: 0.75em; margin-top: -8px;")
                  )
                ),

                fluidRow(
                  column(6,
                    numericInput(ns("min_compounds"), "Min Compounds",
                                 value = 15, min = 5, max = 100, step = 1),
                    helpText("Minimum pathway size", style = "font-size: 0.75em; margin-top: -8px;")
                  ),
                  column(6,
                    numericInput(ns("max_compounds"), "Max Compounds",
                                 value = 300, min = 50, max = 1000, step = 10),
                    helpText("Maximum pathway size", style = "font-size: 0.75em; margin-top: -8px;")
                  )
                ),

                fluidRow(
                  column(6,
                    numericInput(ns("max_iter_num"), "Max Iterations",
                                 value = 1, min = 1, max = 20, step = 1),
                    helpText("Maximum iterations", style = "font-size: 0.75em; margin-top: -8px;")
                  ),
                  column(6,
                    numericInput(ns("fdr_thr"), "FDR Threshold",
                                 value = 0.05, min = 0.001, max = 0.5, step = 0.001),
                    helpText("False discovery rate", style = "font-size: 0.75em; margin-top: -8px;")
                  )
                )
              )
            ),

            # 运行按钮
            div(
              style = "position: relative;",
              actionButton(ns("run_step2"), "Run Step 2: fMSEA Analysis",
                           class = "btn-success",
                           width = "100%"),

              # 取消按钮（初始隐藏）
              actionButton(ns("cancel_step2"), "Cancel",
                           class = "btn btn-outline-danger cancel-btn",
                           style = "display: none;")
            ),

            # 进度状态显示
            div(style = "margin-top: 15px;",
                progress_indicator_ui(ns, "step2", "Enrichment Analysis",
                                      "Perform feature-based metabolite set enrichment analysis"))
          )
        ),

        # 主内容区域
        card(
          full_screen = TRUE,
          card_header(
            div(class = "d-flex justify-content-between align-items-center",
                "Analysis Results",
                div(
                  downloadButton(ns("download_table"), "CSV", class = "btn-sm me-2"),
                  downloadButton(ns("download_excel"), "Excel", class = "btn-sm")
                ))
          ),
          card_body(
            padding = 0,

            # 结果概览
            div(
              id = ns("results_overview"),
              style = "padding: 15px; border-bottom: 1px solid #eee; background: #f8f9fa; display: none;",
              div(
                style = "display: flex; gap: 20px; align-items: center;",
                div(
                  style = "text-align: center;",
                  h4(textOutput(ns("total_pathways"), inline = TRUE), style = "margin: 0; color: #007bff;"),
                  p("Total Pathways", style = "margin: 0; font-size: 0.85em; color: #666;")
                ),
                div(
                  style = "text-align: center;",
                  h4(textOutput(ns("significant_pathways"), inline = TRUE), style = "margin: 0; color: #28a745;"),
                  p("Significant", style = "margin: 0; font-size: 0.85em; color: #666;")
                ),
                div(
                  style = "text-align: center;",
                  h4(textOutput(ns("analysis_time"), inline = TRUE), style = "margin: 0; color: #6c757d;"),
                  p("Analysis Time", style = "margin: 0; font-size: 0.85em; color: #666;")
                )
              )
            ),

            # 结果表格
            div(
              style = "padding: 15px; border-bottom: 1px solid #eee;",
              h6("Significant Pathways (Select to visualize)"),
              DT::dataTableOutput(ns("sig_modules_table"))
            ),

            # 图表展示
            div(
              style = "padding: 15px;",
              h6("Enrichment Plot"),
              plotOutput(ns("fmsea_plot"), height = "450px"),
              div(
                style = "display: flex; gap: 10px; margin-top: 10px; justify-content: space-between;",
                div(
                  downloadButton(ns("download_png"), "PNG", class = "btn-sm"),
                  downloadButton(ns("download_pdf"), "PDF", class = "btn-sm"),
                  downloadButton(ns("download_svg"), "SVG", class = "btn-sm")
                ),
                div(
                  checkboxInput(ns("interactive_plot"), "Interactive Plot", value = FALSE),
                  style = "margin-top: 5px;"
                )
              )
            ),

            # 详细信息
            accordion(
              open = FALSE,
              accordion_panel(
                "Detailed Analysis Summary",
                verbatimTextOutput(ns("result_summary"))
              )
            )
          )
        )
      )
    )
  )
}