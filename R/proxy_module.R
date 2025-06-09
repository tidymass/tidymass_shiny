#' Proxy Module UI
#'
#' @param id module id
#' @export
#'
#' @importFrom shiny NS tagList div
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets switchInput
#' @noRd
proxy_module_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),

    div(
      class = "proxy-control",
      shinyWidgets::switchInput(
        inputId = ns("proxy_switch"),
        label = "Proxy",
        value = FALSE,
        size = "mini",
        width = "150px",

        labelWidth = "50px",
        handleWidth = "30px"
      )
    )
  )
}

#' Proxy Module Server
#'
#' @param id 模块命名空间 ID
#' @export
#'
#' @importFrom shiny moduleServer NS observeEvent textInput icon actionButton reactiveValues
#' @importFrom shinyalert shinyalert
#' @importFrom shinyWidgets updateSwitchInput
#' @importFrom httr GET status_code timeout
#' @noRd
proxy_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 代理状态响应式值
    proxy_status <- reactiveValues(
      http = "",
      https = "",
      socks5 = "",
      active = FALSE,
      test_result = "Proxy not configured",
      # 添加初始化标志，防止启动时显示通知
      initialized = FALSE
    )

    # 添加一个标志来跟踪是否是第一次打开开关
    first_time_open <- reactiveVal(TRUE)

    # 观察代理开关状态变化
    observeEvent(input$proxy_switch, {
      if (input$proxy_switch) {
        # 当开关打开时，弹出代理设置窗口
        show_proxy_settings_modal()
        first_time_open(FALSE)  # 标记为已打开过
      } else {
        # 当开关关闭时，清除代理设置
        clear_proxy_settings()
        proxy_status$active <- FALSE
        proxy_status$test_result <- "Proxy disabled"

        # 仅在用户操作后显示通知，而不是在初始化时
        if (proxy_status$initialized) {
          shinyalert::shinyalert(
            title = "Proxy Disabled",
            text = "All proxy settings have been cleared.",
            type = "success"
          )
        }
      }

      # 标记为已初始化
      proxy_status$initialized <- TRUE
    }, ignoreInit = TRUE)  # 忽略初始化时的触发

    # 显示代理设置模态框
    show_proxy_settings_modal <- function() {
      ns <- session$ns

      # 如果是第一次打开，显示默认值，否则显示上次设置的值
      if (first_time_open()) {
        initial_values <- list(
          http = "127.0.0.1:7897",
          https = "127.0.0.1:7897",
          socks5 = "127.0.0.1:7897"
        )
      } else {
        initial_values <- list(
          http = proxy_status$http,
          https = proxy_status$https,
          socks5 = proxy_status$socks5
        )
      }

      shinyalert::shinyalert(
        html = TRUE,
        title = "Proxy Configuration",
        text = tagList(
          textInput(
            ns("http_proxy_input"),
            "HTTP Proxy (ip:port):",
            value = initial_values$http
          ),
          textInput(
            ns("https_proxy_input"),
            "HTTPS Proxy (ip:port):",
            value = initial_values$https
          ),
          textInput(
            ns("socks5_proxy_input"),
            "SOCKS5 Proxy (ip:port):",
            value = initial_values$socks5
          )
        ),
        size = "m",
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        showCancelButton = TRUE,
        showConfirmButton = TRUE,
        confirmButtonText = "Save & Test",
        cancelButtonText = "Cancel",
        callbackR = function(value) {
          if (value) {
            # 用户点击了保存按钮
            save_and_test_proxy()
          } else {
            # 用户点击了取消按钮，重置开关状态
            shinyWidgets::updateSwitchInput(session, "proxy_switch", value = FALSE)
          }
        }
      )
    }

    # 保存并测试代理设置
    save_and_test_proxy <- function() {
      # 保存代理设置
      proxy_status$http <- input$http_proxy_input
      proxy_status$https <- input$https_proxy_input
      proxy_status$socks5 <- input$socks5_proxy_input

      # 设置环境变量
      set_proxy_env_vars()

      # 测试代理连接
      test_result <- test_proxy_connection()

      # 更新代理状态
      proxy_status$active <- test_result$success
      proxy_status$test_result <- test_result$message

      # 显示测试结果
      if (test_result$success) {
        shinyalert::shinyalert(
          title = "Proxy Test Successful",
          text = test_result$message,
          type = "success"
        )
      } else {
        # 如果测试失败，重置开关状态并清除代理设置
        shinyWidgets::updateSwitchInput(session, "proxy_switch", value = FALSE)
        clear_proxy_settings()

        shinyalert::shinyalert(
          title = "Proxy Test Failed",
          text = paste(
            test_result$message,
            "<br><br><b>Warning:</b> Some modules requiring internet access (e.g., ID conversion and KEGG pathway construction) may not function properly."
          ),
          type = "error",
          html = TRUE
        )
      }
    }

    # 设置环境变量
    set_proxy_env_vars <- function() {
      # 清除现有设置
      clear_proxy_settings()

      # 设置 HTTP 代理
      if (!is.null(proxy_status$http) && proxy_status$http != "") {
        parts <- strsplit(proxy_status$http, ":")[[1]]
        if (length(parts) == 2) {
          Sys.setenv(
            http_proxy = paste0("http://", proxy_status$http),
            HTTP_PROXY = paste0("http://", proxy_status$http)
          )
        }
      }

      # 设置 HTTPS 代理
      if (!is.null(proxy_status$https) && proxy_status$https != "") {
        parts <- strsplit(proxy_status$https, ":")[[1]]
        if (length(parts) == 2) {
          Sys.setenv(
            https_proxy = paste0("http://", proxy_status$https),
            HTTPS_PROXY = paste0("http://", proxy_status$https)
          )
        }
      }

      # 设置 SOCKS5 代理
      if (!is.null(proxy_status$socks5) && proxy_status$socks5 != "") {
        parts <- strsplit(proxy_status$socks5, ":")[[1]]
        if (length(parts) == 2) {
          Sys.setenv(
            all_proxy = paste0("socks5://", proxy_status$socks5),
            ALL_PROXY = paste0("socks5://", proxy_status$socks5)
          )
        }
      }
    }

    # 清除代理设置
    clear_proxy_settings <- function() {
      # 清除环境变量
      Sys.unsetenv("http_proxy")
      Sys.unsetenv("HTTP_PROXY")
      Sys.unsetenv("https_proxy")
      Sys.unsetenv("HTTPS_PROXY")
      Sys.unsetenv("all_proxy")
      Sys.unsetenv("ALL_PROXY")
    }

    # 测试代理连接
    test_proxy_connection <- function() {
      test_urls <- c(
        "https://rest.kegg.jp/list/pathway/hsa",  # KEGG API
        "https://www.uniprot.org/uniprot/P12345"  # UniProt API
      )

      success <- FALSE
      messages <- character()

      for (test_url in test_urls) {
        tryCatch({
          # 使用 httr 测试连接
          response <- httr::GET(test_url,httr::timeout(10))

          if (httr::status_code(response) == 200) {
            success <- TRUE
            messages <- c(messages, paste("Successfully connected to:", test_url))
          } else {
            messages <- c(messages, paste("Failed to connect to:", test_url, "Status code:", httr::status_code(response)))
          }}, error = function(e) {
            messages <- c(messages, paste("Error connecting to:", test_url, "Error:", e$message))
          })
      }
      if (success) {
        return(list(
          success = TRUE,
          message = paste("Proxy connection successful!<br>", paste(messages, collapse = "<br>"))
        ))
      } else {
        return(list(
          success = FALSE,
          message = paste("Proxy connection failed!<br>", paste(messages, collapse = "<br>"))
        ))
      }
    }

    # 返回代理状态给外部使用
    reactive({
      list(
        active = proxy_status$active,
        http = proxy_status$http,
        https = proxy_status$https,
        socks5 = proxy_status$socks5,
        status = proxy_status$test_result
      )
    })
  })
}

