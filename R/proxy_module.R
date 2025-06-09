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
#' @param id module id
#' @export
#'
#' @importFrom shiny moduleServer NS observeEvent textInput icon actionButton reactiveValues
#' @importFrom shinyalert shinyalert
#' @importFrom shinyWidgets updateSwitchInput
#' @importFrom httr GET status_code timeout
#' @noRd
proxy_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # reaction values
    proxy_status <- reactiveValues(
      http = "",
      https = "",
      socks5 = "",
      active = FALSE,
      test_result = "Proxy not configured",
      initialized = FALSE
    )

    # Add a flag to track whether the switch is turned on for the first time
    first_time_open <- reactiveVal(TRUE)

    # Observe the change in the state of the agent switch
    observeEvent(input$proxy_switch, {
      if (input$proxy_switch) {
        # When the switch is turned on, the proxy settings window pops up
        show_proxy_settings_modal()
        first_time_open(FALSE)
      } else {
        # Clear proxy settings when the switch is turned off
        clear_proxy_settings()
        proxy_status$active <- FALSE
        proxy_status$test_result <- "Proxy disabled"

        # Notifications are displayed only after user action, not at initialization
        if (proxy_status$initialized) {
          shinyalert::shinyalert(
            title = "Proxy Disabled",
            text = "All proxy settings have been cleared.",
            type = "success"
          )
        }
      }

      # Mark as initialized
      proxy_status$initialized <- TRUE
    }, ignoreInit = TRUE)

    # Show Proxy Settings Modal Box
    show_proxy_settings_modal <- function() {
      ns <- session$ns

      # If it is the first time, the default value will be displayed, otherwise the value set last time will be displayed.
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
            save_and_test_proxy()
          } else {
            shinyWidgets::updateSwitchInput(session, "proxy_switch", value = FALSE)
          }
        }
      )
    }

    # Save and test proxy settings
    save_and_test_proxy <- function() {
      # # Save proxy settings
      proxy_status$http <- input$http_proxy_input
      proxy_status$https <- input$https_proxy_input
      proxy_status$socks5 <- input$socks5_proxy_input

      # Set environment variables
      set_proxy_env_vars()

      # Test the proxy connection
      test_result <- test_proxy_connection()

      # Update proxy status
      proxy_status$active <- test_result$success
      proxy_status$test_result <- test_result$message

      # Display test results
      if (test_result$success) {
        shinyalert::shinyalert(
          title = "Proxy Test Successful",
          text = test_result$message,
          type = "success"
        )
      } else {
        # If the test fails, reset the switch status and clear the proxy settings
        shinyWidgets::updateSwitchInput(session, "proxy_switch", value = FALSE)
        clear_proxy_settings()

        shinyalert::shinyalert(
          title = "Proxy Test Failed",
          text = HTML(paste(
            test_result$message,
            "<br><br><b>Warning:</b> Some modules requiring internet access (e.g., ID conversion and KEGG pathway construction) may not function properly."
          )),
          type = "error",
          html = TRUE
        )

      }
    }

    # Set environment variable
    set_proxy_env_vars <- function() {
      clear_proxy_settings()

      # Set up HTTP proxy
      if (!is.null(proxy_status$http) && proxy_status$http != "") {
        parts <- strsplit(proxy_status$http, ":")[[1]]
        if (length(parts) == 2) {
          Sys.setenv(
            http_proxy = paste0("http://", proxy_status$http),
            HTTP_PROXY = paste0("http://", proxy_status$http)
          )
        }
      }

      # Set up HTTPS proxy
      if (!is.null(proxy_status$https) && proxy_status$https != "") {
        parts <- strsplit(proxy_status$https, ":")[[1]]
        if (length(parts) == 2) {
          Sys.setenv(
            https_proxy = paste0("http://", proxy_status$https),
            HTTPS_PROXY = paste0("http://", proxy_status$https)
          )
        }
      }

      # Set up SOCKS5 proxy
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

    # Clear proxy settings
    clear_proxy_settings <- function() {
      # Clear environmental variables
      Sys.unsetenv("http_proxy")
      Sys.unsetenv("HTTP_PROXY")
      Sys.unsetenv("https_proxy")
      Sys.unsetenv("HTTPS_PROXY")
      Sys.unsetenv("all_proxy")
      Sys.unsetenv("ALL_PROXY")
    }

    # Test the proxy connection
    test_proxy_connection <- function() {
      test_urls <- c(
        "https://rest.kegg.jp/list/pathway/hsa",  # KEGG API
        "https://www.uniprot.org/uniprot/P12345"  # UniProt API
      )

      success <- FALSE
      messages <- character()

      for (test_url in test_urls) {
        tryCatch({
          # Test the connection with httr
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

    # Return the proxy status for external use
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

