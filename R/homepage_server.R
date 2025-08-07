#' Homepage Server Module
#'
#' Handles server-side logic for the homepage
#'
#' @param id module id
#'
#' @return module server function
#' @export
homepage_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Handle button clicks
    observeEvent(input$start_btn, {
      # Show informational alert instead of navigating
      shinyalert::shinyalert(
        title = "Get Started with TidyMass",
        text = HTML(
          '<div style="text-align: left;">
          <p><strong>Please follow these steps to begin your analysis:</strong></p>
          <ol>
            <li>Navigate to the <b>"Project Init"</b> tab in the navigation bar</li>
            <li>Set up your project directory and parameters</li>
            <li>Start your analysis workflow</li>
          </ol>
          <p><span style="color: #e74c3c; font-weight: bold;">Important Notice:</span></p>
          <ul>
            <li>All analysis results will be automatically deleted after <b>24 hours</b></li>
            <li>Please download your results promptly to avoid data loss</li>
          </ul>
          </div>'
        ),
        html = TRUE,
        type = "info",
        confirmButtonText = "OK, I Understand",
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE
      )
    })

    observeEvent(input$docs_btn, {
      # Open documentation in new tab
      browseURL("https://www.tidymass.org/tidymassshiny-tutorial/")
    })
  })
}
