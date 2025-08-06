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
      # Navigate to project initialization
      # 修复：使用正确的session引用方式
      updateNavbarPage(
        session = session,
        inputId = "main_navbar",
        selected = "project_init"
      )
    })

    observeEvent(input$docs_btn, {
      # Open documentation in new tab
      browseURL("https://www.tidymass.org/tidymassshiny-tutorial/")
    })
  })
}
