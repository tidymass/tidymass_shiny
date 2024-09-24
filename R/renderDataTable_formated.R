#' Render a Formatted DataTable with Extended Features
#'
#' This function wraps `DT::renderDataTable` to provide additional functionality
#' including preset conditions, custom buttons for display control, and a CSV download
#' option. It allows for a highly customizable DataTable presentation with specific
#' interactive features tailored for enhanced user interaction.
#'
#' @param actions Initial actions or JavaScript to be executed.
#' @param condition1 First condition to check before rendering the table; if NULL, the table is not rendered.
#' @param condition2 Second condition to check; similar to `condition1`.
#' @param condition3 Third condition to check; similar to `condition1`.
#' @param condition4 Fourth condition to check; similar to `condition1`.
#' @param filename.a The base filename for the CSV download button.
#' @param tbl The data object to be rendered as a DataTable.
#'
#' @details
#' The function integrates several customization options including custom buttons that
#' allow users to toggle the display length of the table and to download the full table data.
#' The `actions` parameter can be used to inject specific JavaScript actions directly into
#' the DataTable initialization.
#'
#' @importFrom DT renderDataTable
#' @importFrom DT datatable
#' @importFrom DT JS
#' @return A DataTable object that is rendered within Shiny applications.
#'

renderDataTable_formated = function(actions = NA,condition1 = NA,condition2 = NA,condition3 = NA,condition4 = NA,filename.a = "full_page",tbl) {
  DT::renderDataTable(
    DT::datatable(
      {
        actions
        if(is.null(condition1)){return()}
        if(is.null(condition2)){return()}
        if(is.null(condition3)){return()}
        if(is.null(condition4)){return()}
        tbl
      },
      rownames= FALSE,
      extensions = 'Buttons',selection = 'single',
      options = list(
        autoWidth = F,
        dom = 'Bfrtip',
        scrollX = T,
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15,
        scrollY = "400px",
        buttons = list(
          list(
            extend = "collection",
            text = 'Show All',
            action = DT::JS("function ( e, dt, node, config ) {
                                    dt.page.len(-1);
                                    dt.ajax.reload();
                                }")),
          list(
            extend = "collection",
            text = 'Show Less',
            action = DT::JS("function ( e, dt, node, config ) {
                              dt.page.len(15);
                              dt.ajax.reload();}")
          ),
          list(extend = "csv", text = "Download Full Results", filename = filename.a,
               exportOptions = list(
                 modifier = list(page = "all")
               ))

        )
      )
    )
  )# l1
}
