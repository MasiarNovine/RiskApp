# Connection
# -------------------------------------------------
server <- function(input, output, session) {
  output$tbl <- DT::renderDataTable({
    d <- load_data(f = input$d1)
    DT::datatable(d, options = list(pageLength = 10, lengthMenu = c(10, 25, 50, 100), scrollX = TRUE))
  })
  output$network <- networkD3::renderSimpleNetwork({
    g <- create_network_3d(load_data(f = input$d1), width = NULL, height = NULL, zoom = TRUE)
    g
  })
  output$form <- renderText({
    htmltools::code(input$mf)
  })
  output$vars <- rhandsontable::renderRHandsontable({
    d <- load_data(f = input$d1)
    create_rhandsontable(d)
  })
  # output$vartbl <- DT::renderDataTable({
  #   d <- load_data(f = input$d1)
  #   DT::datatable(
  #     create_variable_table(d),
  #     options = list(searchable = FALSE, pageLength = 10, lengthMenu = c(5, 10), scrollX = TRUE)
  #   )
  # })
  # The option 'force' can be used for local testing w/o a Shiny server, for 'TRUE'
  #session$allowReconnect(TRUE)
}