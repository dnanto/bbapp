server <- shinyServer(function(input, output) {
  n <- eventReactive(input$point.add, {
    total <- (
      names(input) %>%
        enframe(name = NULL, value = "label") %>%
        filter(startsWith(label, "point-")) %>%
        separate(label, c("table", "idx", "field", "row"), remove = F) %>%
        distinct(row) %>%
        nrow()
    )
    total + 1
  })
  output$point.input <- renderUI({
    fields <- df.ui$point$field
    lapply(1:n(), function(row)
      fluidRow(lapply(seq_along(fields), function(idx) column(2, textInput(paste("point", idx, fields[idx], row, sep = "-"), ""))))
    )
  })
})