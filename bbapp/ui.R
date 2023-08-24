ui <- shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        actionButton("point.add", "point.add", width = "100%")
      ),
      mainPanel(
        uiOutput("point.input")
      )
    )
  )
)
