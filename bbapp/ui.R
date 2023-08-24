ui <- shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        actionButton("load", "load", width = "100%"),
        actionButton("save", "save", width = "100%"),
        actionButton("delete", "delete", width = "100%")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("shot", rHandsontableOutput("shot")),
          tabPanel("point", rHandsontableOutput("point")),
          tabPanel("penalty", rHandsontableOutput("penalty")),
          tabPanel("note")
        )
      )
    )
  )
)
