ui <- shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        numericInput("year", "year", value = max(df.match$year), width = "100%"),
        selectInput("season", "season", choices = seasons, width = "100%"),
        numericInput("week", "week", value = 1, min = 1, width = "100%"),
        numericInput("game", "game", value = 1, min = 1, width = "100%"),
        actionButton("load", "load", width = "100%"),
        actionButton("save", "save", width = "100%"),
        actionButton("delete", "delete", width = "100%"),
        width = 2
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "match",
            tabsetPanel(
              tabPanel("point", rHandsontableOutput("point")),
              tabPanel("penalty", rHandsontableOutput("penalty")),
              tabPanel("shot", rHandsontableOutput("shot")),
              tabPanel("note")
            )
          ),
          tabPanel("roster", rHandsontableOutput("roster")),
          tabPanel("player", rHandsontableOutput("player"))
        )
      )
    )
  )
)
