ui <- shinyUI(
  fluidPage(
    shinyjs::useShinyjs(),
    tags$script("
      select_view = function(id) { 
        Shiny.onInputChange('select_view', id);
      }
    "),
    sidebarLayout(
      sidebarPanel(
        numericInput("year", "year", value = year(today()), width = "100%"),
        selectInput("season", "season", choices = seasons, width = "100%"),
        numericInput("session", "session", value = 1, min = 1, width = "100%"),
        numericInput("week", "week", value = 1, min = 1, width = "100%"),
        numericInput("game", "game", value = 1, min = 1, width = "100%"),
        actionButton("load", "load", width = "100%"),
        actionButton("save", "save", width = "100%"),
        actionButton("delete", "delete", width = "100%"),
        width = 2
      ),
      mainPanel(
        tabsetPanel(
          id = "tabset",
          tabPanel(
            "view",
            tabsetPanel(
              tabPanel(
                "match",
                textOutput("matchup"),
                visNetworkOutput("assist"),
                tabsetPanel(
                  tabPanel("point", rHandsontableOutput("point", height = 750)),
                  tabPanel("penalty", rHandsontableOutput("penalty", height = 750)),
                  tabPanel("shot", rHandsontableOutput("shot", height = 750)),
                  tabPanel("note"),
                  type = "pills"
                )
              ),
              tabPanel("roster", rHandsontableOutput("roster", height = 750)),
              type = "pills"
            ),
          ),
          tabPanel("season", rHandsontableOutput("links")),
          tabPanel("team", rHandsontableOutput("team", height = 750)),
          tabPanel("player", rHandsontableOutput("player", height = 750))
        )
      )
    )
  )
)
