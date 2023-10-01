ui <- shinyUI(
  ui = htmltools::tagList(
    shinyjs::useShinyjs(),
    tags$style("
      .toggle {
        background: #DCDCDC;
        border: 2px solid gray;
        border-radius: 10px;
        padding: 2px;
      }
    "),
    tags$script("
      set_yss = function(id) {
        console.log(id);
        Shiny.onInputChange('set_yss', id);
      }
      set_wg = function(id) {
        console.log(id);
        Shiny.onInputChange('set_wg', id);
      }
    "),
    navbarPage(
      "BBAPP",
      tabPanel(
        "Match",
        sidebarLayout(
          sidebarPanel(
            tabsetPanel(
              id = "tabMatch",
              tabPanel(
                "⓵ session",
                HTML("<b>(year, season) → session</b>"),
                div(style = "overflow-x: scroll", tableOutput("yss")),
                value = "tabYSS"
              ),
              tabPanel(
                "⓶ game",
                HTML("<b>(week, game)</b>"),
                div(style = "overflow-x: scroll", tableOutput("wg")),
                value = "tabWG"
              )
            ),
            width = 3
          ),
          mainPanel(
            tabsetPanel(
              id = "tabMatchPanels",
              tabPanel(
                "match",
                tags$br(),
                htmlOutput("matchup"),
                tags$hr(),
                visNetworkOutput("assist"),
                tags$hr(),
                tabsetPanel(
                  tabPanel("point", rHandsontableOutput("point", height = 750)),
                  tabPanel("penalty", rHandsontableOutput("penalty", height = 750)),
                  tabPanel("shot", rHandsontableOutput("shot", height = 750)),
                  tabPanel("note"),
                  type = "pills"
                ),
              ),
              tabPanel("roster", rHandsontableOutput("roster", height = 750)),
              tabPanel(
                "record",
                sidebarPanel(
                  htmlOutput("coordinate"),
                  tags$hr(),
                  selectInput("home", "home", choices = NULL),
                  selectInput("away", "away", choices = NULL),
                  dateInput("date", "date", value = as.Date(now())),
                  textInput("time", "time"),
                  numericInput("week", "week", value = 1, min = 1),
                  numericInput("game", "game", value = 1, min = 1),
                  selectInput("rink", "rink", choices = NULL),
                  tags$hr(),
                  actionButton("submit_record", "Submit", width = "100%"),
                  width = 4
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel(
                      "shot",
                      rHandsontableOutput("record_shot", height = 750)
                    ),
                    tabPanel(
                      "point",
                      rHandsontableOutput("record_point", height = 750)
                    ),
                    tabPanel(
                      "penalty",
                      rHandsontableOutput("record_penalty", height = 750)
                    )
                  )
                )
              ),
              type = "pills"
            )
          )
        )
      ),
      tabPanel("Team", rHandsontableOutput("team", height = 750)),
      tabPanel("Player", rHandsontableOutput("player", height = 750)),
      tabPanel(
        "Upload",
        sidebarPanel(
          fileInput("input_roster", "Upload roster...", width = "100%"),
          numericInput("input_year", "Year", value = year(now()), width = "100%"),
          selectInput("input_season", "Season", choices = seasons, selected = seasons[1], width = "100%"),
          numericInput("input_session", "Session", value = 1, min = 1, width = "100%"),
          tags$hr(),
          actionButton("submit_roster", "Submit", width = "100%")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("team", rHandsontableOutput("upload_team", height = 750)),
            tabPanel("roster", rHandsontableOutput("upload_roster", height = 750))
          )
        )
      )
    )
  )
)
