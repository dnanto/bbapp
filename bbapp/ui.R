ui <- shinyUI(
  fluidPage(
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
    sidebarLayout(
      sidebarPanel(
        HTML("<b>⓵ (year, season) → session</b>"),
        rHandsontableOutput("yss"),
        HTML("<b>⓶ (week, game)</b>"),
        rHandsontableOutput("wg"),
        width = 3
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "view",
            tabsetPanel(
              tabPanel(
                "match",
                htmlOutput("matchup"),
                visNetworkOutput("assist"),
                tabsetPanel(
                  tabPanel("point", rHandsontableOutput("point", height = 750)),
                  tabPanel("penalty", rHandsontableOutput("penalty", height = 750)),
                  tabPanel("shot", rHandsontableOutput("shot", height = 750)),
                  tabPanel("note"),
                  type = "pills"
                ),
              ),
              tabPanel("roster", rHandsontableOutput("roster", height = 750)),
              type = "pills"
            )
          ),
          tabPanel("team", rHandsontableOutput("team", height = 750)),
          tabPanel("player", rHandsontableOutput("player", height = 750))
        )
      )
    )
  )
)
