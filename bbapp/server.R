server <- shinyServer(function(input, output) {
  match <- eventReactive(input$load, {
    conn <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")
    params <- lapply(c("year", "season", "week", "game"), \(ele) input[[ele]])
    
    match <- list()
    match$player <- (
      DBI::dbReadTable(conn, "player") %>%
        mutate(name = as.factor(name))
    )
    match$v_roster <- (
      DBI::dbGetQuery(
        conn, 
        "SELECT * FROM v_roster WHERE year == ? AND season = ?;",
        params = params[1:2]
      )
    )
    match <- c(
      match,
      sapply(
        c("v_point", "v_penalty", "v_shot", "v_assist"),
        \(ele)
          DBI::dbGetQuery(
            conn,
            glue::glue_sql(
              "SELECT * FROM {`ele`} WHERE year == ? AND season = ? AND week == ? AND game == ?;",
              .con = conn
            ),
            params = params
          ) %>%
          mutate(
            color = str_remove(names(hex_to_color(color)), "[0-9]+$")
          )
      )
    )
    DBI::dbDisconnect(conn)
    
    match
  })

  output$assist <- renderVisNetwork({
    data <- match()
    nodes <- (
      data.frame(id = with(data$v_assist, unique(c(source_id, target_id)))) %>%
        left_join(data$v_roster, by = join_by(id == player_id), suffix = c("", ".roster")) %>%
        mutate(
          label = player,
          color = str_sub(color, end = -3)
        )
    )
    edges <- (
      select(data$v_assist, assist_id, source_id, target_id, type) %>%
        mutate(
          label = as.character(assist_id),
          arrows = "to",
          dashes = type == "A2"
        ) %>%
        rename(from = source_id, to = target_id)
    )
    print(nodes)
    print(edges)
    visNetwork(nodes = nodes, edges = edges, background = "lightgrey")
  })
  
  output$point <- renderRHandsontable({
    data <- match()
    data$v_point %>%
      select(color, shooter, assist1, assist2, goalie, period, time, EV, PP, SH, EN) %>%
      rhandsontable()
  })

  output$penalty <- renderRHandsontable({
    data <- match()
    data$v_penalty %>%
      select(color, player, server, goalie, call, duration, period, time, scored) %>%
      rhandsontable()
  })

  output$shot <- renderRHandsontable({
    data <- match()
    data$v_shot %>%
      select(color, goalie, SH, period) %>%
      rhandsontable()
  })
  
  output$roster <- renderRHandsontable({
    data <- match()
    data$v_roster %>%
      select(color, player) %>%
      rhandsontable()
  })
  
  output$player <- renderRHandsontable({
    data <- match()
    rhandsontable(data$player)
  })
})
