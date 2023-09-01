server <- shinyServer(function(input, output) {
  match <- eventReactive(input$load, {
    conn <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")
    params <- lapply(c("year", "season", "week", "game"), \(ele) input[[ele]])

    match <- list()
    match$team <- (
      DBI::dbReadTable(conn, "team") %>%
        mutate(name = as.factor(name))
    )
    match$player <- (
      DBI::dbReadTable(conn, "player") %>%
        mutate(name = as.factor(name))
    )
    match$v_roster <- (
      DBI::dbGetQuery(
        conn,
        "SELECT * FROM v_roster WHERE year == ? AND season == ?;",
        params = params[1:2]
      ) %>%
        mutate(
          team = factor(team, levels = levels(match$team$name)),
          player = factor(player, levels = levels(match$player$name)),
          color = str_sub(color, end = -3)
        )
    )
    match$v_matchup <- (
      DBI::dbGetQuery(
        conn,
        "SELECT * FROM v_matchup WHERE year == ? AND season == ? AND week == ? AND game == ?;",
        params = params
      ) %>%
        mutate(
          color = str_sub(color, end = -3)
        )
    )
    match$foul <- (
      DBI::dbReadTable(conn, "foul") %>%
        mutate(call = as.factor(call))
    )
    match <- c(
      match,
      sapply(
        c("v_point", "v_penalty", "v_shot", "v_assist"),
        \(ele)
        DBI::dbGetQuery(
          conn,
          glue::glue_sql(
            "SELECT * FROM {`ele`} WHERE year == ? AND season == ? AND week == ? AND game == ?;",
            .con = conn
          ),
          params = params
        ) %>%
          mutate(
            color = str_sub(color, end = -3),
            team = factor(team, levels = match$v_matchup$team),
            across(
              any_of(c("shooter", "assist1", "assist2", "player", "server", "goalie")),
              factor,
              levels = unique(sort(as.character(match$v_roster$player)))
            ),
            across(any_of(c("call")), factor, levels = levels(match$foul$call))
          )
      )
    )
    match$v_point <- mutate(match$v_point, across(types, as.logical))
    DBI::dbDisconnect(conn)

    match
  })

  output$matchup <- renderText({
    with(
      match()$v_matchup,
      str_c(str_glue("{team[1]} vs. {team[2]}"), if (is.na(rink[1])) c() else rink[1], sep = " @ ") %>%
        str_c(str_glue("{score[1]} - {score[2]}"), sep = ": ")
    )
  })

  output$assist <- renderVisNetwork({
    data <- match()
    nodes <- (
      data.frame(id = with(data$v_assist, unique(c(source_id, target_id)))) %>%
        left_join(data$v_roster, by = join_by(id == player_id), suffix = c("", ".roster")) %>%
        mutate(
          label = player
        )
    )
    edges <- (
      mutate(
        data$v_assist,
        label = as.character(point_id),
        from = source_id,
        to = target_id,
        arrows = "to",
        dashes = type == "A2"
      )
    )
    # print(nodes)
    # print(edges)
    visNetwork(nodes = nodes, edges = edges, background = "lightgrey")
  })

  output$point <- renderRHandsontable({
    data <- match()
    data$v_point %>%
      select(team, shooter, assist1, assist2, goalie, period, time, EV, PP, SH, EN) %>%
      rhandsontable(team_color = data$v_point$color, player_color = as.list(with(data$v_roster, setNames(color, player)))) %>%
      hot_col(c("team", "shooter", "assist1", "assist2", "goalie"), renderer = renderer.color) %>%
      hot_col("time", validator = validator.time) %>%
      hot_validate_numeric("period", min = 1)
  })

  output$penalty <- renderRHandsontable({
    data <- match()
    data$v_penalty %>%
      select(team, player, server, goalie, call, duration, period, time, scored) %>%
      rhandsontable(team_color = data$v_penalty$color, player_color = as.list(with(data$v_roster, setNames(color, player)))) %>%
      hot_col(c("team", "player", "server", "goalie"), renderer = renderer.color) %>%
      hot_col("time", validator = validator.time) %>%
      hot_validate_numeric("duration", min = 0) %>%
      hot_validate_numeric("period", min = 1)
  })

  output$shot <- renderRHandsontable({
    data <- match()
    data$v_shot %>%
      select(team, goalie, SH, period) %>%
      rhandsontable(team_color = data$v_shot$color, player_color = as.list(with(data$v_roster, setNames(color, player)))) %>%
      hot_col(c("team", "goalie"), renderer = renderer.color) %>%
      hot_validate_numeric("SH", min = 0) %>%
      hot_validate_numeric("period", min = 1)
  })

  output$roster <- renderRHandsontable({
    data <- match()
    data$v_roster %>%
      select(id, team, player) %>%
      rhandsontable(team_color = data$v_roster$color, player_color = as.list(with(data$v_roster, setNames(color, player)))) %>%
      hot_col(c("team", "player"), renderer = renderer.color)
  })

  output$player <- renderRHandsontable({
    data <- match()
    rhandsontable(data$player)
  })
})
