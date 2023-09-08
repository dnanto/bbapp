server <- shinyServer(function(input, output) {
  match <- eventReactive(input$load, {
    con <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")
    params <- lapply(c("year", "season", "week", "game"), \(ele) input[[ele]])

    match <- list()
    match$team <- (
      DBI::dbReadTable(con, "team") %>%
        mutate(name = as.factor(name))
    )
    match$player <- (
      DBI::dbReadTable(con, "player") %>%
        mutate(name = as.factor(name))
    )
    match$v_roster <- (
      DBI::dbGetQuery(
        con,
        "SELECT * FROM v_roster WHERE year == ? AND season == ?;",
        params = params[1:2]
      ) %>%
        mutate(
          team = factor(team, levels = levels(match$team$name)),
          player = factor(player, levels = levels(match$player$name))
        )
    )
    match$v_matchup <- (
      DBI::dbGetQuery(
        con,
        "SELECT * FROM v_matchup WHERE year == ? AND season == ? AND week == ? AND game == ?;",
        params = params
      )
    )
    match$foul <- (
      DBI::dbReadTable(con, "foul") %>%
        mutate(call = as.factor(call))
    )
    match <- c(
      match,
      sapply(
        c("v_point", "v_penalty", "v_shot", "v_assist"),
        \(ele)
        DBI::dbGetQuery(
          con,
          glue::glue_sql(
            "SELECT * FROM {`ele`} WHERE year == ? AND season == ? AND week == ? AND game == ?;",
            .con = con
          ),
          params = params
        ) %>%
          mutate(
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
    DBI::dbDisconnect(con)

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
      rhandsontable(rowHeaders = NULL, team_color = data$v_roster$color, player_color = as.list(with(data$v_roster, setNames(color, player)))) %>%
      hot_col("id", readOnly = T) %>%
      hot_col(c("team", "player"), renderer = renderer.color)
  })

  output$team <- renderRHandsontable({
    data <- match()$team
    mutate(data, name = as.character(name)) %>%
      rhandsontable(rowHeaders = NULL, team_color = data$color) %>%
      hot_col("id", readOnly = T) %>%
      hot_col("color", renderer = renderer.color)
  })

  output$player <- renderRHandsontable({
    mutate(match()$player, name = as.character(name)) %>%
      rhandsontable(rowHeaders = NULL) %>%
      hot_col("id", readOnly = T)
  })

  values <- reactiveValues(
    changed = c()
  )

  observeEvent(input$player$changes$changes, {
    df.hot <- hot_to_r(input$player)
    changes <- input$player$changes$changes[[1]]
    values$changed <- c(values$changed, df.hot$id[1 + changes[[1]]])
  })

  observeEvent(input$save, {
    df.hot <- hot_to_r(input$player)
    deleted <- setdiff(match()$player$id, df.hot$id)
    changed <- setdiff(values$changed, deleted)
    print(c("deleted", deleted))
    print(c("changed", changed))

    con <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")

    # DELETE
    if (length(deleted) > 0) {
      n <- (
        DBI::dbExecute(con, str_c("DELETE FROM player WHERE id IN (", str_c(deleted, collapse = ", "), ");"))
      )
      print(c(n, "deleted"))
    }

    # CREATE
    # new rows have missing id, so insert them and assign a new one
    idx <- which(is.na(df.hot$id))
    if (length(idx) > 0) {
      df.hot[idx, ]$id <- DBI::dbGetQuery(
        conn = con,
        str_c("INSERT INTO player VALUES (", str_c(rep("?", ncol(df.hot)), collapse = ", "), ") RETURNING id;"),
        params = unname(as.list(df.hot[idx, ]))
      )
    }
    idx <- c(idx, which(df.hot$id %in% changed))

    # UPDATE
    # changed rows have an id to coordinate the update
    if (length(idx) > 0) {
      df.hot[idx, ] %>%
        pivot_longer(-id, values_drop_na = T) %>%
        # get the latest updated value
        group_by(id, name) %>%
        slice_tail(n = 1) %>%
        ungroup() %>%
        # generate/run SQL
        with(glue::glue_sql("UPDATE player SET {`name`} = {value} WHERE id == {id};", .con = con)) %>%
        lapply(\(ele) {
          res <- DBI::dbSendStatement(con = con, statement = ele)
          status <- c(ele, DBI::dbHasCompleted(res), DBI::dbGetRowsAffected(res))
          DBI::dbClearResult(res)
          setNames(status, c("sql", "success", "n"))
        }) %>%
        bind_rows() %>%
        print()
    }

    values$changed <- values$changed[0]
    shinyjs::click("load")

    DBI::dbDisconnect(con)
  })
})
