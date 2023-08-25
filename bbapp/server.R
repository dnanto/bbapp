server <- shinyServer(function(input, output) {
  match <- eventReactive(input$load, {
    con <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")
    
    match <- (
      DBI::dbGetQuery(
        con,
        "
          SELECT * FROM match
          WHERE year == ? AND season = ? AND week == ? AND game == ?
        ",
        params = lapply(c("year", "season", "week", "game"), \(ele) input[[ele]])
      ) %>%
        unlist()
    )

    roster <- (
      DBI::dbGetQuery(
        con,
        "
          SELECT color, roster.* FROM roster
          JOIN match ON match.id == ? AND (team == team1 OR team == team2)
          JOIN team ON team == team.id
          ORDER BY player
        ",
        params = match[["id"]]
      )
    )

    result <- (
      sapply(
        c("point", "penalty", "shot"),
        \(ele)
        glue::glue_sql(
          "
            SELECT color, {`ele`}.* FROM {`ele`}
            JOIN team ON team == team.id
            WHERE match == ?
          ",
          .con = con
        ) %>%
          DBI::dbGetQuery(con, ., params = match[["id"]])
      ) %>%
        c(match, roster = roster, .)
    )
    DBI::dbDisconnect(con)
    
    result
  })

  output$point <- renderRHandsontable({
    data <- match()
    separate(data$point, time, c("min", "sec"), sep = ":", remove = T) %>%
      select(all_of(df.ui$point$field)) %>%
      mutate(across(types, as.logical)) %>%
      rhandsontable(width = 1000, height = 1000) %>%
      hot_col(col = "color", type = "dropdown", source = sort(unique(data$roster$color))) %>%
      hot_col(col = c("shooter", "assist1", "assist2", "goalie"), type = "dropdown", source = data$roster$player) %>%
      hot_col(col = "period", type = "numeric") %>%
      hot_col(col = c("min", "sec"), type = "numeric") %>%
      hot_col(col = types, type = "checkbox") %>%
      hot_validate_numeric(cols = "period", min = 1, max = 4) %>%
      hot_validate_numeric(cols = "min", min = 0) %>%
      hot_validate_numeric(cols = "sec", min = 0, max = 59.999)
  })

  output$penalty <- renderRHandsontable({
    data <- match()
    separate(data$penalty, time, c("min", "sec"), sep = ":", remove = T) %>%
      select(all_of(df.ui$penalty$field)) %>%
      mutate(scored = as.logical(scored)) %>%
      rhandsontable(width = 1000, height = 1000) %>%
      hot_col(col = "color", type = "dropdown", source = sort(unique(data$roster$color))) %>%
      hot_col(col = c("player", "server", "goalie"), type = "dropdown", source = data$roster$player) %>%
      hot_col(col = "call", type = "dropdown", source = calls) %>%
      hot_col(col = "duration", type = "numeric") %>%
      hot_col(col = "period", type = "numeric") %>%
      hot_col(col = c("min", "sec"), type = "numeric") %>%
      hot_col(col = "scored", type = "checkbox") %>%
      hot_validate_numeric(cols = "duration", min = 0) %>%
      hot_validate_numeric(cols = "period", min = 1, max = 4) %>%
      hot_validate_numeric(cols = "min", min = 0) %>%
      hot_validate_numeric(cols = "sec", min = 0, max = 59.999)
  })

  output$shot <- renderRHandsontable({
    data <- match()
    select(data$shot, all_of(df.ui$shot$field)) %>%
      rhandsontable(width = 1500, height = 1000) %>%
      hot_col(col = "color", type = "dropdown", source = sort(unique(data$roster$color))) %>%
      hot_col(col = "SH", type = "numeric") %>%
      hot_col(col = "goalie", type = "dropdown", source = data$roster$player) %>%
      hot_col(col = "period", type = "numeric") %>%
      hot_validate_numeric(cols = "SH", min = 0) %>%
      hot_validate_numeric(cols = "period", min = 1, max = 4)
  })
  
  output$roster <- renderRHandsontable({
    data <- match()
    con <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")
    result <- (
      DBI::dbGetQuery(
        con,
        "
          SELECT DISTINCT year, season, color, roster.* FROM roster
          JOIN match ON year = ? AND season = ? AND (team == team1 OR team == team2)
          JOIN team ON team = team.id
        ",
        params = c(data$year, data$season)
      )
    )
    DBI::dbDisconnect(con)
    mutate(
      result,
      season = names(seasons)[season - 21 + 1],
      across(c("season", "color", "team", "player"), as.factor)
    ) %>%
      rhandsontable(width = 1000, height = 1000)
  })
})
