server <- shinyServer(function(input, output, session) {
  # match

  output$yss <- function() {
    con <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")

    template <- str_squish("
      <span class='toggle'>
        <label>
          <input id='{year[1]}_{season[1]}_{session[idx]}' name='yss' type='radio' onclick='set_yss(this.id);'>{session[idx]}
        </label>
      </span>
    ")
    data <- (
      DBI::dbReadTable(con, "team") %>%
        distinct(year, season, session) %>%
        mutate(
          label = names(seasons)[season - min(seasons) + 1],
          season = factor(season, levels = seasons)
        ) %>%
        group_by(year, season) %>%
        summarise(
          value = str_c(lapply(seq_along(session), \(idx) glue::glue(template)), collapse = ""),
          .groups = "drop"
        ) %>%
        pivot_wider(names_from = season, names_expand = T, values_from = value, values_fill = "x") %>%
        setNames(c("year", names(seasons))) %>%
        arrange(desc(year))
    )
    DBI::dbDisconnect(con)
    data %>%
      knitr::kable(escape = F) %>%
      kableExtra::kable_styling("striped", full_width = F) %>%
      kableExtra::add_header_above(c(" ", "season" = ncol(data) - 1))
  }

  output$wg <- function() {
    req(input$set_yss)

    con <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")
    template <- str_squish("
      <span class='toggle'>
        <label>
          <input id='{week}_{game}' name='wg' type='radio' onclick='set_wg(this.id);'>({week}, {game})
        </label>
      </span>
    ")
    data <- (
      DBI::dbGetQuery(
        con,
        "SELECT DISTINCT week, game FROM v_matchup WHERE year == ? AND season == ? AND session == ?;",
        params = as.integer(str_split_fixed(input$set_yss, "_", 3))
      ) %>%
        arrange(desc(week)) %>%
        mutate(
          value = glue::glue(template)
        ) %>%
        pivot_wider(names_from = game, names_sort = T, values_from = value, values_fill = "x")
    )
    DBI::dbDisconnect(con)
    data %>%
      knitr::kable(escape = F) %>%
      kableExtra::kable_styling("striped", full_width = T) %>%
      kableExtra::add_header_above(c(" ", "game" = ncol(data) - 1))
  }

  observeEvent(input$set_yss, {
    updateTabsetPanel(session, "tabMatch", selected = "tabWG")
    updateTabsetPanel(session, "tabMatchPanels", selected = "match")
  })

  observeEvent(input$tabMatchPanels, {
    if (input$tabMatchPanels == "record") {
      updateTabsetPanel(session, "tabMatch", selected = "tabYSS")
    }
  })

  match <- eventReactive(input$set_wg, {
    con <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")
    DBI::dbExecute(con, "PRAGMA foreign_keys=on;")

    params <- c(
      as.integer(str_split_fixed(input$set_yss, "_", 3)),
      as.integer(str_split_fixed(input$set_wg, "_", 2))
    )

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
        "SELECT * FROM v_roster WHERE year == ? AND season == ? AND session == ?;",
        params = params[1:3]
      ) %>%
        mutate(
          team = factor(team, levels = levels(match$team$name)),
          player = factor(player, levels = levels(match$player$name)),
          captain = as.logical(as.integer(captain))
        )
    )
    match$v_matchup <- (
      DBI::dbGetQuery(
        con,
        "SELECT * FROM v_matchup WHERE year == ? AND season == ? AND session == ? AND week == ? AND game == ?;",
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
        function(ele) {
          DBI::dbGetQuery(
            con,
            glue::glue_sql(
              "SELECT * FROM {`ele`} WHERE year == ? AND season == ? AND session == ? AND week == ? AND game == ?;",
              .con = con
            ),
            params = params
          ) %>%
            mutate(
              team = factor(team, levels = unique(match$v_matchup$team)),
              across(
                any_of(c("shooter", "assist1", "assist2", "player", "server", "goalie")),
                \(ele) factor(ele, levels = unique(sort(as.character(match$v_roster$player))))
              ),
              across(any_of(c("call")), \(ele) factor(ele, levels = levels(match$foul$call)))
            )
        }
      )
    )
    match$v_point <- mutate(match$v_point, across(all_of(types), \(ele) as.logical(ele)))
    DBI::dbDisconnect(con)

    match
  })

  output$matchup <- renderText({
    with(
      match()$v_matchup,
      str_c(str_glue("{team[1]} vs. {team[2]}"), if (is.na(rink[1])) c() else rink[1], sep = " @ ") %>%
        str_c(str_glue("{score[1]} - {score[2]}"), sep = ": ")
    ) %>%
      str_c("<b>", ., "</b>", sep = "")
  })

  output$assist <- renderVisNetwork({
    data <- match()
    nodes <- (
      data.frame(id = with(data$v_assist, na.omit(unique(c(source_id, target_id))))) %>%
        left_join(data$v_roster, by = join_by(id == player_id), suffix = c("", ".roster")) %>%
        mutate(label = player) %>%
        rename(color.background = color)
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
    visNetwork(nodes = nodes, edges = edges, background = "#DCDCDC") %>%
      visNodes(
        borderWidth = 4,
        color = list(border = "black")
      )
  })

  output$point <- renderRHandsontable({
    data <- match()
    data$v_point %>%
      select(team, shooter, assist1, assist2, goalie, period, time, EV, PP, SH, EN) %>%
      rhandsontable(
        stretchH = "all",
        team_color = as.list(with(data$team, setNames(color, name))),
        shooter_color = as.list(with(data$v_roster, setNames(color, player))),
        assist1_color = as.list(with(data$v_roster, setNames(color, player))),
        assist2_color = as.list(with(data$v_roster, setNames(color, player))),
        goalie_color = as.list(with(data$v_roster, setNames(color, player)))
      ) %>%
      hot_cols(columnSorting = T) %>%
      hot_col(c("team", "shooter", "assist1", "assist2", "goalie"), renderer = renderer.color) %>%
      hot_col("time", validator = validator.time) %>%
      hot_validate_numeric("period", min = 1)
  })

  output$penalty <- renderRHandsontable({
    data <- match()
    data$v_penalty %>%
      select(team, player, server, goalie, call, duration, period, time, scored) %>%
      rhandsontable(
        stretchH = "all",
        team_color = as.list(with(data$team, setNames(color, name))),
        player_color = as.list(with(data$v_roster, setNames(color, player))),
        server_color = as.list(with(data$v_roster, setNames(color, player))),
        goalie_color = as.list(with(data$v_roster, setNames(color, player)))
      ) %>%
      hot_cols(columnSorting = T) %>%
      hot_col(c("team", "player", "server", "goalie"), renderer = renderer.color) %>%
      hot_col("time", validator = validator.time) %>%
      hot_validate_numeric("duration", min = 0) %>%
      hot_validate_numeric("period", min = 1)
  })

  output$shot <- renderRHandsontable({
    data <- match()
    data$v_shot %>%
      select(team, goalie, SH, period) %>%
      rhandsontable(
        stretchH = "all",
        team_color = as.list(with(data$team, setNames(color, name))),
        goalie_color = as.list(with(data$v_roster, setNames(color, player)))
      ) %>%
      hot_cols(columnSorting = T) %>%
      hot_col(c("team", "goalie"), renderer = renderer.color) %>%
      hot_validate_numeric("SH", min = 0) %>%
      hot_validate_numeric("period", min = 1)
  })

  observeEvent(input$set_yss, {
    con <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")

    params <- as.integer(str_split_fixed(input$set_yss, "_", 3))
    
    ## roster
    
    df.roster <- (
      DBI::dbGetQuery(
        con,
        "SELECT * FROM v_roster WHERE year == ? AND season == ? AND session == ?;",
        params = params
      ) %>%
        mutate(captain = as.logical(as.integer(captain)))
    )
    
    output$roster <- renderRHandsontable({
      df.roster %>%
        select(id, team, player, captain) %>%
        rhandsontable(
          stretchH = "all",
          rowHeaders = NULL,
          team_color = as.list(with(df.roster, setNames(color, team)))
        ) %>%
        hot_cols(columnSorting = T) %>%
        hot_col("id", readOnly = T) %>%
        hot_col(c("team", "player"), renderer = renderer.color)
    })
    
    ## record
    
    df.team <- DBI::dbGetQuery(
      con, 
      "SELECT id, name, color FROM team WHERE year == ? AND season == ? AND session == ?", 
      params
    )
    df.rink <- (
      DBI::dbReadTable(con, "rink") %>%
        select(id, name)
    )
    
    teams <- with(df.team, setNames(id, name))
    rinks <- with(df.rink, setNames(id, name))
    
    updateSelectInput(session, "home", choices = teams, selected = first(teams))
    updateSelectInput(session, "away", choices = teams, selected = last(teams))
    updateSelectInput(session, "rink", choices = rinks, selected = last(rinks))
    
    output$coordinate <- renderText({
      glue::glue("<b>Session {params[3]} of the {names(seasons)[params[2] - min(seasons) + 1]} {params[1]} Season</b>")
    })

    DBI::dbDisconnect(con)
  })

  observeEvent(c(input$home, input$away), {
    req(input$home, input$away)
    
    con <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")
    
    params <- as.integer(str_split_fixed(input$set_yss, "_", 3))
    
    df.roster <- (
      DBI::dbGetQuery(
        con,
        "SELECT * FROM v_roster WHERE year == ? AND season == ? AND session == ?;",
        params = params
      ) %>%
        mutate(captain = as.logical(as.integer(captain)))
    )
    df.team <- (
      DBI::dbGetQuery(
        con,
        "SELECT id, name, color FROM team WHERE id == ?;", 
        params = list(c(input$home, input$away))
      )
    )
    df.foul <- DBI::dbReadTable(con, "foul")
    
    team.labels <- c("home", "away")
    team.colors <- as.list(setNames(df.team$color, team.labels))
    player.labels <- with(df.roster, glue::glue("{player_id}. {player}"))
    
    output$record_shot <- renderRHandsontable({
      data.frame(
        team = c(team.labels, team.labels),
        goalie = "",
        SH = NA,
        period = c(1, 1, 2, 2)
      ) %>%
        mutate(
          team = factor(team, levels = team.labels),
          goalie = factor(goalie, levels = c("", player.labels)),
          across(c(SH, period), \(ele) as.integer(ele))
        ) %>%
        rhandsontable(
          height = 750, 
          stretchH = "all",
          team_color = team.colors
        ) %>%
        hot_col("team", renderer = renderer.color) %>%
        hot_validate_numeric("SH", min = 0) %>%
        hot_validate_numeric("period", min = 1)
    })
    
    output$record_point <- renderRHandsontable({
      data.frame(
        team = "",
        shooter = "",
        assist1 = "",
        assist2 = "",
        goalie = "",
        period = NA,
        time = "",
        EV = NA,
        PP = NA,
        SH = NA,
        EN = NA
      ) %>%
        mutate(
          across(c(goalie, assist1, assist2), \(ele) factor(ele, levels = c("", player.labels))),
          team = factor(team, levels = team.labels),
          shooter = factor(shooter, levels = player.labels),
          period = as.integer(period),
        ) %>%
        rhandsontable(
          stretchH = "all",
          team_color = team.colors
        ) %>%
        hot_col("team", renderer = renderer.color) %>%
        hot_validate_numeric("SH", min = 0) %>%
        hot_validate_numeric("period", min = 1)
    })
    
    output$record_penalty <- renderRHandsontable({
      data.frame(
        team = "",
        player = "",
        server = "",
        goalie = "",
        foul = "",
        duration = 2,
        period = NA,
        time = "",
        scored = NA
      ) %>%
        mutate(
          across(c(player, server, goalie), \(ele) factor(ele, levels = c("", player.labels))),
          across(c(duration, period), \(ele) as.integer(ele)),
          foul = factor(foul, levels = with(df.foul, glue::glue("{id}. {call}"))),
          team = factor(team, levels = team.labels),
        ) %>%
        rhandsontable(
          stretchH = "all",
          team_color = team.colors
        ) %>%
        hot_col("team", renderer = renderer.color) %>%
        hot_validate_numeric(c("period", "duration"), min = 1)
    })
    
    DBI::dbDisconnect(con)
  })
  
  observeEvent(input$submit_record, {
    conn <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")

    match_id <- (
      DBI::dbGetQuery(
        conn = conn,
        "INSERT INTO match VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING id;",
        params = c(NA, input$home, input$away, input$date, input$time, input$week, input$game, input$rink, NA)
      )
    )
    
    team_to_id <- setNames(1:2, c("home", "away"))
    
    df.shot <- hot_to_r(input$record_shot)
    if (!is.null(df.shot)) {
      df.shot %>%
        mutate(
          id = NA,
          match = match_id$id,
          team = team_to_id[team]
        ) %>%
        separate(goalie, c("goalie", "goalie.name"), ". ", extra = "merge", fill = "right") %>%
        select(id, match, team, goalie, SH, period) %>%
        DBI::dbAppendTable(conn, "shot", value = .) %>%
        print()
    }
    df.point <- hot_to_r(input$record_point)
    if (!is.null(df.point)) {
      df.point %>%
        mutate(
          id = NA,
          match = match_id$id,
          team = team_to_id[team],
          across(all_of(c("EV", "PP", "SH", "EN")), \(ele) as.integer(ele))
        ) %>%
        separate(shooter, c("shooter", "shooter.name"), ". ", extra = "merge", fill = "right") %>%
        separate(assist1, c("assist1", "assist1.name"), ". ", extra = "merge", fill = "right") %>%
        separate(assist2, c("assist2", "assist2.name"), ". ", extra = "merge", fill = "right") %>%
        separate(goalie, c("goalie", "goalie.name"), ". ", extra = "merge", fill = "right") %>%
        select(id, match, team, shooter, assist1, assist2, goalie, period, time, EV, PP, SH, EN) %>%
        DBI::dbAppendTable(conn, "point", value = .) %>%
        print()
    }
    df.penalty <- hot_to_r(input$record_penalty)
    if (!is.null(df.penalty)) {
      filter(df.penalty, team != "") %>%
        mutate(
          id = NA,
          match = match_id$id,
          team = team_to_id[team],
          scored = as.integer(scored)
        ) %>%
        separate(player, c("player", "player.name"), ". ", extra = "merge", fill = "right") %>%
        separate(server, c("server", "server.name"), ". ", extra = "merge", fill = "right") %>%
        separate(goalie, c("goalie", "goalie.name"), ". ", extra = "merge", fill = "right") %>%
        separate(foul, c("foul", "foul.call"), ". ", extra = "merge") %>%
        select(id, match, team, player, server, goalie, foul, duration, period, time, scored) %>%
        DBI::dbAppendTable(conn, "penalty", value = .) %>%
        print()
    }

    DBI::dbDisconnect(conn)
  })

  # team

  output$team <- renderRHandsontable({
    con <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")

    data <- (
      DBI::dbReadTable(con, "team") %>%
        arrange(desc(year), desc(season), desc(session))
    )
    result <- (
      data %>%
        rhandsontable(
          rowHeaders = NULL,
          stretchH = "all",
          color_color = as.list(unique(data$color) %>% setNames(., .)),
        ) %>%
        hot_cols(columnSorting = T) %>%
        hot_col("id", readOnly = T) %>%
        hot_col("color", renderer = renderer.color)
    )
    DBI::dbDisconnect(con)

    result
  })

  # player

  output$player <- renderRHandsontable({
    con <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")

    result <- (
      DBI::dbReadTable(con, "player") %>%
        rhandsontable(rowHeaders = NULL) %>%
        hot_cols(columnSorting = T) %>%
        hot_col("id", readOnly = T)
    )
    DBI::dbDisconnect(con)

    result
  })

  # upload roster

  observeEvent(input$input_roster, {
    con <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")
    df.player <- DBI::dbReadTable(con, "player")
    DBI::dbDisconnect(con)
    data <- (
      read_tsv(input$input_roster$datapath, col_types = cols(.default = "c")) %>%
        pivot_longer(cols = everything(), names_to = "team", values_to = "player", values_drop_na = T) %>%
        separate("team", c("color", "team"), ":") %>%
        mutate(
          color = tolower(color),
          captain = str_detect(player, "(, ?C| ?\\(C\\))$"),
          goalie = str_detect(player, "(, ?G| ?\\(G\\))$"),
          player = str_remove(player, "(, ?[CG]| ?\\([CG]\\))$")
        ) %>%
        arrange(color, player) %>%
        left_join(select(df.player, id, name), by = join_by(player == name))
    )
    output$upload_team <- renderRHandsontable({
      distinct(data, color, team) %>%
        rhandsontable(
          stretchH = "all",
          color_color = as.list(with(data, setNames(color, color)))
        ) %>%
        hot_col("color", renderer = renderer.color)
    })
    output$upload_roster <- renderRHandsontable({
      rhandsontable(
        data,
        stretchH = "all",
        color_color = as.list(with(data, setNames(color, color)))
      ) %>%
        hot_col("color", readOnly = T, renderer = renderer.color) %>%
        hot_col("team", readOnly = T) %>%
        hot_col("player", type = "dropdown", source = df.player$name, strict = F, allowInvalid = T)
    })
    outputOptions(output, "upload_team", suspendWhenHidden = F)
    outputOptions(output, "upload_roster", suspendWhenHidden = F)
  })

  observeEvent(input$upload_team$changes$changes, {
    coor <- input$upload_team$changes$changes[[1]]
    req(coor[[3]] != coor[[4]])
    data_team <- hot_to_r(input$upload_team)
    data_roster <- hot_to_r(input$upload_roster)
    key <- names(data_team)[coor[[2]] + 1]
    data_roster[[key]] <- if_else(data_roster[[key]] == coor[[3]], coor[[4]], data_roster[[key]])
    con <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")
    df.player <- DBI::dbReadTable(con, "player")
    DBI::dbDisconnect(con)
    output$upload_team <- renderRHandsontable({
      rhandsontable(
        data_team,
        stretchH = "all",
        color_color = as.list(with(data_team, setNames(color, color)))
      ) %>%
        hot_col("color", renderer = renderer.color)
    })
    output$upload_roster <- renderRHandsontable({
      rhandsontable(
        data_roster,
        stretchH = "all",
        color_color = as.list(with(data_team, setNames(color, color)))
      ) %>%
        hot_col("color", readOnly = T, renderer = renderer.color) %>%
        hot_col("team", readOnly = T) %>%
        hot_col("player", type = "dropdown", source = df.player$name, strict = F, allowInvalid = T)
    })
  })

  observeEvent(input$upload_roster$changes$changes, {
    print(input$upload_roster$changes$changes)
    coor <- input$upload_roster$changes$changes[[1]]
    req(coor[[3]] != coor[[4]])
    data_roster <- hot_to_r(input$upload_roster)
    key <- names(data_roster)[coor[[2]] + 1]
    req(key == "player")
    con <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")
    df.player <- DBI::dbReadTable(con, "player")
    result <- DBI::dbGetQuery(con, "SELECT id FROM player WHERE name == ?;", params = coor[[4]])
    DBI::dbDisconnect(con)
    data_roster[coor[[1]] + 1, "id"] <- ifelse(nrow(result) > 0, result$id, NA_integer_)
    output$upload_roster <- renderRHandsontable({
      rhandsontable(
        data_roster,
        stretchH = "all",
        color_color = as.list(with(distinct(data_roster, color), setNames(color, color)))
      ) %>%
        hot_col("color", readOnly = T, renderer = renderer.color) %>%
        hot_col("team", readOnly = T) %>%
        hot_col("player", type = "dropdown", source = df.player$name, strict = F, allowInvalid = T)
    })
  })

  observeEvent(input$submit_roster, {
    conn <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")
    
    data_team <- hot_to_r(input$upload_team)
    data_roster <- hot_to_r(input$upload_roster)
    
    df.team <- (
      data_team %>%
        rename(name = team) %>%
        mutate(id = NA) %>%
        mutate(id, year = input$input_year, season = input$input_season, session = input$input_session)
    )
    df.team <- bind_cols(
      DBI::dbGetQuery(
        conn = conn,
        glue::glue_sql("INSERT INTO team VALUES (:id, :year, :season, :session, :name, :color) RETURNING id;", .con = con),
        params = df.team
      ),
      select(df.team, -id)
    )
    print(df.team)
    
    df.player <- (
      data_roster %>%
        filter(is.na(id)) %>%
        mutate(id = NA) %>%
        rename(name = player) %>%
        select(id, name)
    )
    df.player <- bind_cols(
      DBI::dbGetQuery(
        conn = conn,
        glue::glue_sql("INSERT INTO player VALUES (:id, :name) RETURNING id;", .con = con),
        params = df.player
      ),
      select(df.player, -id)
    )
    print(df.player)
    
    ls.team <- with(df.team, setNames(id, name))
    ls.player <- with(df.player, setNames(id, name))
    
    df.roster <- (
      data_roster %>%
        mutate(
          team.id = ls.team[team],
          player.id = coalesce(id, ls.player[player]),
          captain = as.integer(captain)
        ) %>%
        select(team.id, player.id, captain) %>%
        rename(team = team.id, player = player.id) %>%
        mutate(id = NA)
    )
    df.roster <- bind_cols(
      DBI::dbGetQuery(
        conn = conn,
        glue::glue_sql("INSERT INTO roster VALUES (:id, :team, :player, :captain) RETURNING id;", .con = con),
        params = df.roster
      ),
      select(df.roster, -id)
    )
    print(df.roster)
    
    DBI::dbDisconnect(conn)
  })

  # CRUD

  values <- reactiveValues(
    changed = sapply(c("team", "player", "roster"), \(ele) NULL)
  )

  lapply(
    c("team", "player", "roster"),
    function(ele) {
      observeEvent(input[[ele]]$changes$changes, {
        df.hot <- hot_to_r(input[[ele]])
        changes <- input[[ele]]$changes$changes[[1]]
        # print(c(ele, changes))
        values$changed[[ele]] <- c(values$changed[[ele]], df.hot$id[1 + changes[[1]]])
      })
    }
  )

  observeEvent(input$save, {
    con <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")
    DBI::dbExecute(con, "PRAGMA foreign_keys=on;")

    lapply(
      c("team", "player", "roster"),
      function(ele) {
        # print(values$changed)
        df.hot <- hot_to_r(input[[ele]])
        deleted <- if (is.null(df.hot)) c() else setdiff(match()[[ele]]$id, df.hot$id)
        changed <- setdiff(values$changed[[ele]], deleted)

        print(c("deleted", ele, deleted))
        print(c("changed", ele, changed))
        # DELETE
        if (length(deleted) > 0) {
          n <- (
            DBI::dbExecute(con, glue::glue_sql("DELETE FROM {`ele`} WHERE id IN (", str_c(deleted, collapse = ", "), ");", .con = con))
          )
          print(c(n, "deleted"))
        }

        # CREATE
        # new rows have missing id, so insert them and assign a new one
        idx <- which(is.na(df.hot$id))
        if (length(idx) > 0) {
          df.hot[idx, ]$id <- DBI::dbGetQuery(
            conn = con,
            glue::glue_sql("INSERT INTO {`ele`} VALUES (", str_c(rep("?", ncol(df.hot)), collapse = ", "), ") RETURNING id;", .con = con),
            params = unname(as.list(df.hot[idx, ]))
          )
        }
        idx <- c(idx, which(df.hot$id %in% changed))

        # UPDATE
        # changed rows have an id to coordinate the update
        if (length(idx) > 0) {
          df.hot[idx, ] %>%
            mutate(across(everything(), \(ele) as.character(ele))) %>%
            pivot_longer(-id, values_drop_na = T) %>%
            # get the latest updated value
            group_by(id, name) %>%
            slice_tail(n = 1) %>%
            ungroup() %>%
            # generate/run SQL
            with(glue::glue_sql("UPDATE {`ele`} SET {`name`} = {value} WHERE id == {id};", .con = con)) %>%
            lapply(function(ele) {
              res <- DBI::dbSendStatement(con = con, statement = ele)
              status <- c(ele, DBI::dbHasCompleted(res), DBI::dbGetRowsAffected(res))
              DBI::dbClearResult(res)
              setNames(status, c("sql", "success", "n"))
            }) %>%
            bind_rows() %>%
            print()
        }

        values$changed[[ele]] <- values$changed[[ele]][0]
      }
    )

    DBI::dbDisconnect(con)
  })
})
