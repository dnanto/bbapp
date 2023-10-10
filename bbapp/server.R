server <- function(input, output, session) {
  # match
  
  output$yss <- function() {
    template <- str_squish("
      <span class='toggle'>
        <label>
          <input id='yss_{year[1]}_{season[1]}_{session[idx]}' name='yss' type='radio' onclick='set_yss(this.id);'>{session[idx]}
        </label>
      </span>
    ")
    data <- (
      tbl(pool, "season") %>%
        as_tibble() %>%
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
    data %>%
      knitr::kable(escape = F) %>%
      kableExtra::kable_styling("striped", full_width = F) %>%
      kableExtra::add_header_above(c(" ", "season" = ncol(data) - 1))
  }

  output$wg <- function() {
    template <- str_squish("
      <span class='toggle'>
        <label>
          <input id='wg_{week}_{game}' name='wg' type='radio' onclick='set_wg(this.id);' {checked}>({week}, {game})
        </label>
      </span>
    ")
    params <- tail(unlist(strsplit(input$set_yss, "_")), -1)
    data <- (
      tbl(pool, "v_matchup") %>%
        filter(year == !!params[1], season == !!params[2], session == !!params[3]) %>%
        distinct(week, game) %>%
        arrange(desc(week)) %>%
        as_tibble() %>%
        mutate(
          checked = if_else(week == 1 & game == 1, "checked", ""),
          value = glue::glue(template)
        ) %>%
        select(-checked) %>%
        pivot_wider(names_from = game, names_sort = T, values_from = value, values_fill = "x")
    )
    data %>%
      knitr::kable(escape = F) %>%
      kableExtra::kable_styling("striped", full_width = T) %>%
      kableExtra::add_header_above(c(" ", "game" = ncol(data) - 1))
  }

  observeEvent(input$set_yss, {
    shinyjs::runjs("set_wg('wg_1_1');")
    updateTabsetPanel(session, "tabMatch", selected = "tabWG")
    updateTabsetPanel(session, "tabMatchPanels", selected = "match")
  })

  observeEvent(input$tabMatchPanels, {
    if (input$tabMatchPanels == "record") {
      updateTabsetPanel(session, "tabMatch", selected = "tabYSS")
    }
  })

  output$coordinate <- renderText({
    req(input$set_yss)
    glue::glue(
      "<b>Session {params[3]} of the {names(seasons)[params[2] - min(seasons) + 1]} {params[1]} Season</b>",
      params = as.integer(tail(unlist(strsplit(input$set_yss, "_")), -1))
    )
  })

  ## match

  roster <- eventReactive(input$set_yss, {
    params <- tail(unlist(str_split(input$set_yss, "_")), -1)
    tbl(pool, "v_roster") %>%
      filter(year == !!params[1], season == !!params[2], session == !!params[3]) %>%
      as_tibble()
  })

  match <- eventReactive(c(input$set_yss, input$set_wg), {
    params <- c(
      tail(unlist(str_split(input$set_yss, "_")), -1),
      tail(unlist(str_split(input$set_wg, "_")), -1)
    )
    df.matchup <- (
      tbl(pool, "v_matchup") %>%
        filter(
          year == !!params[1], season == !!params[2], session == !!params[3],
          week == !!params[4], game == !!params[5]
        ) %>%
        as_tibble()
    )
    req(nrow(df.matchup) > 0)
    df.matchup
  })

  observe({
    data <- match()
    df.roster <- arrange(roster(), player)
    match_id <- first(data$match_id)
    player_id <- with(df.roster, setNames(player_id, player))
    player_labels <- with(df.roster, glue::glue("{player_id}. {player}"))
    team_labels <- setNames(c("home", "away"), data$team)
    team_color <- as.list(setNames(data$color, unname(team_labels)))
    roster_color <- as.list(with(df.roster, setNames(color, player_labels)))

    ### matchup

    output$matchup <- renderText({
      with(
        data,
        str_c(
          str_glue(
            "<span style='border: 4px dotted {color[1]};'>{team[1]}</span> vs. <span style='border: 4px dotted {color[2]}'>{team[2]}</span>"
          ),
          if (is.na(rink[1])) c() else rink[1],
          sep = " @ "
        ) %>% str_c(str_glue("{score[1]} - {score[2]}"), sep = ": ")
      ) %>%
        str_c("<b>", ., ".</b>", sep = "")
    })

    ### assist

    output$assist <- renderVisNetwork({
      df.assist <- (
        tbl(pool, "v_assist") %>%
          filter(match == match_id)
      )
      df.node <- (
        df.assist %>%
          select(assist_id, source_id, target_id) %>%
          pivot_longer(cols = c(source_id, target_id), values_to = "id") %>%
          distinct(id) %>%
          as_tibble() %>%
          left_join(df.roster, by = join_by(id == player_id)) %>%
          mutate(label = str_c(id, ". ", player)) %>%
          rename(color.background = color)
      )
      df.edge <- (
        df.assist %>%
          mutate(
            label = as.character(point_id),
            from = source_id,
            to = target_id,
            arrows = "to",
            dashes = type == "A2"
          ) %>%
          as_tibble()
      )
      visNetwork(nodes = df.node, edges = df.edge, background = "#DCDCDC") %>%
        visNodes(
          borderWidth = 4,
          color = list(border = "black")
        )
    })

    ### point

    output$point <- renderRHandsontable({
      tbl(pool, "v_point") %>%
        filter(match == match_id) %>%
        as_tibble() %>%
        mutate(
          across(c(shooter, goalie, assist1, assist2), \(ele) str_c(player_id[ele], ". ", ele)),
          across(c(goalie, assist1, assist2), \(ele) factor(ele, levels = c("", player_labels))),
          across(c("EV", "PP", "SH", "EN"), \(ele) as.logical(ele)),
          team = factor(team_labels[team], levels = unname(team_labels)),
          shooter = factor(shooter, levels = player_labels)
        ) %>%
        select(team, shooter, assist1, assist2, goalie, period, time, EV, PP, SH, EN) %>%
        rhandsontable(
          stretchH = "all",
          team_color = team_color,
          shooter_color = roster_color,
          assist1_color = roster_color,
          assist2_color = roster_color,
          goalie_color = roster_color
        ) %>%
        hot_cols(columnSorting = T) %>%
        hot_col(c("team", "shooter", "assist1", "assist2", "goalie"), renderer = renderer.color) %>%
        hot_col("period", renderer = renderer.integer) %>%
        hot_col("time", validator = validator.time.stat) %>%
        hot_validate_numeric("period", min = 1)
    })

    ### penalty

    output$penalty <- renderRHandsontable({
      df.foul <- as_tibble(tbl(pool, "foul"))
      fouls <- with(df.foul, setNames(str_c(id, ". ", call), call))
      tbl(pool, "v_penalty") %>%
        filter(match == match_id) %>%
        as_tibble() %>%
        mutate(
          across(
            c(player, server, goalie),
            \(ele) factor(str_c(player_id[ele], ". ", ele), levels = c("", player_labels))
          ),
          team = factor(team_labels[team], levels = unname(team_labels)),
          call = factor(setNames(str_c(foul_id, ". ", call), call), levels = fouls)
        ) %>%
        select(team, player, server, goalie, call, duration, period, time, scored) %>%
        rhandsontable(
          stretchH = "all",
          team_color = team_color,
          player_color = roster_color,
          server_color = roster_color,
          goalie_color = roster_color
        ) %>%
        hot_cols(columnSorting = T) %>%
        hot_col(c("team", "player", "server", "goalie"), renderer = renderer.color) %>%
        hot_col(c("duration", "period"), renderer = renderer.integer) %>%
        hot_col("time", validator = validator.time.stat) %>%
        hot_validate_numeric("duration", min = 0) %>%
        hot_validate_numeric("period", min = 1)
    })

    ### shot

    output$shot <- renderRHandsontable({
      tbl(pool, "v_shot") %>%
        filter(match == match_id) %>%
        as_tibble() %>%
        mutate(
          goalie = factor(
            str_c(player_id[goalie], ". ", goalie),
            levels = c("", player_labels)
          ),
          team = factor(team_labels[team], levels = unname(team_labels))
        ) %>%
        select(team, goalie, SH, period) %>%
        rhandsontable(
          stretchH = "all",
          team_color = team_color,
          goalie_color = roster_color
        ) %>%
        hot_cols(columnSorting = T) %>%
        hot_col(c("team", "goalie"), renderer = renderer.color) %>%
        hot_col(c("SH", "period"), renderer = renderer.integer) %>%
        hot_validate_numeric("SH", min = 0) %>%
        hot_validate_numeric("period", min = 1)
    })

    ### meta

    output$meta <- renderRHandsontable({
      df.team <- distinct(df.roster, team_id, team, color)
      team_labels <- with(df.team, glue::glue("{team_id}. {team}"))
      team_color <- as.list(setNames(df.team$color, team_labels))
      tbl(pool, "match") %>%
        filter(id == match_id) %>%
        left_join(select(tbl(pool, "team"), id, name, color), by = join_by(home == id)) %>%
        left_join(select(tbl(pool, "team"), id, name, color), by = join_by(away == id), suffix = c(".home", ".away")) %>%
        as_tibble() %>%
        mutate(
          home = factor(str_c(home, ". ", name.home), levels = team_labels),
          away = factor(str_c(away, ". ", name.away), levels = team_labels)
        ) %>%
        select(home, away, date, time, week, game, rink, meta) %>%
        rhandsontable(
          stretchH = "all",
          home_color = team_color,
          away_color = team_color,
          rowHeaders = F
        ) %>%
        hot_col(c("home", "away"), renderer = renderer.color) %>%
        hot_col("date", type = "date", dateFormat = "YYYY-MM-DD") %>%
        hot_col("time", validator = validator.time.match) %>%
        hot_validate_numeric(c("week", "game"), min = 1)
    })
  })

  ## roster

  output$roster <- renderRHandsontable({
    data <- roster()
    team_color <- as.list(with(distinct(data, team, color), setNames(color, team)))
    select(data, id, team, player, captain) %>%
      arrange(team) %>%
      mutate(captain = as.logical(captain)) %>%
      rhandsontable(
        stretchH = "all",
        rowHeaders = NULL,
        team_color = team_color
      ) %>%
      hot_cols(columnSorting = T) %>%
      hot_col("id", readOnly = T) %>%
      hot_col(c("team", "player"), renderer = renderer.color)
  })

  ## record

  observe({
    df.roster <- roster()
    teams <- with(df.roster, setNames(team_id, team))
    updateSelectInput(session, "home", choices = teams, selected = first(teams))
    updateSelectInput(session, "away", choices = teams, selected = last(teams))
  })

  observeEvent(c(input$home, input$away), {
    req(input$home, input$away)

    params <- tail(unlist(str_split(input$set_yss, "_")), -1)

    df.roster <- roster()
    df.team <- (
      pool::dbGetQuery(
        pool,
        "SELECT id, name, color FROM team WHERE id == ?;",
        params = list(c(input$home, input$away))
      )
    )
    df.foul <- as_tibble(tbl(pool, "foul"))

    team_labels <- c("home", "away")
    team_colors <- as.list(with(df.team, setNames(color, team_labels)))
    player_labels <- with(df.roster, glue::glue("{player_id}. {player}"))

    output$record_shot <- renderRHandsontable({
      data.frame(team = c(team_labels, team_labels), goalie = "", SH = NA_integer_, period = c(1, 1, 2, 2)) %>%
        mutate(
          team = factor(team, levels = team_labels),
          goalie = factor(goalie, levels = c("", player_labels)),
        ) %>%
        rhandsontable(
          height = 750,
          stretchH = "all",
          team_color = team_colors
        ) %>%
        hot_col("team", renderer = renderer.color) %>%
        hot_col(c("SH", "period"), renderer = renderer.integer) %>%
        hot_validate_numeric("SH", min = 0) %>%
        hot_validate_numeric("period", min = 1)
    })

    output$record_point <- renderRHandsontable({
      bind_rows(lapply(1:20, function(idx) {
        data.frame(
          team = "",
          shooter = "", assist1 = "", assist2 = "", goalie = "",
          period = NA_integer_, time = "",
          EV = F, PP = F, SH = F, EN = F
        )
      })) %>%
        mutate(
          across(c(goalie, assist1, assist2), \(ele) factor(ele, levels = c("", player_labels))),
          shooter = factor(shooter, levels = player_labels),
          team = factor(team, levels = team_labels)
        ) %>%
        rhandsontable(
          stretchH = "all",
          team_color = team_colors
        ) %>%
        hot_col("team", renderer = renderer.color) %>%
        hot_col("period", renderer = renderer.integer) %>%
        hot_validate_numeric("period", min = 1)
    })

    output$record_penalty <- renderRHandsontable({
      bind_rows(lapply(1:20, function(idx) {
        data.frame(
          team = "",
          player = "", server = "", goalie = "",
          foul = "", duration = NA_integer_,
          period = NA_integer_, time = "",
          scored = NA
        )
      })) %>%
        mutate(
          across(c(player, server, goalie), \(ele) factor(ele, levels = c("", player_labels))),
          foul = factor(foul, levels = with(df.foul, glue::glue("{id}. {call}"))),
          team = factor(team, levels = team_labels),
        ) %>%
        rhandsontable(
          stretchH = "all",
          team_color = team_colors
        ) %>%
        hot_col("team", renderer = renderer.color) %>%
        hot_col(c("period", "duration"), renderer = renderer.integer) %>%
        hot_validate_numeric(c("period", "duration"), min = 1)
    })
  })

  observeEvent(input$submit_record, {
    req(input$set_yss)
    tryCatch(
      pool::poolWithTransaction(pool, function(conn) {
        params <- tail(unlist(strsplit(input$set_yss, "_")), -1)
        season_id <- insert_or_get_season_id(conn, params[1], params[2], params[3])
        
        match_id <- (
          pool::dbGetQuery(
            conn = conn,
            "INSERT INTO match VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING id;",
            params = c(NA, season_id, input$home, input$away, input$rink, input$week, input$game, input$date, input$time, input$record_meta)
          )
        )
        print(match_id)
        team_to_id <- setNames(1:2, c("home", "away"))

        print("point")
        df.point <- hot_to_r(input$record_point)
        n <- 0
        if (!is.null(df.point)) {
          n <- (
            df.point %>%
              mutate(
                across(all_of(c("assist1", "assist2", "goalie")), \(ele) na_if(ele, "")),
                across(all_of(c("EV", "PP", "SH", "EN")), \(ele) as.integer(ele)),
                id = NA,
                match = match_id$id,
                team = team_to_id[team]
              ) %>%
              filter(!is.na(team), !is.na(shooter)) %>%
              separate(shooter, c("shooter", "shooter.name"), ". ", extra = "merge", fill = "right") %>%
              separate(assist1, c("assist1", "assist1.name"), ". ", extra = "merge", fill = "right") %>%
              separate(assist2, c("assist2", "assist2.name"), ". ", extra = "merge", fill = "right") %>%
              separate(goalie, c("goalie", "goalie.name"), ". ", extra = "merge", fill = "right") %>%
              select(id, match, team, shooter, assist1, assist2, goalie, period, time, EV, PP, SH, EN) %T>%
              print() %>%
              pool::dbAppendTable(conn, "point", value = .)
          )
        }

        if (n < 1) {
          pool::dbBreak()
        }

        print("shot")
        df.shot <- hot_to_r(input$record_shot)
        if (!is.null(df.shot)) {
          df.shot %>%
            mutate(
              id = NA,
              match = match_id$id,
              team = team_to_id[team],
              goalie = na_if(goalie, "")
            ) %>%
            filter(!is.na(team)) %>%
            separate(goalie, c("goalie", "goalie.name"), ". ", extra = "merge", fill = "right") %>%
            select(id, match, team, goalie, SH, period) %T>%
            print() %>%
            pool::dbAppendTable(conn, "shot", value = .) %>%
            print()
        }

        print("penalty")
        df.penalty <- hot_to_r(input$record_penalty)
        if (!is.null(df.penalty)) {
          filter(df.penalty, team != "") %>%
            mutate(
              across(all_of(c("player", "server", "goalie")), \(ele) na_if(ele, "")),
              id = NA,
              match = match_id$id,
              team = team_to_id[team],
              scored = as.integer(scored)
            ) %>%
            filter(!is.na(team), !is.na(duration)) %>%
            separate(player, c("player", "player.name"), ". ", extra = "merge", fill = "right") %>%
            separate(server, c("server", "server.name"), ". ", extra = "merge", fill = "right") %>%
            separate(goalie, c("goalie", "goalie.name"), ". ", extra = "merge", fill = "right") %>%
            separate(foul, c("foul", "foul.call"), ". ", extra = "merge") %>%
            select(id, match, team, player, server, goalie, foul, duration, period, time, scored) %T>%
            print() %>%
            pool::dbAppendTable(conn, "penalty", value = .) %>%
            print()
        }

        pool::dbBreak()
      }),
      error = function(e) print(e)
    )
  })

  # team

  output$team <- renderRHandsontable({
    color_color <- (
      tbl(pool, "team") %>%
        distinct(color) %>%
        as_tibble() %>%
        with(setNames(color, color)) %>%
        as.list()
    )
    tbl(pool, "team") %>%
      left_join(tbl(pool, "season"), by = join_by(season == id)) %>%
      as_tibble() %>%
      select(-season) %>%
      rename(season = season.y) %>%
      arrange(desc(year), desc(season), desc(session)) %>%
      mutate(season = factor(names(seasons)[season - min(seasons) + 1], levels = names(seasons))) %>%
      rhandsontable(
        rowHeaders = NULL,
        stretchH = "all",
        color_color = color_color,
      ) %>%
      hot_cols(columnSorting = T) %>%
      hot_col("id", readOnly = T) %>%
      hot_col(c("year", "session"), renderer = renderer.integer) %>%
      hot_col("color", renderer = renderer.color) %>%
      hot_validate_numeric("session", min = 1)
  })

  # player

  output$player <- renderRHandsontable({
    tbl(pool, "player") %>%
      as_tibble() %>%
      rhandsontable(
        rowHeaders = NULL,
        stretchH = "all"
      ) %>%
      hot_cols(columnSorting = T) %>%
      hot_col("id", readOnly = T)
  })

  # upload

  observeEvent(input$input_roster, {
    df.player <- as_tibble(tbl(pool, "player"))
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
    color_color <- as.list(with(data, setNames(color, color)))
    output$upload_team <- renderRHandsontable({
      distinct(data, color, team) %>%
        rhandsontable(
          stretchH = "all",
          color_color = color_color
        ) %>%
        hot_col("color", renderer = renderer.color)
    })
    output$upload_roster <- renderRHandsontable({
      rhandsontable(
        data,
        stretchH = "all",
        color_color = color_color
      ) %>%
        hot_col("id", readOnly = T) %>%
        hot_col("color", readOnly = T, renderer = renderer.color) %>%
        hot_col("team", type = "dropdown", source = unique(data$team)) %>%
        hot_col("player", type = "dropdown", source = unique(df.player$name), strict = F, allowInvalid = T)
    })
    outputOptions(output, "upload_team", suspendWhenHidden = F)
    outputOptions(output, "upload_roster", suspendWhenHidden = F)
  })

  ## update based on input table changes

  lapply(c("upload_roster", "upload_team"), function(ele) {
    observeEvent(input[[ele]]$changes$changes, {
      coor <- input[[ele]]$changes$changes[[1]]
      req(coor[[3]] != coor[[4]])
      data_team <- hot_to_r(input$upload_team)
      data_roster <- hot_to_r(input$upload_roster)
      color_color <- as.list(with(data_team, setNames(color, color)))
      df.player <- as_tibble(tbl(pool, "player"))

      # note: team and roster share the same first two columns: [1] color & [2] team
      idx <- coor[[2]] + 1 # change from 0 to 1-based index
      key <- names(data_roster)[coor[[2]] + 1]
      if (ele == "upload_team" & (key == "color" | key == "team")) {
        idx <- coor[[2]] + 1
        data_roster[idx][data_roster[idx] == coor[[3]]] <- coor[[4]]
      } else if (ele == "upload_roster" & key == "player") {
        result <- pool::dbGetQuery(pool, "SELECT id FROM player WHERE name == ?;", params = coor[[4]])
        data_roster[coor[[1]] + 1, "id"] <- ifelse(nrow(result) > 0, result$id, NA_integer_)
      }

      data_roster$color <- with(data_team, setNames(color, team))[data_roster$team]

      output$upload_team <- renderRHandsontable({
        rhandsontable(
          data_team,
          stretchH = "all",
          color_color = color_color
        ) %>%
          hot_col("color", renderer = renderer.color)
      })
      output$upload_roster <- renderRHandsontable({
        rhandsontable(
          data_roster,
          stretchH = "all",
          color_color = color_color
        ) %>%
          hot_col("id", readOnly = T) %>%
          hot_col("color", readOnly = T, renderer = renderer.color) %>%
          hot_col("team", type = "dropdown", source = unique(data_team$team)) %>%
          hot_col("player", type = "dropdown", source = unique(df.player$name), strict = F, allowInvalid = T)
      })
    })
  })

  ## submit roster
  
  observeEvent(input$submit_roster, {
    data_team <- hot_to_r(input$upload_team)
    data_roster <- hot_to_r(input$upload_roster)

    tryCatch(
      pool::poolWithTransaction(pool, function(conn) {
        
        season_id <- insert_or_get_season_id(conn, input$input_year, input$input_season, input$input_session)
        
        df.team <- (
          data_team %>%
            rename(name = team) %>%
            mutate(id = NA, season = season_id)
        )
        df.team <- bind_cols(
          pool::dbGetQuery(
            conn = conn,
            glue::glue_sql("INSERT INTO team VALUES (:id, :season, :name, :color) RETURNING id;", .con = conn),
            params = df.team
          ),
          select(df.team, -id)
        )

        df.player <- (
          data_roster %>%
            filter(is.na(id)) %>%
            mutate(id = NA) %>%
            rename(name = player) %>%
            select(id, name)
        )
        df.player <- bind_cols(
          pool::dbGetQuery(
            conn = conn,
            glue::glue_sql("INSERT INTO player VALUES (:id, :name) RETURNING id;", .con = conn),
            params = df.player
          ),
          select(df.player, -id)
        )

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
          pool::dbGetQuery(
            conn = conn,
            glue::glue_sql("INSERT INTO roster VALUES (:id, :team, :player, :captain) RETURNING id;", .con = conn),
            params = df.roster
          ),
          select(df.roster, -id)
        )

        pool::dbBreak()
      }),
      error = function(e) print(e)
    )
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
        coor <- input[[ele]]$changes$changes[[1]]
        req(coor[[3]] != coor[[4]])
        key <- names(df.hot)[coor[[2]] + 1]
        values$changed[[ele]] <- c(values$changed[[ele]], df.hot$id[coor[[1]] + 1])
      })
    }
  )

  #   observeEvent(input$save, {
  #     lapply(
  #       c("team", "player", "roster"),
  #       function(ele) {
  #         # print(values$changed)
  #         df.hot <- hot_to_r(input[[ele]])
  #         deleted <- if (is.null(df.hot)) c() else setdiff(match()[[ele]]$id, df.hot$id)
  #         changed <- setdiff(values$changed[[ele]], deleted)
  #
  #         print(c("deleted", ele, deleted))
  #         print(c("changed", ele, changed))
  #
  #         # DELETE
  #         if (length(deleted) > 0) {
  #           n <- (
  #             DBI::dbExecute(conn, glue::glue_sql("DELETE FROM {`ele`} WHERE id IN (", str_c(deleted, collapse = ", "), ");", .con = conn))
  #           )
  #           print(c(n, "deleted"))
  #         }
  #
  #         # CREATE
  #         # new rows have missing id, so insert them and assign a new one
  #         idx <- which(is.na(df.hot$id))
  #         if (length(idx) > 0) {
  #           df.hot[idx, ]$id <- DBI::dbGetQuery(
  #             conn = conn,
  #             glue::glue_sql("INSERT INTO {`ele`} VALUES (", str_c(rep("?", ncol(df.hot)), collapse = ", "), ") RETURNING id;", .con = conn),
  #             params = unname(as.list(df.hot[idx, ]))
  #           )
  #         }
  #         idx <- c(idx, which(df.hot$id %in% changed))
  #
  #         # UPDATE
  #         # changed rows have an id to coordinate the update
  #         if (length(idx) > 0) {
  #           df.hot[idx, ] %>%
  #             mutate(across(everything(), \(ele) as.character(ele))) %>%
  #             pivot_longer(-id, values_drop_na = T) %>%
  #             # get the latest updated value
  #             group_by(id, name) %>%
  #             slice_tail(n = 1) %>%
  #             ungroup() %>%
  #             # generate/run SQL
  #             with(glue::glue_sql("UPDATE {`ele`} SET {`name`} = {value} WHERE id == {id};", .con = conn)) %>%
  #             lapply(function(ele) {
  #               res <- DBI::dbSendStatement(conn, statement = ele)
  #               status <- c(ele, DBI::dbHasCompleted(res), DBI::dbGetRowsAffected(res))
  #               DBI::dbClearResult(res)
  #               setNames(status, c("sql", "success", "n"))
  #             }) %>%
  #             bind_rows() %>%
  #             print()
  #         }
  #
  #         values$changed[[ele]] <- values$changed[[ele]][0]
  #       }
  #     )
  #   })
}
