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
    if(input$tabMatchPanels == "record") {
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
              team = factor(team, levels = match$v_matchup$team),
              across(
                any_of(c("shooter", "assist1", "assist2", "player", "server", "goalie")),
                factor,
                levels = unique(sort(as.character(match$v_roster$player)))
              ),
              across(any_of(c("call")), factor, levels = levels(match$foul$call))
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
      data.frame(id = with(data$v_assist, unique(c(source_id, target_id)))) %>%
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
        team_color = data$v_point$color,
        player_color = as.list(with(data$v_roster, setNames(color, player)))
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
        team_color = data$v_penalty$color,
        player_color = as.list(with(data$v_roster, setNames(color, player)))
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
        team_color = data$v_shot$color,
        player_color = as.list(with(data$v_roster, setNames(color, player)))
      ) %>%
      hot_cols(columnSorting = T) %>%
      hot_col(c("team", "goalie"), renderer = renderer.color) %>%
      hot_validate_numeric("SH", min = 0) %>%
      hot_validate_numeric("period", min = 1)
  })

  output$roster <- renderRHandsontable({
    data <- match()
    data$v_roster %>%
      select(id, team, player, captain) %>%
      rhandsontable(
        stretchH = "all",
        rowHeaders = NULL,
        team_color = data$v_roster$color,
        player_color = as.list(with(data$v_roster, setNames(color, player)))
      ) %>%
      hot_cols(columnSorting = T) %>%
      hot_col("id", readOnly = T) %>%
      hot_col(c("team", "player"), renderer = renderer.color)
  })

  observeEvent(input$set_yss, {
    con <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")

    params <- as.integer(str_split_fixed(input$set_yss, "_", 3))

    output$coordinate <- renderText({
      glue::glue("<b>Session {params[3]} of the {names(seasons)[params[2] - min(seasons) + 1]} {params[1]} Season</b>")
    })

    v_roster <- (
      DBI::dbGetQuery(
        con,
        "SELECT * FROM v_roster WHERE year == ? AND season == ? AND session == ?;",
        params = params
      ) %>%
        mutate(
          label = if_else(is.na(alias), player, glue::glue("{player} ({alias})")),
          label = glue::glue("{player_id}. {label}"),
          captain = as.logical(as.integer(captain))
        )
    )

    team <- DBI::dbGetQuery(con, "SELECT id, name FROM team WHERE year == ? AND season == ? AND session == ?", params)
    choices.team <- with(v_roster, setNames(team_id, team))

    updateSelectInput(session, "home", choices = choices.team, selected = first(choices.team))
    updateSelectInput(session, "away", choices = choices.team, selected = last(choices.team))

    rink <- (
      DBI::dbReadTable(con, "rink") %>%
        select(id, name)
    )
    choices.rink <- with(rink, setNames(id, name))
    updateSelectInput(session, "rink", choices = choices.rink, selected = last(choices.rink))

    output$record_shot <- renderRHandsontable({
      data.frame(
        team = c("home", "away", "home", "away"),
        goalie = "",
        SH = 0,
        period = c(1, 2, 1, 2)
      ) %>%
        mutate(
          team = factor(team, levels = c("home", "away")),
          goalie = factor(goalie, levels = c("", v_roster$label)),
          across(c(SH, period), \(ele) as.integer(ele))
        ) %>%
        rhandsontable(height = 750, stretchH = "all") %>%
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
        EV = F,
        PP = F,
        SH = F,
        EN = F
      ) %>%
        mutate(
          across(c(goalie, assist1, assist2), \(ele) factor(ele, levels = c("", v_roster$label))),
          team = factor(team, levels = c("home", "away")),
          shooter = factor(shooter, levels = v_roster$label),
          period = as.integer(period),
        ) %>%
        rhandsontable(stretchH = "all") %>%
        hot_validate_numeric("SH", min = 0) %>%
        hot_validate_numeric("period", min = 1)
    })

    df.foul <- DBI::dbReadTable(con, "foul")

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
        scored = F
      ) %>%
        mutate(
          across(c(player, server, goalie), \(ele) factor(ele, levels = c("", v_roster$label))),
          across(c(duration, period), \(ele) as.integer(ele)),
          foul = factor(foul, levels = df.foul$call),
          team = factor(team, levels = c("home", "away")),
        ) %>%
        rhandsontable(stretchH = "all") %>%
        hot_validate_numeric(c("period", "duration"), min = 1)
    })

    DBI::dbDisconnect(con)
  })

  # team

  output$team <- renderRHandsontable({
    con <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")

    result <- (
      DBI::dbReadTable(con, "team") %>%
        rhandsontable(
          rowHeaders = NULL,
          stretchH = "all"
        ) %>%
        hot_cols(columnSorting = T) %>%
        hot_col("id", readOnly = T)
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
    template <- str_squish("
      <span class='toggle'>
        <label>
          <input id='fix_{id}' name='fix_{row}' type='radio' onclick='console.log(this.id);'>{label}
        </label>
      </span>
    ")
    output$temp_roster <- renderRHandsontable({
      con <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")
      df.player <- (
        DBI::dbReadTable(con, "player") %>%
          mutate(label = if_else(is.na(alias), name, glue::glue("{name} ({alias})")))
      )
      DBI::dbDisconnect(con)

      data <- (
        read_tsv(input$input_roster$datapath, col_types = cols(.default = "c")) %>%
          pivot_longer(cols = everything(), names_to = "team", values_to = "player", values_drop_na = T) %>%
          separate("team", c("team", "color"), ":") %>%
          mutate(
            color = tolower(color),
            captain = str_detect(player, "(, ?C| ?\\(C\\))$"),
            goalie = str_detect(player, "(, ?G| ?\\(G\\))$"),
            player = str_remove(player, "(, ?[CG]| ?\\([CG]\\))$")
          ) %>%
          left_join(df.player, by = join_by(player == label)) %>%
          rownames_to_column("row") %>%
          select(-name) %>%
          arrange(color, player)
      )
      left_join(
        data,
        filter(data, is.na(id)) %>%
          mutate(
            create = glue::glue(template, id = 0, row = row, label = ""),
            choose = (
              apply(., 1, function(ele) {
                agrep(ele[["player"]], df.player$name, max.distance = list(all = 5), ignore.case = T) %>%
                  head(3) %>%
                  df.player[., ] %>%
                  with(glue::glue(template)) %>%
                  str_c(collapse = "")
              })
            )
          ) %>%
          select(player, create, choose),
        by = join_by(player)
      ) %>%
        rhandsontable(
          allowedTags = "<span><label><input>",
          stretchH = "all"
        ) %>%
        hot_col(c("create", "choose"), renderer = htmlwidgets::JS("safeHtmlRenderer"))
    })
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
