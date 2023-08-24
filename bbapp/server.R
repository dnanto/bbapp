server <- shinyServer(function(input, output) {

  output$shot <- renderRHandsontable({
    fields <- df.ui$shot$field
    
    ncol <- length(fields)
    nrow <- 1
    result <- (
      matrix(ncol = ncol, nrow = nrow, dimnames = list(c(), fields)) %>%
        as_tibble() %>%
        mutate(across(everything(), as.character))
    )
    
    colors <- sort(unique(ls.roster[[paste(year, season, sep = ".")]]$color))
    players <- ls.roster[[paste(year, season, sep = ".")]]$player
    
    rhandsontable(result, width = 1000, height = 1000) %>%
      hot_col(col = "color", type = "dropdown", source = colors) %>%
      hot_col(col = "shot", type = "numeric") %>%
      hot_col(col = "goalie", type = "dropdown", source = players) %>%
      hot_col(col = "period", type = "numeric") %>%
      hot_validate_numeric(cols = "shot", min = 0) %>%
      hot_validate_numeric(cols = "period", min = 1, max = 4)
  })
  
  output$point <- renderRHandsontable({
    fields <- df.ui$point$field
    
    ncol <- length(fields)
    nrow <- 1
    result <- (
      matrix(ncol = ncol, nrow = nrow, dimnames = list(c(), fields)) %>%
        as_tibble() %>%
        mutate(across(everything(), as.character))
    )
    
    colors <- sort(unique(ls.roster[[paste(year, season, sep = ".")]]$color))
    players <- ls.roster[[paste(year, season, sep = ".")]]$player
    
    rhandsontable(result, width = 1000, height = 1000) %>%
      hot_col(col = "color", type = "dropdown", source = colors) %>%
      hot_col(col = c("shooter", "assist1", "assist2", "goalie"), type = "dropdown", source = players) %>%
      hot_col(col = "period", type = "numeric") %>%
      hot_col(col = c("min", "sec"), type = "numeric") %>%
      hot_col(col = types, type = "checkbox") %>%
      hot_validate_numeric(cols = "period", min = 1, max = 4) %>%
      hot_validate_numeric(cols = "min", min = 0) %>%
      hot_validate_numeric(cols = "sec", min = 0, max = 59.999)
  })

output$penalty <- renderRHandsontable({
  fields <- df.ui$penalty$field
  
  ncol <- length(fields)
  nrow <- 1
  result <- (
    matrix(ncol = ncol, nrow = nrow, dimnames = list(c(), fields)) %>%
      as_tibble() %>%
      mutate(across(everything(), as.character))
  )
  
  colors <- sort(unique(ls.roster[[paste(year, season, sep = ".")]]$color))
  players <- ls.roster[[paste(year, season, sep = ".")]]$player
  
  rhandsontable(result, width = 1000, height = 1000) %>%
    hot_col(col = "color", type = "dropdown", source = colors) %>%
    hot_col(col = c("player", "server", "goalie"), type = "dropdown", source = players) %>%
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
})