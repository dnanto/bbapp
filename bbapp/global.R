library(shiny)
library(shinyTime)
library(DT)
library(rhandsontable)
library(tidyverse)

year <- 2023
season <- 24
con <- DBI::dbConnect(RSQLite::SQLite(), "stats.sdb")
df.match <- DBI::dbReadTable(con, "match")
df.roster <- DBI::dbReadTable(con, "roster")
df.team <- DBI::dbReadTable(con, "team")
DBI::dbDisconnect(con)

types <- c("EV", "PP", "SH", "EN")
calls <- c(
  "Checking",
  "Cross-checking",
  "Delay of Game",
  "Elbowing",
  "Game Misconduct",
  "Goaltender Interference",
  "High Sticking",
  "Holding",
  "Holding the Stick",
  "Hooking",
  "Interference",
  "Misconduct",
  "Misconduct - Abuse of Officials",
  "Roughing",
  "Slashing",
  "Throwing Equipment",
  "Throwing Stick",
  "Too Many Men on the Ice",
  "Tripping",
  "Unsportsmanlike Conduct",
  "Unsportsmanlike Conduct - Diving",
  "Unsportsmanlike Conduct - Taunting"
)

ls.roster <- (
  distinct(df.match, year, season, team1, team2) %>%
    pivot_longer(-c(year, season), values_to = "team", names_to = NULL) %>%
    distinct() %>%
    inner_join(df.roster, by = join_by(team)) %>%
    left_join(df.team, by = join_by(team == id)) %>%
    split(list(.$year, .$season))
)

# https://www.loc.gov/standards/datetime/
seasons <- setNames(21:24, c("Spring", "Summer", "Autumn", "Winter"))

df.ui <- (
  read_tsv("ui.tsv") %>% 
    split(.$section)
)