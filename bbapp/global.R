library(shiny)
library(rhandsontable)
library(visNetwork)
library(tidyverse)

pool <- pool::dbPool(RSQLite::SQLite(), dbname = "stats.sdb")
pool::dbExecute(pool, "PRAGMA foreign_keys=on;")

rinks <- with(as_tibble(tbl(pool, "rink")), setNames(id, name))
# https://www.loc.gov/standards/datetime/
seasons <- setNames(21:24, c("Spring", "Summer", "Autumn", "Winter"))
types <- c("EV", "PP", "SH", "EN")

df.colors <- (
  colors() %>%
    lapply(\(ele) c(color = ele, t(col2rgb(ele))[1, ])) %>%
    bind_rows() %>%
    column_to_rownames(var = "color") %>%
    mutate(across(everything(), as.integer))
)

hex_to_color <- Vectorize(
  function(ele) {
    str_match(ele, "^#(?<r>[A-F0-9]{2})(?<g>[A-F0-9]{2})(?<b>[A-F0-9]{2})([A-F0-9]{2})?$") %>%
      .[2:4] %>%
      sapply(\(ele) strtoi(str_c("0x", ele))) %>%
      {
        which.min(sqrt(rowSums(sweep(df.colors, 2, .)^2)))
      }
  },
  USE.NAMES = F
)

renderer.color <- (
  "
    function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      key = instance.getColHeader()[col] + '_color';
      obj = instance.params
      if (value !== null && typeof obj === 'object' && obj !== null && key in obj) {
        color = obj[key];
        td.style.borderColor = color[value];
        td.style.borderStyle = 'dotted';
        td.style.borderWidth = '2px';
      }
      return td;
    }
  "
)

renderer.integer <- (
  "
    function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      td.innerHTML = value ? parseInt(value) : td.innerHTML;
      return td;
    }
  "
)

validator.time.stat <- (
  "
    function (value, callback) {
      setTimeout(function() {
        callback(!value || /^[0-9]{2}:[0-9]{2}\\.[0-9]{3}$/.test(value));
      }, 250);
    }
  "
)

validator.time.match <- (
  "
    function (value, callback) {
      setTimeout(function() {
        var result = /^[0-9]{2}:[0-9]{2}$/.test(value);
        if (result) {
          var tokens = value.split(':').map(e=>parseInt(e));
          result = ((0 <= tokens[0] && tokens[0] < 24) && (0 <= tokens[1] && tokens[1] < 60));
        } else {
          result = false;
        }
        callback(!value || result);
      }, 250);
    }
  "
)

onStop(\() pool::poolClose(pool))
