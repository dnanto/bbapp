library(shiny)
library(rhandsontable)
library(visNetwork)
library(tidyverse)

# https://www.loc.gov/standards/datetime/
seasons <- setNames(21:24, c("Spring", "Summer", "Autumn", "Winter"))
types <- c("EV", "PP", "SH", "EN")

df.ui <- (
  read_tsv("ui.tsv") %>% 
    split(.$section)
)

df.colors <- (
  colors() %>% 
    lapply(\(ele) c(color = ele, t(col2rgb(ele))[1,])) %>% 
    bind_rows() %>%
    column_to_rownames(var = "color") %>%
    mutate(across(everything(), as.integer))
)

hex_to_color <- Vectorize(
  \(ele) {
    str_match(ele, "^#(?<r>[A-F0-9]{2})(?<g>[A-F0-9]{2})(?<b>[A-F0-9]{2})([A-F0-9]{2})?$") %>% 
      .[2:4] %>% 
      sapply(\(ele) strtoi(str_c("0x", ele))) %>%
      {which.min(sqrt(rowSums(sweep(df.colors, 2, .)^2)))}
  },
  USE.NAMES = F
)

renderer.team <- (
  "
    function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      td.style.borderStyle = 'dashed';
      td.style.borderWidth = '2px';
      if (instance.params) {
        colors = instance.params.colors;
        colors = colors instanceof Array ? colors : [colors];
        td.style.borderColor = colors[row];
      }
      return td;
    }
  "
)

validator.time <- (
  "
    function (value, callback) {
      setTimeout(function() {
        callback(/^[0-9]{2}:[0-9]{2}\\.[0-9]{3}$/.test(value));
      }, 1000);
    }
  "
)