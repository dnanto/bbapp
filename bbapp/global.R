library(shiny)
library(DT)
library(tidyverse)

df.ui <- (
  read_tsv("ui.tsv") %>% 
    split(.$section)
)
