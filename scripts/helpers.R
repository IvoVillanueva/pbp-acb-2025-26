# Librerias comunes
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(janitor)

# Cabeceras comunes para la API
headers <- c(
  "accept" = "application/json",
  "authorization" = Sys.getenv("ACB_TOKEN"),
  "x-apikey" = Sys.getenv("ACB_KEY")
)


pbp_template <- fromJSON(content(GET(
  url = paste0(Sys.getenv("PBP"), partidos_2026[1], "&jvFilter=true"),
  add_headers(.headers = headers)
), "text")) %>%
  pluck() %>%
  unnest(
    cols = c(competition, edition, license, team, type, statistics),
    names_sep = "_"
  ) %>%
  select(!c(
    id_subphase, id_round, license_media, team_media,
    contains("_date")
  )) %>%
  tibble() %>%
  slice(0)

pbp <- read_csv("https://raw.githubusercontent.com/IvoVillanueva/pbp-acb-2025-26/refs/heads/main/data/playbyplay_2025_26.csv",
                show_col_types = FALSE) 
 
