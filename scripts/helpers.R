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

pbp <- read_csv("https://raw.githubusercontent.com/IvoVillanueva/pbp-acb-2025-26/refs/heads/main/data/playbyplay_2025_26.csv",
                show_col_types = FALSE) 
 