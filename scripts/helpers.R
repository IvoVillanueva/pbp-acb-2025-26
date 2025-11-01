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
