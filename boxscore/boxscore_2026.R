# Description: This script retrieves and processes boxscore data for all played matches in the 2025-26 ACB season.
# It saves the processed data as a CSV file in a "data" directory.
# Load necessary libraries
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(janitor)

# Define headers for API requests
headers <- c(
  "accept" = "application/json",
  "authorization" = "H4sIAAAAAAAAA32Ry3aqMBSG36iLi7qOw4oFk9PEQyohZEYilkCirIMF5OkbOmiVQUdZ+/bv798pbrAUkVR7BUEyAhcr0IIzWcoArEDdMBrA9VNxg5BPiQqp1wDGnEGTp/009MIZD4sd0dIsSxFMw7ATUaL2ulWxN7jZGV2poTegehuvrzKiTp7iRniL7zxhoZuzTZl5uhPKTTkD055Q+qTkkR6Pka4y9qNDTDjyu/hg6zxdVnkajq+UazmZ0Bstz7gT5j5HGhmtx4MPa9s/34dyxvUj41LL22OdMOxkqdXxSZN5d9r10BwNDS2v5Vh/zPg8ntD6W9OhwzHVvfDnPU033ZFHL495uinlmTTcsv/q5Yshmc+2GdNB8fXWs32wK3a4Ej7UMx8JT13LBy3LvUdsNXDDjf2P2X3YW6+EgQZUlwEf6gXeIgdvk4W9my52z2pfAReNcY8q1KPte4+CXtn/cub9p/jJef7j/Xvf/j150Sag6C1ctS41qVe3p6Fml5U4gf/xdecli8sn/TA0Eb8CAAA=",
  "x-apikey" = "0dd94928-6f57-4c08-a3bd-b1b2f092976"
)

# Create data directory if it doesn't exist
if (!dir.exists("data")) dir.create("data")

# Load match calendar for the 2025-26 ACB season
calendario <- read_csv("https://raw.githubusercontent.com/IvoVillanueva/pbp-acb-2025-26/refs/heads/main/calendario/calendario25_26.csv", show_col_types = FALSE)

# Filter matches that have already been played
partidos_2026 <- calendario %>%
  select(id, matchweek_number, date, time) %>%
  mutate(
    date = as.Date(as_datetime(date)),
    time = hms::hms(time)
  ) %>%
  filter(date < today()) %>%
  pull(id)

# Function to get boxscore for a single match
boxscores_matches <- function(partidos_2026) {
  json_resids <- fromJSON(content(GET(
    url = paste0(
      "https://api2.acb.com/api/v1/openapilive/Boxscore/playermatchstatistics?idMatch=",
      partidos_2026
    ),
    add_headers(.headers = headers)
  ), "text"))

  boxscores <- pluck(json_resids) %>%
    unnest(
      cols = c(competition, license, local_team, visitor_team, edition),
      names_sep = "_"
    ) %>%
    mutate(abb = ifelse(is_local == FALSE, visitor_team_team_abbrev_name,
      local_team_team_abbrev_name
    )) %>%
    select(where(~ !is.list(.))) %>%
    clean_names() 
}

# Map function to get all boxscores
stats_boxscores_df <- map_df(partidos_2026, boxscores_matches)

# write dataframe to .csv in a folder called "data/"
write.csv(stats_boxscores_df, "data/boxscores_2025_26.csv", row.names = FALSE)
