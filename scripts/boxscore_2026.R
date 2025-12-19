# Script to extract play-by-play data for the 2025-26 ACB season
# Description: This script retrieves and processes play-by-play data
# for all played matches in the 2025-26 ACB season.
# It saves the processed data as a CSV file in a "data" directory.

source("scripts/helpers.R")

# Create data directory if it doesn't exist
if (!dir.exists("data")) dir.create("data")

# Load match calendar for the 2025-26 ACB season
partidos_2026 <- read_csv("https://raw.githubusercontent.com/IvoVillanueva/pbp-acb-2025-26/refs/heads/main/data/marcadores_2025_26.csv",
                          show_col_types = FALSE) %>%
  pull(id)

# Function to get boxscore for a single match
boxscores_matches <- function(partidos_2026) {
  fromJSON(content(GET(
    url = paste0(
      Sys.getenv("API_URL"),
      partidos_2026
    ),
    add_headers(.headers = headers)
  ), "text")) %>%
    pluck() %>%
    unnest(
      cols = c(competition, license, local_team, visitor_team, edition),
      names_sep = "_"
    ) %>%
    mutate(
      abb = ifelse(is_local == FALSE, visitor_team_team_abbrev_name,
        local_team_team_abbrev_name
      )
    ) %>%
    mutate(
      fecha = calendario %>%
        filter(id == partidos_2026) %>%
        mutate(date = as.Date(as_datetime(date))) %>%
        pull(date), .before = 1,
      num_jornada = calendario %>%
        filter(id == partidos_2026) %>%
        pull(matchweek_number)
    ) %>%
    select(where(~ !is.list(.))) %>%
    clean_names() %>%
    filter(!is.na(license_id)) %>%
    group_by(id) %>%
    arrange(-is_local) %>%
    ungroup()
}

# Map function to get all boxscores
stats_boxscores_df <- map_df(partidos_2026, boxscores_matches)

# write dataframe to .csv in a folder called "data/"
write.csv(stats_boxscores_df, "data/boxscores_2025_26.csv", row.names = FALSE)
