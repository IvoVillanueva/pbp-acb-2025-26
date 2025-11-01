# Script to extract play-by-play data for the 2025-26 ACB season
# Description: This script retrieves and processes play-by-play data for all played matches in the 2025-26 ACB season.
# It saves the processed data as a CSV file in a "data" directory.

source("scripts/helpers.R")

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
    url = paste0(Sys.getenv("PBP"),
      partidos_2026, "&jvFilter=true"
    ),
    add_headers(.headers = headers)
  ), "text")) %>%
    pluck() %>%
    unnest(cols = c(competition, edition, license, team, type, statistics),
           names_sep = "_") %>%
    select(!c(id_subphase, id_round, license_media, team_media,
              contains("_date"))) %>%
    tibble()
}

# Map function to get all boxscores
pbp_df <- map_df(partidos_2026, boxscores_matches)

# write dataframe to .csv in a folder called "data/"
write.csv(pbp_df, "data/playbyplay_2025_26.csv", row.names = FALSE)
