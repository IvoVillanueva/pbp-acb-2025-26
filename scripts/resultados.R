source("scripts/helpers.R")

# Create data directory if it doesn't exist
if (!dir.exists("data")) dir.create("data")


ids <- "https://raw.githubusercontent.com/IvoVillanueva/pbp-acb-2025-26/refs/heads/main/data/id_week.csv"

resids <- GET(url = ids, add_headers(.headers = headers))
json_resids <- fromJSON(content(resids, "text"))
ids_df <- json_resids %>%
  tibble() %>%
  unnest(cols = c(competition, edition)) |>
  arrange(num_matchweek)

id <- ids_df$id




# Load match calendar for the 2025-26 ACB season
calendario <- read_csv(
  "https://raw.githubusercontent.com/IvoVillanueva/pbp-acb-2025-26/refs/heads/main/calendario/calendario25_26.csv",
  show_col_types = FALSE
)

# Filter matches that have already been played
partidos_2026 <- calendario %>%
  select(id, matchweek_number, date, time) %>%
  mutate(
    date = as.Date(as_datetime(date)),
    time = hms::hms(time)
  ) %>%
  filter(
    date < today() |
      (date == today() & time < hms::as_hms(format(Sys.time(), "%H:%M:%S")))
  ) %>%
  pull(id)

id <- 104505

iddf <- function(id) {
  link <- paste0("https://api2.acb.com/api/v1/openapilive/Matches/matchesbymatchweeklite?idCompetition=1&idEdition=90&idMatchweek=", id)
  res1 <- GET(url = link, add_headers(.headers = headers))
  json_resp1 <- fromJSON(content(res1, "text"))
  matches <- pluck(json_resp1) %>%
    unnest(cols = c(competition, phase, local_team, visitor_team, arena, edition), names_sep = "_")


  return(matches)
}

resultados <- map_df(id, iddf)
