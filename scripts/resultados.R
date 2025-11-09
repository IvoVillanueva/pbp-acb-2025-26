source("scripts/helpers.R")

# Create data directory if it doesn't exist
if (!dir.exists("data")) dir.create("data")


ids <- read_csv("https://raw.githubusercontent.com/IvoVillanueva/pbp-acb-2025-26/refs/heads/main/calendario/ids_df.csv")


id <- ids %>% 
  select(id, date = start_date) %>% 
  mutate(
    date = as.Date(as_datetime(date))
  ) %>%
  filter(
    date <=today()) %>% 
  pull(id)

id <- 5886
iddf <- function(id) {
  link <- paste0("https://api2.acb.com/api/v1/openapilive/Matches/matchesbymatchweeklite?idCompetition=1&idEdition=90&idMatchweek=", id)
  res1 <- GET(url = link, add_headers(.headers = headers))
  json_resp1 <- fromJSON(content(res1, "text"))
  matches <- pluck(json_resp1) %>%
    unnest(cols = c(competition, phase, local_team, visitor_team, arena, edition), names_sep = "_")


  return(matches)
}

resultados <- map_df(id, iddf)

resultados %>%
  select(
       abb_local = local_team_team_abbrev_name,
       score_local,
       abb_visitor = visitor_team_team_abbrev_name,
       score_visitor) 