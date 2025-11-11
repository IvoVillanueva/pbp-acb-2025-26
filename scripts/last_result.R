source("scripts/helpers.R")

# Create data directory if it doesn't exist
if (!dir.exists("data")) dir.create("data")

# csv con los datos
ids <- read_csv("https://raw.githubusercontent.com/IvoVillanueva/pbp-acb-2025-26/refs/heads/main/calendario/ids_df.csv")

# filtro para las semanas
id <- ids %>%
  select(id, date = end_date) %>%
  mutate(
    date = as.Date(as_datetime(date))
  ) %>%
  filter(
    date <= today()
  ) %>%
  pull(id)

# funcion resultados
iddf <- function(id) {
  link <- paste0(Sys.getenv("API_RESULTADOS"), id)
  res1 <- GET(url = link, add_headers(.headers = headers))
  json_resp1 <- fromJSON(content(res1, "text"))
  matches <- pluck(json_resp1) %>%
    unnest(cols = c(
      competition, phase, local_team,
      visitor_team, arena, edition
    ), names_sep = "_")
}

resultados <- map_df(id, iddf)

resultados_jornada <- resultados %>%
  transmute(
    abb_local = local_team_team_abbrev_name,
    score_local,
    abb_visitor = visitor_team_team_abbrev_name,
    score_visitor,
    jornada = as.numeric(matchweek_number),
    dif_local = score_local - score_visitor,
    dif_visitor = score_visitor - score_local
  ) %>%
  filter(jornada == max(jornada)) %>%
  pivot_longer(
    cols = c(
      abb_local, dif_local,
      abb_visitor, dif_visitor
    ),
    names_to = c(".value", "type"),
    names_pattern = "(.*)_(local|visitor)"
  ) %>%
  select(abb, dif) %>%
  mutate(dif = ifelse(dif > 0, paste0("W +", dif), paste0("L ", dif)))

write.csv(resultados_jornada, "data/last_result.csv", row.names = FALSE)
