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

# calcular victorias derrotas
victorias_derrotas <- resultados %>%
  select(
    abb_local = local_team_team_abbrev_name,
    score_local,
    abb_visitor = visitor_team_team_abbrev_name,
    score_visitor,
    jornada = matchweek_descriptor
  ) %>%
  mutate(
    ganador = if_else(score_local > score_visitor, abb_local, abb_visitor)
  ) %>%
  pivot_longer(
    cols = c(abb_local, abb_visitor),
    names_to = "rol",
    values_to = "abb"
  ) %>%
  mutate(
    resultado = if_else(abb == ganador, "W", "L") # W = victoria, L = derrota
  ) %>%
  select(abb, resultado, jornada)

# calcular rachas
rachas_actual <- victorias_derrotas %>%
  mutate(jornada = as.numeric(str_remove(jornada, "Jornada "))) %>%
  arrange(abb, jornada) %>%
  group_by(abb) %>%
  mutate(
    ultimo = last(resultado)
  ) %>%
  arrange(desc(jornada), .by_group = TRUE) %>%
  mutate(
    sigue_igual = resultado == ultimo,
    flag = cumall(sigue_igual)
  ) %>%
  summarise(
    tipo_racha = first(ultimo),
    racha_actual = sum(flag),
    .groups = "drop"
  ) %>%
  arrange(desc(tipo_racha), desc(racha_actual)) %>%
  mutate(win_lose = paste0(tipo_racha, racha_actual)) %>%
  select(abb, win_lose)

# write dataframe to .csv in a folder called "data/"
write.csv(rachas_actual, "data/racha_2025_26.csv", row.names = FALSE)
