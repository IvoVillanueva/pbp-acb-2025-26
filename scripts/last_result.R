source("scripts/helpers.R")

# Create data directory if it doesn't exist
if (!dir.exists("data")) dir.create("data")

resultados <- read_csv("https://raw.githubusercontent.com/IvoVillanueva/pbp-acb-2025-26/refs/heads/main/data/marcadores_2025_26.csv",
                          show_col_types = FALSE) 

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
      abb_local, score_local, dif_local,
      abb_visitor, score_visitor, dif_visitor
    ),
    names_to = c(".value", "type"),
    names_pattern = "(.*)_(local|visitor)"
  ) %>%
  mutate(id_partido = rep(1:(n() / 2), each = 2)) %>%
  group_by(id_partido) %>%
  mutate(
    rival = if_else(type == "local",
      paste0("vs. ", abb[type == "visitor"]),
      paste0("@. ", abb[type == "local"])
    )
  ) %>%
  ungroup() %>%
  mutate(rival = ifelse(dif > 0, paste0(rival, " (W +", dif, ")"),
    paste0(rival, " (L ", dif, ")")
  )) %>%
  select(abb, rival)

write.csv(resultados_jornada, "data/last_result.csv", row.names = FALSE)
