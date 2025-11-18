source("scripts/helpers.R")


# Create data directory if it doesn't exist
if (!dir.exists("data")) dir.create("data")


pbp %>%
  select(id_match, num_jornada,
    abb = team_team_abbrev_name, license_id, license_licenseStr15,
    license_licenseAbbrev, license_licenseNick, id_play = id_playbyplaytype,
    type_description, type_normalized_description, local, period, minute,
    second, crono, score_local, score_visitor, wall_clock
  ) %>%
  mutate(
    player_asist = ifelse(str_detect(type_description, "Asistencia"),
      license_licenseStr15, "0"
    ),
    player_shoot = ifelse(str_detect(type_description, "Asistencia"),
      lag(license_licenseStr15), "0"
    ),
    asist_type = ifelse(str_detect(type_description, "Asistencia"),
      lag(type_description), "0"
    ),
    recovered_block = ifelse(type_description == "Rebote Defensivo" &
                               lead(type_description == "Tapón"), 1, 0),
    player_points = case_when(
      type_normalized_description == "2-Point Shot Made" ~ 2,
      type_normalized_description == "Dunk" ~ 2,
      type_normalized_description == "3-Point Shot Made" ~ 3,
      type_normalized_description == "Free Throw Made" ~ 1,
      TRUE ~ 0
    ),
    msg_type = case_when(
      type_description %in% c(
        "Intento fallado de 2", "Intento fallado de 3",
        "Mate fuera"
      ) ~ 1,
      type_description %in% c("Canasta de 2", "Canasta de 3", "Mate") ~ 2,
      type_description %in% c("Intento fallado de 1", "Canasta de 1") ~ 3,
      TRUE ~ 0
    ),
    three_make = ifelse(type_normalized_description == "3-Point Shot Made",
      1, 0
    ),
    three_misses = ifelse(type_normalized_description == "3-Point Shot Missed",
      1, 0
    ),
    two_make = ifelse(type_normalized_description == "2-Point Shot Made",
      1, 0
    ),
    two_misses = ifelse(type_normalized_description == "2-Point Shot Missed",
      1, 0
    ),
    free_make = ifelse(type_normalized_description == "Free Throw Made",
      1, 0
    ),
    free_misses = ifelse(type_normalized_description == "Free Throw Missed",
      1, 0
    ),
    dunk = ifelse(type_normalized_description == "Dunk", 1, 0),
    dunk_misses = ifelse(type_normalized_description == "Missed Dunk",
      1, 0
    ),
    valoracion = case_when(
      type_normalized_description == "2-Point Shot Made" ~ 2,
      type_normalized_description == "3-Point Shot Made" ~ 3,
      type_normalized_description == "Free Throw Made" ~ 1,
      type_normalized_description == "Free Throw Missed" ~ -1,
      type_normalized_description == "2-Point Shot Missed" ~ -1,
      type_normalized_description == "3-Point Shot Missed" ~ -1,
      type_normalized_description == "Defensive Rebound" ~ 1,
      type_normalized_description == "Block Received" ~ -1,
      type_normalized_description == "Assist 3-Point Shot" ~ 1,
      type_normalized_description == "Block" ~ 1,
      type_normalized_description == "Unsportsmanlike 2FT" ~ -1,
      type_normalized_description == "Double Foul - No FT" ~ -1,
      type_normalized_description == "Foul No FT" ~ -1,
      type_normalized_description == "Offensive Rebound" ~ 1,
      type_normalized_description == "Assist 2-Point Shot" ~ 1,
      type_normalized_description == "Offensive Foul" ~ -1,
      type_normalized_description == "Foul Received" ~ 1,
      type_normalized_description == "Foul 2FT" ~ -1,
      type_normalized_description == "Turnover" ~ -1,
      type_normalized_description == "Steal" ~ 1,
      type_normalized_description == "Foul 1FT" ~ -1,
      type_normalized_description == "Foul 3FT" ~ -1,
      type_normalized_description == "Technical Foul 1FT" ~ -1,
      type_normalized_description == "Double Unsportsmanlike - No FT" ~ -1,
      type_normalized_description == "Assist Foul Received" ~ 1,
      type_normalized_description == "Missed Dunk" ~ -1,
      type_normalized_description == "Dunk" ~ 2,
      TRUE ~ 0
    ),
    reboundOff = ifelse(type_normalized_description == "Offensive Rebound",
      1, 0
    ),
    reboundDef = ifelse(type_normalized_description == "Defensive Rebound",
      1, 0
    ),
    rebounds = ifelse(type_normalized_description %in% c(
      "Defensive Rebound",
      "Offensive Rebound"
    ),
    1, 0
    ),
    asistencias = ifelse(str_detect(type_normalized_description, "Assist"),
      1, 0
    ),
    pts_diff = abs(c(score_local - score_visitor))
  ) %>%
  # write dataframe to .csv in a folder called "data/"
  write.csv("data/acb_pbp_2026_clean.csv", row.names = FALSE)
