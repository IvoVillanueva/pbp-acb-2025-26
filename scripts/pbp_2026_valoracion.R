# Script to add on-court players to play-by-play data for ACB 2025-26 season
library(tidyverse)


# Load cleaned play-by-play data for the 2025-26 ACB season
pbp_Df_clean26 <- read_csv("https://raw.githubusercontent.com/IvoVillanueva/pbp-acb-2025-26/refs/heads/main/data/acb_pbp_2026_clean.csv",
  show_col_types = FALSE
) %>%
  mutate(
    row_num = row_number(),
    .before = 1
  )

# Create substitution in and out dataframes
inn <- pbp_Df_clean26 %>%
  filter(type_normalized_description %in% c("Substitution - In")) %>%
  select(row_num2 = row_num, playerIn = license_licenseStr15, type_normalized_description)

out <- pbp_Df_clean26 %>%
  filter(type_normalized_description %in% c("Substitution - Out")) %>%
  select(row_num, playerOut = license_licenseStr15, type_normalized_description) %>%
  cbind(inn) %>%
  select(row_num, playerOut, playerIn)


# Add on-court players to play-by-play data
p <- pbp_Df_clean26 %>%
  group_by(id_match) %>%
  mutate(
    h1 = license_licenseStr15[6],
    h2 = license_licenseStr15[7],
    h3 = license_licenseStr15[8],
    h4 = license_licenseStr15[9],
    h5 = license_licenseStr15[10],
    a1 = license_licenseStr15[1],
    a2 = license_licenseStr15[2],
    a3 = license_licenseStr15[3],
    a4 = license_licenseStr15[4],
    a5 = license_licenseStr15[5]
  ) %>%
  left_join(out)


match <- unique(pbp_Df_clean26$id_match)

update_columns <- function(df) {
  # Iterar sobre cada fila del dataframe
  for (i in 1:nrow(df)) {
    # Verificar si playerOut y playerIn no son NA y coinciden con h1
    if (!is.na(df$playerOut[i]) && !is.na(df$playerIn[i]) && df$playerOut[i] == df$h1[i]) {
      # Actualizar h1 hacia abajo con playerIn
      df$h1[(i + 1):nrow(df)] <- df$playerIn[i]
    }
    # Verificar si playerOut y playerIn no son NA y coinciden con h2
    if (!is.na(df$playerOut[i]) && !is.na(df$playerIn[i]) && df$playerOut[i] == df$h2[i]) {
      # Actualizar h2 hacia abajo con playerIn
      df$h2[(i + 1):nrow(df)] <- df$playerIn[i]
    }
    if (!is.na(df$playerOut[i]) && !is.na(df$playerIn[i]) && df$playerOut[i] == df$h3[i]) {
      df$h3[(i + 1):nrow(df)] <- df$playerIn[i]
    }
    if (!is.na(df$playerOut[i]) && !is.na(df$playerIn[i]) && df$playerOut[i] == df$h4[i]) {
      df$h4[(i + 1):nrow(df)] <- df$playerIn[i]
    }
    if (!is.na(df$playerOut[i]) && !is.na(df$playerIn[i]) && df$playerOut[i] == df$h5[i]) {
      df$h5[(i + 1):nrow(df)] <- df$playerIn[i]
    }
    if (!is.na(df$playerOut[i]) && !is.na(df$playerIn[i]) && df$playerOut[i] == df$a1[i]) {
      df$a1[(i + 1):nrow(df)] <- df$playerIn[i]
    }
    if (!is.na(df$playerOut[i]) && !is.na(df$playerIn[i]) && df$playerOut[i] == df$a2[i]) {
      df$a2[(i + 1):nrow(df)] <- df$playerIn[i]
    }
    if (!is.na(df$playerOut[i]) && !is.na(df$playerIn[i]) && df$playerOut[i] == df$a3[i]) {
      df$a3[(i + 1):nrow(df)] <- df$playerIn[i]
    }
    if (!is.na(df$playerOut[i]) && !is.na(df$playerIn[i]) && df$playerOut[i] == df$a4[i]) {
      df$a4[(i + 1):nrow(df)] <- df$playerIn[i]
    }
    if (!is.na(df$playerOut[i]) && !is.na(df$playerIn[i]) && df$playerOut[i] == df$a5[i]) {
      df$a5[(i + 1):nrow(df)] <- df$playerIn[i]
    }
  }
  return(df)
}


players_oncourt <- function(match) {
  df <- p %>%
    filter(id_match == match) %>%
    arrange(period, desc(minute), desc(second))

  df_enhaced <- update_columns(df)

  return(df_enhaced)
}


# Apply the function to each match and combine the results
pbp_Df_clean26_valoracion <- map_df(match, players_oncourt)

# Save the enhanced play-by-play data with on-court players
write.csv(pbp_Df_clean26_valoracion, "data/pbp_2026_valoracion.csv", row.names = FALSE)
