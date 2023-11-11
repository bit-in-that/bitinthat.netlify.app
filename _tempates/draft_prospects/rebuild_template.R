library(arrow)
library(dplyr)
library(stringr)
library(purrr)
library(fs)
library(quarto)

quarto_template <- readLines("_tempates/draft_prospects/template.qmd")

male_folder_path <- "players/afl/underage_profiles/"
female_folder_path <- "players/aflw/underage_profiles/"

combine_player_details <- read_parquet("https://github.com/bit-in-that/data-automation/raw/main/players/data/processed/combine_player_details.parquet")
combine_player_seasons <- read_parquet("https://github.com/bit-in-that/data-automation/raw/main/players/data/processed/combine_player_seasons.parquet")
combine_player_stats <- read_parquet("https://github.com/bit-in-that/data-automation/raw/main/players/data/processed/combine_player_stats.parquet")

season_data_table <- combine_player_seasons |> 
  select(-tier_short) |> 
  relocate(
    season, .before = "tier"
  ) |> 
  arrange(
    desc(season), desc(games_played)
  )

match_data_table <- combine_player_stats |> 
  relocate(
    match_time, .before = "tier"
  ) |> 
  mutate(
    comp_name = paste(comp_name, season)
  ) |> 
  select(
    -season, -player_first_name, -player_surname, -match_id, -(inside50s:handballs_received)
  ) |> 
  arrange(desc(match_time))

write_parquet(combine_player_details, "_tempates/draft_prospects/combine_player_details.parquet")
write_parquet(season_data_table, "_tempates/draft_prospects/season_data_table.parquet")
write_parquet(match_data_table, "_tempates/draft_prospects/match_data_table.parquet")


try({
  file_delete(dir_ls(male_folder_path))
  file_delete(dir_ls(female_folder_path))
  
  dir_create("players/afl/underage_profiles", recurse = TRUE)
  dir_create("players/aflw/underage_profiles", recurse = TRUE)

}, silent = TRUE)

print(dir("players/afl/", recursive = TRUE, include.dirs = TRUE))
print(dir("players/aflw/", recursive = TRUE, include.dirs = TRUE))
  
combine_player_details |>
  mutate(
    first_playerId = map_chr(playerIds, head, n = 1)
  ) |>
  head(1) |> 
with({
  for(index in seq_along(first_playerId)) {
    id = first_playerId[index]
    first_name = player_first_name[index]
    surname = player_surname[index]
    gender = gender[index]
  # pwalk(list(first_playerId, player_first_name, player_surname, gender), \(id, first_name, surname, gender){
    if(gender == "male") {
      folder_path <- male_folder_path
    } else {
      folder_path <- female_folder_path

    }

    quarto_template |>
      str_replace_all(fixed("||player_name||"), paste(first_name, surname)) |>
      str_replace_all(fixed("||player_first_name||"), first_name) |>
      str_replace_all(fixed("||player_surname||"), surname) |>
      writeLines(paste0(folder_path, id, ".qmd"))
  # })
  }
})

try({
  dir_delete("_freeze/players/afl/underage_profiles/")
  dir_delete("_freeze/players/aflw/underage_profiles/")
}, silent = TRUE)

print(dir("_freeze/players/afl/", recursive = TRUE, include.dirs = TRUE))
print(dir("_freeze/players/aflw/", recursive = TRUE, include.dirs = TRUE))

quarto_render("players/afl/underage_profiles")
quarto_render("players/aflw/underage_profiles")
