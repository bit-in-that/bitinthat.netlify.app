library(dplyr)
library(yaml)


source("https://github.com/bit-in-that/data-automation/raw/main/aflw_fantasy/modules/get_player_data_afw.R")
player_data <- get_player_data()

player_yaml <- player_data |> 
  mutate(
    img_url = paste0("https://aflwfantasy.afl/media/players/", id, "_500.webp"),
    player_url = paste0("https://dfsaustralia.com/aflw-fantasy-player-summary/?playerId=CD_I", id),
    yaml_list = pmap(list(full_name, squad_name, position, img_url, player_url), ~{
      list(
        name = ..1,
        href = ..5,
        image = ..4,
        categories = list(..2, ..3)
      )
    })
  ) |> 
  pull()

write_yaml(player_yaml, file = "players/players.yml")
