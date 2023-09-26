library(dplyr)
library(yaml) 
library(scales) 


source("https://github.com/bit-in-that/data-automation/raw/main/aflw_fantasy/modules/get_player_data_afw.R")
player_data <- get_player_data()

player_yaml <- player_data |>
  arrange(desc(avgPoints)) |> 
  mutate(
    img_url = paste0("https://aflwfantasy.afl/media/players/", id, "_500.webp"),
    # player_url = paste0("https://dfsaustralia.com/aflw-fantasy-player-summary/?playerId=CD_I", id),
    player_url = paste0("profiles/", id, ".html"),
    yaml_list = pmap(list(full_name, squad_name, position, img_url, player_url, cost, squad_name, avgPoints), ~{
      list(
        title = ..1,
        path = ..5,
        # path = "../../index.html",
        image = ..4,
        description = paste0(number(..6/1000, big.mark = ","), "K | ", ..7, " | ", ..8),
        categories = list(..2, ..3)
      )
    })
  ) |> 
  pull()

write_yaml(player_yaml, file = "players/aflw/players.yaml")
