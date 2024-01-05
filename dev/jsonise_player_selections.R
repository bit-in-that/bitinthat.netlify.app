library(arrow)
library(dplyr)
library(purrr)
library(jsonlite)

player_selections_long <- read_parquet("https://github.com/bit-in-that/data-automation/raw/main/afl_fantasy/data/processed/2024/player_selections_long.parquet")

all_player_ids <- unique(player_selections_long$id)

player_selections_modified <- player_selections_long |> 
  mutate(
    direction = if_else(owned_by_adjusted_diff > 0, "Daily Increase", "Daily Decrease"),
    full_name = paste(first_name, last_name),
    snapshot_date = as.Date(snapshot_time, tz = "Australia/Sydney"),
    snapshot_date_int = as.integer(snapshot_date),
    text_snapshot = paste0("Snapshot Date: ", format(snapshot_date, format = "%Y-%m-%d"), "<br />Owned By  % (Adjusted): ", round(owned_by_adjusted, 2), "<br />Owned By  % (Official): ", round(owned_by, 2), "<br /># Coaches: ", fantasy_coaches, "(", round(100*completion_percentage, 1), "% complete)"),
    text_daily_change = paste0("Snapshot Date: ", format(snapshot_date, format = "%Y-%m-%d"), "<br />Daily Movement In Adjusted Ownership: ", round(owned_by_adjusted_diff, 2), "<br /># Coaches: ", fantasy_coaches, " (", round(100*completion_percentage, 1), "% complete)"),
    )

owned_by_adjusted_plot_data <- all_player_ids |> 
  `names<-`(all_player_ids) |> 
  map(~ {
    individual_player_data <- player_selections_modified |> 
      filter(.x == id)
    
    player_name <- individual_player_data$full_name[1]
    
    daily_increase_data <- individual_player_data |> 
      filter(
        direction == "Daily Increase"
      )
    
    daily_decrease_data <- individual_player_data |> 
      filter(
        direction == "Daily Decrease"
      )
    
    max_ownership <- max(individual_player_data$owned_by_adjusted)
    
    if(max_ownership>=50) {
      tick_interval <- 10
    } else if(max_ownership>=25){
      tick_interval <- 5
    } else if(max_ownership>=2.5){
      tick_interval <- 1
    } else {
      tick_interval <- 0.1
    }
    
    
    list(
      full_name = player_name,
      tick_interval = tick_interval,
      daily_increase_x = daily_increase_data$snapshot_date_int,
      daily_increase_y = daily_increase_data$owned_by_adjusted_diff,
      daily_increase_text = daily_increase_data$text_daily_change,
      daily_decrease_x = daily_decrease_data$snapshot_date_int,
      daily_decrease_y = daily_decrease_data$owned_by_adjusted_diff,
      daily_decrease_y_abs = -daily_decrease_data$owned_by_adjusted_diff,
      daily_decrease_text = daily_decrease_data$text_daily_change,
      snapshot_x = individual_player_data$snapshot_date_int,
      snapshot_y = individual_player_data$owned_by_adjusted,
      snapshot_text = individual_player_data$text_snapshot
    )
    })

write_json(owned_by_adjusted_plot_data, "fantasy/afl/json_data/owned_by_adjusted_plot_data.json")
