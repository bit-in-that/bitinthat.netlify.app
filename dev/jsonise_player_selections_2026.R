library(arrow)
library(dplyr)
library(purrr)
library(stringr)
library(jsonlite)
library(scales)

player_data <- read_parquet("https://github.com/bit-in-that/data-automation/raw/main/2026/output/player_data.parquet")

player_labels <- player_data |> 
  transmute(
    id,
    # label = paste0(first_name, " ", last_name, " || ", position, " || $", comma(cost/1000), "K || ", team_name)
    label = paste0(first_name, " ", last_name, " || ", position, " || $", comma(price/1000), "K")
  )

player_selections_long <- read_parquet("https://github.com/bit-in-that/data-automation/raw/main/2026/output/player_selections_long.parquet")

all_player_ids <- unique(player_selections_long$id)

create_text_role_label <- function(snapshot_date, metric_value, metric_label, official_value, completion_value, completion_label = NULL) {
  if(is.null(completion_label)) {
    completion_label <- paste(str_remove(metric_label, fixed(" (Adjusted)")), "Role Completion")
  }
  
  paste0("Snapshot Date: ", format(snapshot_date, format = "%Y-%m-%d"), "<br /><b>", metric_label, ": ", round(metric_value, 2), "%</b><br />Normalised for: ~", round(100*completion_value, 2), "% ", completion_label, "<br />Official: ", official_value, "%")
  
}


player_selections_modified <- player_selections_long |> 
  left_join(player_data, by = "id") |> 
  mutate(
    direction = if_else(ownership_adjusted_diff > 0, "Daily Increase", "Daily Decrease"),
    full_name = paste(first_name, last_name),
    snapshot_date = as.Date(snapshot_time, tz = "Australia/Sydney") - 1,
    snapshot_date_int = as.integer(snapshot_date),
    ) |> 
  left_join(player_labels, by = "id")

ownership_adjusted_plot_data <- all_player_ids |> 
  `names<-`(all_player_ids) |> 
  map(~ {
    individual_player_data <- player_selections_modified |> 
      filter(.x == id)
    
    player_name <- individual_player_data$full_name[1]
    label <- individual_player_data$label[1]
    
    daily_increase_data <- individual_player_data |> 
      filter(
        direction == "Daily Increase"
      )
    
    daily_decrease_data <- individual_player_data |> 
      filter(
        direction == "Daily Decrease"
      )
    
    max_ownership <- max(individual_player_data$ownership_adjusted)
    
    
    if(max_ownership>=50) {
      tick_interval <- 10
    } else if(max_ownership>=25){
      tick_interval <- 5
    } else if(max_ownership>=2.5){
      tick_interval <- 1
    } else if(max_ownership>=0.25){
      tick_interval <- 0.1
    } else {
      tick_interval <- 0.01
    }


    
    list(
      full_name = player_name,
      label = label,
      tick_interval = tick_interval,
      daily_increase_x = daily_increase_data$snapshot_date_int,
      daily_increase_y = daily_increase_data$ownership_adjusted_diff,
      daily_increase_text = daily_increase_data$text_daily_change,
      daily_decrease_x = daily_decrease_data$snapshot_date_int,
      daily_decrease_y = daily_decrease_data$ownership_adjusted_diff,
      daily_decrease_y_abs = -daily_decrease_data$ownership_adjusted_diff,
      daily_decrease_text = daily_decrease_data$text_daily_change,
      snapshot_x = individual_player_data$snapshot_date_int,
      snapshot_y = individual_player_data$ownership_adjusted,
      snapshot_text = individual_player_data$text_snapshot
    )
  })

system.time({
  iwalk(ownership_adjusted_plot_data,~ write_json(.x, paste0("fantasy/2026/json_data/ownership_adjusted_plot_data/", .y, ".json")))
})

