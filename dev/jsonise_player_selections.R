library(arrow)
library(dplyr)
library(purrr)
library(stringr)
library(jsonlite)

player_selections_long <- read_parquet("https://github.com/bit-in-that/data-automation/raw/main/afl_fantasy/data/processed/2024/player_selections_long.parquet")

all_player_ids <- unique(player_selections_long$id)

create_text_role_label <- function(snapshot_date, metric_value, metric_label, official_value, completion_value, completion_label = NULL) {
  if(is.null(completion_label)) {
    completion_label <- paste(str_remove(metric_label, fixed(" (Adjusted)")), "Role Completion")
  }
  
  paste0("Snapshot Date: ", format(snapshot_date, format = "%Y-%m-%d"), "<br />", metric_label, ": ", round(metric_value, 2), "% (normalised for: ~", round(100*completion_value, 2), "% ", completion_label, ")<br />Official: ", official_value, "%")
  
}


player_selections_modified <- player_selections_long |> 
  mutate(
    direction = if_else(owned_by_adjusted_diff > 0, "Daily Increase", "Daily Decrease"),
    full_name = paste(first_name, last_name),
    snapshot_date = as.Date(snapshot_time, tz = "Australia/Sydney") - 1,
    snapshot_date_int = as.integer(snapshot_date),
    captain_adjusted_text = create_text_role_label(snapshot_date, selections_captain_adjusted, "Captain (Adjusted)", selections_captain, completion_percentage_captain),
    captain_if_owned_text = create_text_role_label(snapshot_date, selections_captain_if_owned, "Captain (If Owned)", selections_captain, owned_by/100, "Ownership"),
    vice_captain_adjusted_text = create_text_role_label(snapshot_date, selections_vice_captain_adjusted, "Vice Captain (Adjusted)", selections_vice_captain, completion_percentage_vice_captain),
    vice_captain_if_owned_text = create_text_role_label(snapshot_date, selections_vice_captain_if_owned, "Vice Captain (If Owned)", selections_vice_captain, owned_by/100, "Ownership"),
    bench_adjusted_text = create_text_role_label(snapshot_date, selections_bench_adjusted, "Bench (Adjusted)", selections_bench, completion_percentage_bench),
    bench_if_owned_text = create_text_role_label(snapshot_date, selections_bench_if_owned, "Bench (If Owned)", selections_bench, owned_by/100, "Ownership"),
    emergency_adjusted_text = create_text_role_label(snapshot_date, selections_emergency_adjusted, "Emergency (Adjusted)", selections_emergency, completion_percentage_emergency),
    emergency_if_owned_text = create_text_role_label(snapshot_date, selections_emergency_if_owned, "Emergency (If Owned)", selections_emergency, owned_by/100, "Ownership"),
    emergency_if_bench_text = create_text_role_label(snapshot_date, selections_emergency_if_bench, "Emergency (If Bench)", selections_emergency, selections_bench/100, "Bench Selections"),
    any_captain_adjusted_text = create_text_role_label(snapshot_date, selections_any_captain_adjusted, "Any Captain (Adjusted)", selections_captain + selections_vice_captain, completion_percentage_captain),
    any_captain_if_owned_text = create_text_role_label(snapshot_date, selections_any_captain_if_owned, "Any Captain (If Owned)", selections_captain + selections_vice_captain, owned_by/100, "Ownership")
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
    
    max_ownership_if_owned <- max(individual_player_data$selections_any_captain_if_owned, individual_player_data$selections_bench_if_owned, individual_player_data$selections_emergency_if_bench)
    
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

    if(max_ownership_if_owned>=75) {
      tick_interval_if_owned <- 20
    } else if(max_ownership>=50){
      tick_interval_if_owned <- 10
    } else if(max_ownership>=25){
      tick_interval_if_owned <- 5
    } else if(max_ownership>=2.5){
      tick_interval_if_owned <- 1
    } else if(max_ownership>=0.25){
      tick_interval_if_owned <- 0.1
    } else {
      tick_interval_if_owned <- 0.01
    }
    
    list(
      full_name = player_name,
      tick_interval = tick_interval,
      tick_interval_if_owned = tick_interval_if_owned,
      daily_increase_x = daily_increase_data$snapshot_date_int,
      daily_increase_y = daily_increase_data$owned_by_adjusted_diff,
      daily_increase_text = daily_increase_data$text_daily_change,
      daily_decrease_x = daily_decrease_data$snapshot_date_int,
      daily_decrease_y = daily_decrease_data$owned_by_adjusted_diff,
      daily_decrease_y_abs = -daily_decrease_data$owned_by_adjusted_diff,
      daily_decrease_text = daily_decrease_data$text_daily_change,
      snapshot_x = individual_player_data$snapshot_date_int,
      snapshot_y = individual_player_data$owned_by_adjusted,
      snapshot_text = individual_player_data$text_snapshot,
      captain_adjusted_text = individual_player_data$captain_adjusted_text,
      captain_adjusted_y = individual_player_data$selections_captain_adjusted,
      captain_if_owned_text = individual_player_data$captain_if_owned_text,
      captain_if_owned_y = individual_player_data$selections_captain_if_owned,
      vice_captain_adjusted_text = individual_player_data$vice_captain_adjusted_text,
      vice_captain_adjusted_y = individual_player_data$selections_vice_captain_adjusted,
      vice_captain_if_owned_text = individual_player_data$vice_captain_if_owned_text,
      vice_captain_if_owned_y = individual_player_data$selections_vice_captain_if_owned,
      bench_adjusted_text = individual_player_data$bench_adjusted_text,
      bench_adjusted_y = individual_player_data$selections_bench_adjusted,
      bench_if_owned_text = individual_player_data$bench_if_owned_text,
      bench_if_owned_y = individual_player_data$selections_bench_if_owned,
      emergency_adjusted_text = individual_player_data$emergency_adjusted_text,
      emergency_adjusted_y = individual_player_data$selections_emergency_adjusted,
      emergency_if_owned_text = individual_player_data$emergency_if_owned_text,
      emergency_if_owned_y = individual_player_data$selections_emergency_if_owned,
      emergency_if_bench_text = individual_player_data$emergency_if_bench_text,
      emergency_if_bench_y = individual_player_data$selections_emergency_if_bench,
      any_captain_adjusted_text = individual_player_data$any_captain_adjusted_text,
      any_captain_adjusted_y = individual_player_data$selections_any_captain_adjusted,
      any_captain_if_owned_text = individual_player_data$any_captain_if_owned_text,
      any_captain_if_owned_y = individual_player_data$selections_any_captain_if_owned
    )
  })

write_json(owned_by_adjusted_plot_data, "fantasy/afl/json_data/owned_by_adjusted_plot_data.json")
