library(arrow)
library(dplyr)
library(reactable)
top_10000_lineups <- read_parquet("https://github.com/bit-in-that/data-automation/raw/main/aflw_fantasy/data/raw/top_10000_lineups.parquet")

top_10000_lineups |> 
  group_by(overall_rank) |> 
  summarise(
    no_captain = sum(is_captain) == 0,
    no_vice_captain = sum(is_vice_captain) == 0,
    .groups = "drop"
  ) |>
  group_by(no_captain, no_vice_captain) |> 
  summarise(
    top_10000 = sum(overall_rank <= 10000)/10000,
    top_5000 = sum(overall_rank <= 5000)/5000,
    top_2000 = sum(overall_rank <= 2000)/2000,
    top_1000 = sum(overall_rank <= 1000)/1000,
    top_100 = sum(overall_rank <= 100)/100,
    .groups = "drop"
  ) |> 
  reactable(
    columns = list(
      no_captain = colDef(
        name = "Captain Set",
        cell = function(cell_value) {
          if(cell_value) {
            "❌"
          } else {
            "✅"
          }
        }
      ),
      no_vice_captain = colDef(
        name = "VC Set",
        cell = function(cell_value) {
          if(cell_value) {
            "❌"
          } else {
            "✅"
          }
        }
      ),
      top_10000 = colDef(name = "10K", format = colFormat(percent = TRUE)),
      top_5000 = colDef(name = "5K", format = colFormat(percent = TRUE)),
      top_2000 = colDef(name = "2K", format = colFormat(percent = TRUE)),
      top_1000 = colDef(name = "1K", format = colFormat(percent = TRUE)),
      top_100 = colDef(name = "100🧢", format = colFormat(percent = TRUE))
    ),
    columnGroups = list(
      colGroup("Proportion of coaches in the top...", 
               c("top_10000", "top_5000", "top_2000", "top_1000", "top_100"))
    )
  )
  

