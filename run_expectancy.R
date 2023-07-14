## We need to take the PBP scrape and return a run expectancy matrix similar to
## https://rfrey22.medium.com/collegiate-linear-weights-f0237cf40451

## Read in the necessary libararies
library("dplyr")
library("tidyverse")

## Set working directory and read in PBP CSV
setwd('C:/Users/tyler/OneDrive/Coding Work Materials')
pbp = read.csv('parsed_pbp.csv',header = TRUE)

## Next steps

## Filter out non game-events 
## Below is necessary to avoid double counting

pbp_game_events <- pbp %>%
  filter(!event_cd %in% c(0,1))

View(pbp_game_events)

## Partition data by base-out state and AVERAGE runs til end of inning attributes

re_matrix <- pbp_game_events %>%
  group_by(base_cd_before,outs_before) %>%
  summarize(mean = mean(runs_to_end))

colnames(re_matrix) <- c('base_cd','outs','run_expectancy')

View(re_matrix)

## Test a base/out state to validate results. Can do manual calculations if small enough dataset

testing <- filter(pbp_game_events, base_cd_before == 7 & outs_before == 0)

View(testing)

## Write final results to a CSV file

setwd('C:/Users/tyler/OneDrive/Coding Work Materials/ncaa_run_expectancies')

write.csv(x = re_matrix,
          file = "RE_Matrix.csv",
          row.names = FALSE)
