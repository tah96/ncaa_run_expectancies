## We need to take the PBP scrape and return a run expectancy matrix similar to
## https://rfrey22.medium.com/collegiate-linear-weights-f0237cf40451

## Read in the necessary libararies
library("dplyr")
library("tidyverse")

## Set working directory and read in PBP CSV
setwd('C:/Users/tyler/OneDrive/Coding Work Materials/ncaa_run_expectancies')
pbp = read.csv('single_pbp.csv',header = TRUE)

## Next steps
## Partition data by base-out state and AVERAGE runs til end of inning attributes

re_matrix <- pbp %>%
  mutate(runs_end_2 = end_half_inning_runs - current_runs) %>%
  group_by(base_cd_before,outs_before) %>%
  summarize(mean = mean(runs_end_2))

View(re_matrix)

## Test a base/out state to validate results. Can do manual calculations if small enough dataset

testing <- filter(pbp, base_cd_before == 5 & outs_before == 0)

View(testing)

## Write final results to a CSV file

write.csv(x = re_matrix,
          file = "RE_Matrix.csv",
          row.names = FALSE)

