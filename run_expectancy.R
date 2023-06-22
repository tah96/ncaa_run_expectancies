## We need to take the PBP scrape and return a run expectancy matrix similar to
## https://rfrey22.medium.com/collegiate-linear-weights-f0237cf40451

## Read in the necessary libararies
library("dplyr")

## Set working directory and read in PBP CSV
setwd('C:/Users/tyler/OneDrive/Coding Work Materials/ncaa_run_expectancies')
pbp = read.csv('single_pbp.csv',header = TRUE)

## Next steps
## Partition data by base-out state and AVERAGE runs til end of inning attributes