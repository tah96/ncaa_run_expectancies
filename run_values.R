## We need to take the PBP scrape, a RE Matrix to derive run values for each event

## Read in the necessary libararies
library("dplyr")
library("tidyverse")

## Set working directory and read in PBP CSV, and RE Matrix
setwd('C:/Users/tyler/OneDrive/Coding Work Materials/ncaa_run_expectancies')
pbp = read.csv('single_pbp.csv',header = TRUE)
re_matrix = read.csv('RE_Matrix.csv',header = TRUE)

View(pbp)

## Join PBP to RE Matrix. Need to know RE before and after event so two joins

pbp_re_merge <- pbp %>%
  inner_join(re_matrix, 
              by=c('base_cd_before'='base_cd',
                   'outs_before'='outs')
  ) %>%
  left_join(re_matrix,
              by=c('base_cd_after'='base_cd', 
                   'outs_after'='outs')
  ) %>%
  mutate(run_value = ifelse(!is.na(base_cd_after),run_expectancy.y,0) - run_expectancy.x + (result_runs - current_runs)) %>%
  mutate(event_group = case_when (
    event_cd == 20 ~ 'Single',
    event_cd == 21 ~ 'Double',
    event_cd == 22 ~ 'Triple',
    event_cd == 23 ~ 'Home Run',
    event_cd == 14 ~ 'Walk',
    event_cd == 16 ~ 'HBP',
    event_cd %in% c(2,3,6,8,19) ~ 'Outs',
    ### Other to be modified ###
    TRUE ~ 'Other'
  ))

View(pbp_re_merge)

## Calculate non-scaled Run Value matrix

rv_matrix <-
  pbp_re_merge %>%
  group_by(event_group) %>%
  summarize(mean = mean(run_value)) %>%
  arrange(desc(event_group))

colnames(rv_matrix) <- c('event_group','avg_run_val')

View(rv_matrix)

unscaled_out_value <- rv_matrix$avg_run_val[3]

scaled_rv_values <- rv_matrix$avg_run_val - unscaled_out_value

scaled_rv_matrix <- rv_matrix %>%
  mutate(scale_run_val = scaled_rv_values) %>%
  select(-avg_run_val)

write.csv(x = scaled_rv_matrix,
          file = "SRV_Matrix.csv",
          row.names = FALSE)

## Testing

##testing <- filter(pbp_re_merge, event_cd == 20)

View(testing)
