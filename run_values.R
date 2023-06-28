## We need to take the PBP scrape, a RE Matrix to derive run values for each event

## Read in the necessary libararies
library("dplyr")
library("tidyverse")

## Set working directory and read in PBP CSV, and RE Matrix
setwd('C:/Users/tyler/OneDrive/Coding Work Materials/ncaa_run_expectancies')
pbp = read.csv('single_pbp.csv',header = TRUE)
re_matrix = read.csv('RE_Matrix.csv',header = TRUE)

## Join PBP to RE Matrix. Need to know RE before and after event so two joins

pbp_re_merge <- pbp %>%
  inner_join(re_matrix, 
              by=c('base_cd_before'='base_cd',
                   'outs_before'='outs')
  ) %>%
  inner_join(re_matrix,
              by=c('base_cd_after'='base_cd', 
                   'outs_after'='outs'))

View(pbp_re_merge)