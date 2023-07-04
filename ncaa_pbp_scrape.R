library("baseballr")
library("dplyr")
library("tidyverse")

## Set working directory and read in Bscore CSV
setwd('C:/Users/tyler/OneDrive/Coding Work Materials/ncaa_run_expectancies')
blist = read.csv('bscore_list.csv',header = TRUE)

blist_vector <- blist$x


pbp <- ncaa_pbp(game_info_url = blist_vector[1])

## will use length(blist_vector)

for (i in 2:6) {
  game_pbp <- ncaa_pbp(game_info_url = blist_vector[i])
  pbp <<- rbind(pbp,game_pbp)
  print(i)
}
