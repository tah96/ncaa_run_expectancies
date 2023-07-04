library("baseballr")
library("dplyr")
library("tidyverse")

## Set working directory and read in Bscore CSV
setwd('C:/Users/tyler/OneDrive/Coding Work Materials/ncaa_run_expectancies')
blist = read.csv('bscore_list.csv',header = TRUE)

blist_vector <- blist$x

total_boxscores <- length(blist_vector)

pbp <- ncaa_pbp(game_info_url = blist_vector[1940])

## will use length(blist_vector)

for (i in 1941:total_boxscores) {
  game_pbp <- ncaa_pbp(game_info_url = blist_vector[i])
  pbp <<- rbind(pbp,game_pbp,use.names=TRUE,fill=TRUE)
  print(i)
}

View(pbp)

pbp_qa <- ncaa_pbp(game_info_url = blist_vector[149])

View(pbp_qa)

setwd('C:/Users/tyler/OneDrive/Coding Work Materials')

write.csv(x = pbp,
          file = "pbp_1939.csv",
          row.names = FALSE)
