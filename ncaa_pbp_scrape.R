library("baseballr")
library("dplyr")
library("tidyverse")

setwd('C:/Users/tyler/OneDrive/Coding Work Materials/ncaa_run_expectancies')

all_schools <- ncaa_school_id_lu(team_name = "")
d1_schools <- filter(all_schools, division == 1 & year == 2023)
all_team_ids <- sort(unique(d1_schools$team_id), decreasing = FALSE)

all_team_ids

pbp_scrape <- function(team_id) {
  bscore_vec <- c()
  y <- ncaa_schedule_info(team_id, 2023)
  bx_urls <- y$game_info_url
  for (i in seq_along(bx_urls)) {
    url = unlist(bx_urls[i])
    if (any(url==bscore_vec) == FALSE) {
      bscore_vec <- append(bscore_vec,url)
    }
  }
  print(bscore_vec)
}

pbp_scrape(490)
