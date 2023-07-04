library("baseballr")
library("dplyr")
library("tidyverse")
library("glue")

setwd('C:/Users/tyler/OneDrive/Coding Work Materials/ncaa_run_expectancies')

all_schools <- ncaa_school_id_lu(team_name = "")
d1_schools <- filter(all_schools, division == 1 & year == 2023)
all_team_ids <- sort(unique(d1_schools$team_id), decreasing = FALSE)
next_run <- all_team_ids[all_team_ids > 458]

boston <- filter(all_schools, team_name == 'Boston College')

#all_team_ids
bscore_vec <- c()

pbp_scrape <- function(team_id) {
  y <- ncaa_schedule_info(team_id, 2023)
  bx_urls <- y$game_info_url
  for (i in seq_along(bx_urls)) {
    url = unlist(bx_urls[i])
    if (!is.na(url)) {
      if (any(url==bscore_vec) == FALSE) {
        bscore_vec <<- append(bscore_vec,url)
      }
    }
  }
  print(glue("Done scraping {team_id}"))
  return("Finished")
}

for (team in next_run) {
  pbp_scrape(team)
}

write.csv(x = bscore_vec,
          file = "bscore_list.csv",
          row.names = FALSE)
