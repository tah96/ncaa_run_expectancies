library(rvest)
library(dplyr)
library(tidyverse)

standard_links <- c('https://theacc.com/stats.aspx?path=baseball&year=2023#individual',
           'https://bigten.org/stats.aspx?path=baseball&year=2023#individual',
           'https://theamerican.org/stats.aspx?path=baseball&year=2023',
           'https://americaeast.com/stats.aspx?path=baseball&year=2023&',
           'https://atlantic10.com/stats.aspx?path=baseball&year=2023',
           'https://asunsports.org/stats.aspx?path=baseball&year=2023',
           'https://big12sports.com/stats.aspx?path=baseball&year=2023',
           'https://www.bigeast.com/stats.aspx?path=baseball&year=2023',
           'https://bigsouthsports.com/stats.aspx?path=baseball&year=2023',
           'https://bigwest.org/stats.aspx?path=baseball&year=2023',
           'https://caasports.com/stats.aspx?path=baseball&year=2023',
           'https://conferenceusa.com/stats.aspx?path=baseball&year=2023',
           'https://horizonleague.org/stats.aspx?path=baseball&year=2023',
           'https://ivyleague.com/stats.aspx?path=baseball&year=2023',
           'https://maacsports.com/stats.aspx?path=baseball&year=2023',
           'https://getsomemaction.com/stats.aspx?path=baseball&year=2023',
           'https://mvc-sports.com/stats.aspx?path=baseball&year=2023',
       ##    'https://themw.com/stats.aspx?path=baseball&year=2023',
           'https://northeastconference.org/stats.aspx?path=baseball&year=2023',
           'https://ovcsports.com/stats.aspx?path=baseball&year=2023',
           'https://patriotleague.org/stats.aspx?path=baseball&year=2023',
           'https://www.southland.org/stats.aspx?path=baseball&year=2023',
           'https://swac.org/stats.aspx?path=baseball&year=2023',
           'https://thesummitleague.org/stats.aspx?path=baseball&year=2023',
           'https://sunbeltsports.org/stats.aspx?path=baseball&year=2023',
           'https://wccsports.com/stats.aspx?path=baseball&year=2023')

pac12_links <- data.frame(links = c('https://thesundevils.com/sports/baseball/stats/2023',
                 'https://calbears.com/sports/baseball/stats/2023',
                 'https://arizonawildcats.com/sports/baseball/stats/2023',
                 'https://osubeavers.com/sports/baseball/stats/2023',
                 'https://goducks.com/sports/baseball/stats/2023',
                 'https://usctrojans.com/sports/baseball/stats/2023',
                 'https://uclabruins.com/sports/baseball/stats/2023',
                 'https://gostanford.com/sports/baseball/stats/2023',
                 'https://wsucougars.com/sports/baseball/stats/2023',
                 'https://gohuskies.com/sports/baseball/stats/2023',
                 'https://utahutes.com/sports/baseball/stats/2023'),
                  team_name = c('Arizona State', 'California', 'Arizona', 'Oregon State','Oregon','Southern Cal','UCLA','Stanford','Washington State','Washington','Utah'))

ncaa_bat_stats = data.frame()
ncaa_pit_stats = data.frame()
pac12_bat_stats = data.frame()
pac12_pit_stats = data.frame()

########## NCAA fetch ######################

for (i in standard_links) {
  page <- read_html(i)
  
  stats_bat <- page %>%
    html_node('#ind_hitting') %>%
    html_element("table") %>%
    html_table()
  
  stats_pit <- page %>%
    html_node('#ind_pitching') %>%
    html_element("table") %>%
    html_table()
  
  bat_df <- data.frame(stats_bat)
  pit_df <- data.frame(stats_pit)
  
  ncaa_bat_stats <<- rbind(ncaa_bat_stats,bat_df,use.names=TRUE,fill=TRUE)
  ncaa_pit_stats <<- rbind(ncaa_pit_stats,pit_df,use.names=TRUE,fill=TRUE)
  
  print(i)
}

##########################################

########### PAC-12 fetch ####################

for (i in 1:nrow(pac12_links)) {
    page <- read_html(pac12_links[i,1])
    
    stats_bat <- page %>%
      html_node('#individual-overall-batting') %>%
      html_element("table") %>%
      html_table()
    
    ### Will have to reorganize to use selenium in the future. To access pitching requires to click a button. This is not 
    ### possible with rvest
    
    ##stats_pit <- page %>%
    ##  html_node('#individual-overall-pitching') %>%
    ##  html_element("table") %>%
    ##  html_table()
    
    bat_df <- data.frame(stats_bat)
    bat_df$Team <- pac12_links[i,2]
    #pit_df <- data.frame(stats_pit)
    
    pac12_bat_stats <<- rbind(pac12_bat_stats,bat_df,use.names=TRUE,fill=TRUE)
    #pac12_pit_stats <<- rbind(pac12_pit_stats,pit_df,use.names=TRUE,fill=TRUE)
    
    print(i)
}

####################################################

### Non-norm conference stat fetch ################

setwd('C:/Users/tyler/OneDrive/Coding Work Materials/ncaa_run_expectancies')

nnorm_page <- read_html('https://southernconf_ftp.sidearmsports.com/custompages/socon/stats/baseball/2023/lgplyrs.htm')
##('https://a.espncdn.com/sec/baseball/2023/lgplyrs.htm')
socon_table <- nnorm_page %>%
  html_element("pre") %>%
  html_element("font") %>%
  html_text()

lets_see <- gsub(" ",",",socon_table)

while (grepl(",,",lets_see)) {
  lets_see <<- gsub(",,",",",lets_see)
}

writeLines(lets_see,con=file("socon_bat2.csv",open="w+"),sep="\n")
writeLines(lets_see,con=file("socon_pit.csv",open="w+"),sep="\n")

###########################################################

########### NCAA General Cleaning ##############################

ncaa_bat_copy <- ncaa_bat_stats

View(ncaa_bat_copy)

#ncaa_player_vec <- c()
ncaa_player_fname <- c()
ncaa_player_lname <- c()
ncaa_team_vec <- c()

for (j in 1:nrow(ncaa_bat_copy)){
  player_and_team <- strsplit(ncaa_bat_copy$Player[j],'[()]')
  player_pre_clean <- player_and_team[[1]][1]
  team <- player_and_team[[1]][2]
  ncaa_team_vec <<- append(ncaa_team_vec,team)
  if (grepl(",",player_pre_clean)==TRUE) {
    ncaa_player <- player_pre_clean
    player_name_split <- strsplit(ncaa_player, ',')
    ncaa_player_fname <<- append(ncaa_player_fname,toupper(trimws(player_name_split[[1]][2])))
    ncaa_player_lname <<- append(ncaa_player_lname,toupper(trimws(player_name_split[[1]][1])))
  }
  else {
    ncaa_player <- str_replace_all(str_replace(player_pre_clean,"\\s+", ","),",,",",")
    player_name_split <- strsplit(ncaa_player, ',')
    ncaa_player_fname <<- append(ncaa_player_fname,toupper(trimws(player_name_split[[1]][1])))
    ncaa_player_lname <<- append(ncaa_player_lname,toupper(trimws(player_name_split[[1]][2])))
  }
}

ncaa_bat_copy$FirstName = ncaa_player_fname
ncaa_bat_copy$LastName = ncaa_player_lname
ncaa_bat_copy$Team = ncaa_team_vec

ncaa_bat_clean <- ncaa_bat_copy %>%
  filter(Player != "TRUE") %>%
  separate(SB.ATT,c("SB","SB_ATT"),"-") %>%
  separate(GP.GS,c("GP","GS"),"-") %>%
  rename(SLG = SLG.,OBP = OB.) %>%
  select(FirstName,LastName,Team,GP,GS,AVG,OPS,AB,R,H,X2B,X3B,HR,RBI,TB,SLG,BB,HBP,SO,GDP,OBP,SF,SH,SB,SB_ATT)

View(ncaa_bat_clean)

################################################################

## Some manual processing needed prior to next steps. Totals < 1 min ##

############## PAC-12 Cleaning ######################

new_player_vec <- c()

for (player in pac12_bat_stats$Player) {
  first_clean <- str_replace_all(str_remove_all(player," "),"\r\n"," ")
  new_player <- substr(first_clean,start=1,stop=unlist(gregexpr(" ",first_clean))[1]-1)
  new_player_vec <<- append(new_player_vec,new_player)
}

pac12_bat_stats$Player = new_player_vec

pac12_bat_clean <- pac12_bat_stats %>%
  filter(Player != "") %>%
  separate(Player, c("FirstName", "LastName"), ",") %>%
  separate(SB.ATT,c("SB","SB_ATT"),"-") %>%
  separate(GP.GS,c("GP","GS"),"-") %>%
  rename(GP_GS = GP.GS,SLG = SLG.,OBP = OB.) %>%
  select(FirstName,LastName,Team,AVG,OPS,GP_GS,AB,R,H,X2B,X3B,HR,RBI,TB,SLG,BB,HBP,SO,GDP,OBP,SF,SH,SB,SB_ATT)

View(pac12_bat_clean)

write.csv(pac12_bat_clean,"pac12_bat_clean")

################################################################

sec_bat <- read.csv("sec_bat.csv")
sec_pit <- read.csv("sec_pit.csv")
socon_bat <- read.csv("socon_bat.csv")
socon_pit <- read.csv("socon_pit.csv")

View(socon_bat)
View(ncaa_bat_stats)
