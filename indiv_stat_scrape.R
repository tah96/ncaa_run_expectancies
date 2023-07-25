#install.packages("rvest")
library(rvest)

### Following conferences missing from below ###
### PAC-12, SEC, SOCON ###

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
           'https://themw.com/stats.aspx?path=baseball&year=2023',
           'https://northeastconference.org/stats.aspx?path=baseball&year=2023',
           'https://ovcsports.com/stats.aspx?path=baseball&year=2023',
           'https://patriotleague.org/stats.aspx?path=baseball&year=2023',
           'https://www.southland.org/stats.aspx?path=baseball&year=2023',
           'https://swac.org/stats.aspx?path=baseball&year=2023',
           'https://thesummitleague.org/stats.aspx?path=baseball&year=2023',
           'https://sunbeltsports.org/stats.aspx?path=baseball&year=2023',
           'https://wccsports.com/stats.aspx?path=baseball&year=2023')

pac12_links <- c('https://thesundevils.com/sports/baseball/stats/2023',
                 'https://calbears.com/sports/baseball/stats/2023',
                 'https://arizonawildcats.com/sports/baseball/stats/2023',
                 'https://osubeavers.com/sports/baseball/stats/2023',
                 'https://goducks.com/sports/baseball/stats/2023',
                 'https://usctrojans.com/sports/baseball/stats/2023',
                 'https://uclabruins.com/sports/baseball/stats/2023',
                 'https://gostanford.com/sports/baseball/stats/2023',
                 'https://wsucougars.com/sports/baseball/stats/2023',
                 'https://gohuskies.com/sports/baseball/stats/2023',
                 'https://utahutes.com/sports/baseball/stats/2023')

ncaa_bat_stats = data.frame()
ncaa_pit_stats = data.frame()
pac12_bat_stats = data.frame()
pac12_pit_stats = data.frame()

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
}

for (i in pac12_links) {
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
  
  pac12_bat_stats <<- rbind(pac12_bat_stats,bat_df,use.names=TRUE,fill=TRUE)
  pac12_pit_stats <<- rbind(pac12_pit_stats,pit_df,use.names=TRUE,fill=TRUE)
}

## Non-norm conference stat pages

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

## Some manual processing needed prior to next steps. Totals < 1 min ##

sec_bat <- read.csv("sec_bat.csv")
sec_pit <- read.csv("sec_pit.csv")
socon_bat <- read.csv("socon_bat.csv")
socon_pit <- read.csv("socon_pit.csv")
