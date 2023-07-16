install.packages("rvest")
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

ncaa_bat_stats = data.frame()
ncaa_pit_stats = data.frame()

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