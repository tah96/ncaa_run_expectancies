# NCAA Sabermetrics
**Description** - This project aims to calculate popular sabermetrics at the NCAA Division I level for the purpose of evaluating batter performance. 

**Project Status** - In Development (~80% Complete)

**<ins>Requirements</ins>**
  1. Programming Languages - R
  2. Packages - baseballr, dplyr, tidyverse, rvest

**<ins>Sources / Helpful Links</ins>**
  1. NCAA PBP Parsing - Code in *Play_By_Play_Scrape.R* leverages much of the parsing script created by Dave Miller (*NCAA_base_pbp_parser.R*) in his Github project [NCAA-baseball](https://github.com/davmiller/NCAA-baseball/tree/master)
  2. Collegiate Linear Weights - Project [Collegiate Linear Weights](https://rfrey22.medium.com/collegiate-linear-weights-f0237cf40451) by Robert Frey published on medium.com. Provided guidance on the concept of linear weights.
  3. Fangraphs - On fangraphs.com the "Guts!" section is helpful for understanding the calculations behind sabermetrics. [wOba Article](https://library.fangraphs.com/offense/woba/) user for guidance

## Step-By-Step Guide ##
Below is a  listing of all R scripts used. Predecessors will be listed in parenthesis () when necessary. 🚧 icon means a script has not been created
  1. ncaa-pbp-scrape
  2. Play_By_Play_Scrape (1)
  3. run_expectancy (2)
  4. run_values (3)
  5. indiv_stat_scrape
  6. league_stat_scrape 🚧🚧🚧
  7. sabermetric_calcs (4,5,6) 🚧🚧🚧
