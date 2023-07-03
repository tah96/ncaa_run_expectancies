library("baseballr")
library("dplyr")
library("tidyverse")

setwd('C:/Users/tyler/OneDrive/Coding Work Materials/ncaa_run_expectancies')

all_schools <- ncaa_school_id_lu(team_name = "")
d1_schools <- filter(all_schools, division == 1 & year == 2023)
all_team_ids <- sort(unique(d1_schools$team_id), decreasing = FALSE)
nc_state_test <- filter(all_schools,team_name == 'NC State')

## 490 is NC State's team_id
### NEEDS ####
# 1) Start of Inning position
# 2) End of Inning position
# 3) Previous Base-Out State
# 4) Result
# 5) New Base-Out State
# 6) Runs scored til end of inning

## Grabbing the schedule info for NC State for 2023

x <- ncaa_schedule_info(490, 2023)

## Using first available game of season only for testing only
single_bscore <- x$game_info_url[1]
single_pbp <- ncaa_pbp(game_info_url = single_bscore)
print(single_bscore)

## Removing summary stats that are optional at the end of each inning to get rid of noise

summary_stats <- c('R:', 'H:', 'LOB:')
single_pbp <- filter(single_pbp,!grepl(paste(summary_stats, collapse = '|'),description))

## Attributes useful for other calculations

single_pbp <- single_pbp %>%
  mutate(inning_half = case_when(inning_top_bot == 'top' ~ ((strtoi(inning) * 2) - 1), # in this case, set WL to 1
                        TRUE  ~ (strtoi(inning) * 2)),
         result_runs = strtoi(sub("\\-.*","",score)) + strtoi(sub("*.\\-","",score)),
         current_runs = lag(result_runs,n=1,default=0),
         row_id = row_number()
  )

### Additional Cleaning based on each half-inning. Top inning, end of inning is important for utilizing Dav Miller's R Code
single_pbp_partial_clean <- single_pbp %>% 
  group_by(inning_half) %>%

  mutate(
    top_inning_flag = ifelse(min(which(single_pbp$inning_half == inning_half))==row_id,1,0),
    end_inning_flag = ifelse(max(which(single_pbp$inning_half == inning_half))==row_id,1,0),
    end_half_inning_runs = max(result_runs)
    ) %>% 
  ungroup()

## Some functions

stripwhite <- function(x) gsub("\\s*$", "", gsub("^\\s*", "", x))

strip_punc <- function(x){ 
  x=stripwhite(x)
  x=ifelse(str_sub(x,-1)=='.',gsub("\\.", "", x),x)
  return(x)}





##########################################################
##########################################################
##########################################################
## !! EVERYTHING FROM THIS POINT FORWARD IS CODE COPIED FROM ##
## DAV MILLER. GITHUB LINK: https://github.com/davmiller/NCAA-baseball/blob/master/R/get_NCAA_base_pbp.R
## I DO NOT TAKE CREDIT FOR THIS CODE #####################
##########################################################################
#########################################################################






# Functions for parsing 

game_end = function(game_id){
  m=length(game_id)
  game_end=integer(m)
  for (i in 2:m){
    if (game_id[i]!=game_id[i-1]){
      game_end[i-1]=1
    }
    game_end[m]=1
  }
  return(game_end)
}

r1_name = function(bat_text, bat_name, r1_text, r1_name, inn_end, game_end, sub_in, sub_out){
  m=length(bat_text)
  r1_name= character(m)
  for (i in 2:m){
    if (inn_end[i-1]==0 & game_end[i-1]==0){
      r1_name[i]=case_when(
        sub_out[i-1]!=''&sub_out[i-1]==stripwhite(r1_name[i-1])~sub_in[i-1],
        (str_detect(bat_text[i-1], '(singled|walked|hit by pitch|reached)') == TRUE) & (str_detect(bat_text[i-1], '(doubled|tripled|homered|advanced|scored|out|stole)') == FALSE) ~ bat_name[i-1],
        (str_detect(bat_text[i-1], '(reached first)') == TRUE) & (str_detect(bat_text[i-1], '(struck out)') == TRUE) ~ bat_name[i-1],
        (r1_text[i-1]==''|(str_detect(r1_text[i-1], '(advanced to second|stole second|advanced to third|stole third|scored|out)') == FALSE)) & (str_detect(bat_text[i-1], '(double play|advanced to second|stole second|advanced to third|stole third|scored|caught stealing|picked off|homered)') == FALSE)  ~ r1_name[i-1],
        (str_detect(bat_text[i-1], '(singled|doubled|tripled|advanced to second|stole second|advanced to third|stole third|scored|homered|out at second c to)') == FALSE) & (str_detect(r1_text[i-1], '(advanced to third|stole third|scored|out at third)') == TRUE) & stripwhite(gsub('((advanced to second|stole second|stole third|advanced to third|scored|out).*$)', '', r1_text[i-1]))!=stripwhite(gsub('((singled|reached).*$)', '', r1_name[i-1])) ~ r1_name[i-1],
        r1_text[i-1]=='' & stripwhite(gsub('((advanced to second|stole second|stole third|advanced to third|scored|out|failed|Failed|picked off).*$)', '', bat_text[i-1]))!=stripwhite(r1_name[i-1]) ~ r1_name[i-1]
      )}}
  return(stripwhite(r1_name))
}


r2_name = function(bat_text, bat_name, r1_text, r1_name, r2_text, r2_name,  inn_end, game_end, sub_in, sub_out){
  m=length(bat_text)
  r2_name= character(m)
  for (i in 2:m){
    if (inn_end[i-1]==0 & game_end[i-1]==0){
      r2_name[i]=case_when(
        sub_out[i-1]!=''&sub_out[i-1]==stripwhite(r2_name[i-1])~sub_in[i-1],
        ((str_detect(bat_text[i-1], '(doubled|advanced to second|stole second)') == TRUE) & (str_detect(bat_text[i-1], '(advanced to third|scored|out|stole third)') == FALSE)) ~ stripwhite(gsub('((doubled|advanced to second|stole second).*$)', '', bat_text[i-1])),
        ((str_detect(r1_text[i-1], '(advanced to second|stole second)') == TRUE) & (str_detect(r1_text[i-1], '(advanced to third|scored|out|stole third)') == FALSE)) ~ stripwhite(gsub('((advanced to second|stole second).*$)', '', r1_text[i-1])),
        r2_text[i-1]=='' & stripwhite(gsub('((stole third|advanced to third|scored|out).*$)', '', r1_text[i-1]))!=stripwhite(r2_name[i-1]) & (str_detect(bat_text[i-1], '(advanced to third|stole third|scored|picked off|caught stealing)') == FALSE) ~ r2_name[i-1],
        r2_text[i-1]=='' & stripwhite(gsub('((out on the play).*$)', '', r1_text[i-1]))!=stripwhite(r2_name[i-1]) & (str_detect(bat_text[i-1], '(double play)') == TRUE) ~ r2_name[i-1],
        r1_text[i-1]=='' & (str_detect(bat_text[i-1], '(stole third|advanced to third|scored|picked off|homered|caught stealing)') == FALSE) ~ r2_name[i-1],
        sub_out[i-1]!=''&sub_out[i-1]==stripwhite(r2_name[i-1])~sub_in[i-1]
      )
      r2_name[i]=stripwhite(gsub('((singled|reached).*$)', '', r2_name[i]))
    }
  }
  return(stripwhite(r2_name))
}


r3_name = function(bat_text, bat_name, r1_text, r1_name, r2_text, r2_name, r3_text, r3_name, inn_end, game_end, sub_in, sub_out){
  m=length(bat_text)
  r3_name= character(m)
  for (i in 2:m){
    if (inn_end[i-1]==0 & game_end[i-1]==0){
      r3_name[i]=case_when( 
        sub_out[i-1]!=''&sub_out[i-1]==stripwhite(r3_name[i-1])~sub_in[i-1],
        ((str_detect(bat_text[i-1], '(tripled|advanced to third|stole third)') == TRUE) & (str_detect(bat_text[i-1], '(scored|out)') == FALSE)) ~ stripwhite(gsub('((tripled|advanced to third|stole third).*$)', '', bat_text[i-1])),
        ((str_detect(r1_text[i-1], '(advanced to third|stole third)') == TRUE) & (str_detect(r1_text[i-1], '(scored|out)') == FALSE)) ~ stripwhite(gsub('((advanced to third|stole third).*$)', '', r1_text[i-1])),
        ((str_detect(r2_text[i-1], '(advanced to third|stole third)') == TRUE) & (str_detect(r2_text[i-1], '(scored|out)') == FALSE)) ~ stripwhite(gsub('((advanced to third|stole third).*$)', '', r2_text[i-1])),
        r1_text[i-1]=='' & (str_detect(bat_text[i-1], '(scored|stole home|homered)') == FALSE) ~ r3_name[i-1],
        r2_text[i-1]=='' & stripwhite(gsub('((scored|stole home|out).*$)', '', r1_text[i-1]))!=stripwhite(r3_name[i-1]) & (str_detect(bat_text[i-1], '(scored|stole home)') == FALSE) ~ r3_name[i-1],
        r3_text[i-1]=='' & (str_detect(r2_text[i-1], '(scored|stole home|out)') == FALSE) & (str_detect(r1_text[i-1], '(scored|stole home|out)') == FALSE) & (str_detect(bat_text[i-1], '(scored|stole home)') == FALSE) ~ r3_name[i-1])
      r3_name[i]=stripwhite(gsub('((singled|doubled|reached|advanced|stole|failed|Failed|picked off).*$)', '', r3_name[i]))
    }
  }
  return(stripwhite(r3_name))
}

outs_before= function(outs_on_play, new_game, new_inn){
  m=length(outs_on_play)
  inn_outs=integer(m)
  for (i in 2:m){
    if (new_game[i]==0 & new_inn[i]==0){
      inn_outs[i]=((inn_outs[i-1]+outs_on_play[i-1]) %% 3)
    }
  }
  return(inn_outs)
}

new_game=function(game_end){
  m = length(game_end)
  new_game=integer(m)
  new_game[1]=1
  for (i in 2:m){
    new_game[i]=game_end[i-1]
  }
  return(new_game)
}


## Base Out States #################

####################################

pbp_dataframe_clean <- single_pbp_partial_clean %>%
  mutate(
    ##tmp_text=paste(away_text,home_text),
    ###  # 
    
    sub_fl=case_when(
      str_detect(description, '(singled|doubled|tripled|homered|walked|reached|struck out|grounded|flied|lined|popped| hit|infield fly|infield fly|out|double play|triple play)')==TRUE & str_detect(description, c('pinch hit'))==FALSE ~ 0,
      str_detect(description, c('to (p|c|1b|2b|3b|ss|lf|rf|cf|dh)'))==TRUE ~ 1,
      str_detect(description, c('pinch hit'))==TRUE ~ 1,
      str_detect(description, c('pinch ran'))==TRUE ~ 1,
      TRUE ~ 0),
    
    # Split the text up
    bat_text=gsub('(;|3a|:).*$','', description),
    
    r1_text=case_when(
      str_detect(description, '(;|3a|:)')==TRUE ~ stripwhite(gsub('^.*?(;|3a|:)','',description)),
      TRUE~''),
    
    r2_text=case_when(
      str_detect(r1_text, '(;|3a|:)')==TRUE ~ stripwhite(gsub('^.*?(;|3a|:)','',r1_text)),
      TRUE~''),
    
    r3_text=case_when(
      str_detect(r2_text, '(;|3a|:)')==TRUE ~ stripwhite(gsub('^.*?(;|3a|:)','',r2_text)),
      TRUE~''),
    
    r2_text=stripwhite(gsub('(;|3a|:).*$','',r2_text)),
    
    r1_text=stripwhite(gsub('(;|3a|:).*$','',r1_text)),
    
    # Event code: same as retrosheet
    event_cd=case_when(
      #sub_fl==1 ~ 1,
      str_sub(stripwhite(description),1,1)=='(' ~ 1,
      str_detect(description, '(hitting out of turn| for |No play|halted|delay|postponed|ejected|suspended|coach|sunny|review|challenged|HC|\\*\\*)') == TRUE ~ 1,
      str_detect(description,'struck out') == TRUE ~ 3,
      str_detect(description,'stole') == TRUE ~ 4,
      (str_detect(description,'(caught stealing|out at second c to|out at third c to)') == TRUE) & (str_detect(description,'(bunt|grounded)') == FALSE) ~ 6,
      str_detect(description,'picked off') == TRUE ~ 8,
      str_detect(description,'wild pitch') == TRUE ~ 9,
      str_detect(description,'passed ball') == TRUE ~ 10,
      str_detect(description,'balk') == TRUE ~ 11,
      str_detect(description,'Dropped foul') == TRUE ~ 13,
      str_detect(description,'walked') == TRUE ~ 14,
      str_detect(description,'hit by pitch') == TRUE ~ 16,
      str_detect(description,'interference') == TRUE ~ 17,
      str_detect(description,'error') == TRUE ~ 18,
      str_detect(description,'muffed') == TRUE ~ 18,
      str_detect(description,'dropped') == TRUE ~ 18,
      str_detect(description,'fielder\'s choice') == TRUE ~ 19,
      str_detect(description,'singled') == TRUE ~ 20,
      str_detect(description,'doubled') == TRUE ~ 21,
      str_detect(description,'tripled') == TRUE ~ 22,
      str_detect(description,'homered') == TRUE ~ 23,
      str_detect(description, '(flied out|grounded out|popped|fouled out|lined out| infield fly|double play|triple play|out at (first|second|third|home))') == TRUE ~ 2,
      str_detect(description, 'advanced') == TRUE ~ 12,
      TRUE ~ 0),
    
    bat_name= case_when(
      event_cd %in% c(0,1)~'',
      str_detect(bat_text, '(Batter|Runner\'s interference)')==TRUE ~'',
      str_detect(bat_text, '(walked|singled|doubled|tripled|reached|struck out|grounded out)')==FALSE & str_detect(bat_text, '(advanced|caught stealing|stole|picked off|out at (first|second|third|home)|tagged out)')==TRUE ~ '',
      str_detect(bat_text, '(singled|doubled|tripled|homered|walked|reached|struck out|grounded|flied|lined|popped|hit | out |fouled out|pinch hit|infield fly|intentionally walked|was intentionally walked|fouled into double play)')==TRUE ~ gsub('((singled|doubled|tripled|homered|walked|reached|struck out|grounded|flied|lined|popped|hit | out |fouled out|pinch hit|infield fly|intentionally walked|was intentionally walked|fouled into double play).*$)', '', bat_text),
      str_detect(stripwhite(r1_text), 'caught stealing  c to (2b|3b), double play.')==TRUE ~ bat_text,
      TRUE ~ ''),
    
    #  Sub in
    sub_in= case_when(
      sub_fl==1&str_detect(bat_text, 'to (p|c|1b|2b|3b|ss|lf|rf|cf|dh)')==TRUE ~ stripwhite(gsub('(to (p|c|1b|2b|3b|ss|lf|rf|cf|dh).*$)', '', bat_text)),
      sub_fl==1&str_detect(bat_text, 'pinch ran for')==TRUE ~ stripwhite(gsub('pinch ran for.*$', '', bat_text)),
      sub_fl==1&str_detect(bat_text, 'pinch hit for')==TRUE ~ stripwhite(gsub('pinch hit for.*$', '', bat_text)),
      TRUE ~ ''),
    
    # Sub out
    sub_out= case_when(
      sub_fl==1&str_detect(bat_text, 'to (p|c|1b|2b|3b|ss|lf|rf|cf|dh) for')==TRUE ~ gsub('^.*to (p|c|1b|2b|3b|ss|lf|rf|cf|dh) for', '', bat_text),
      sub_fl==1&str_detect(bat_text, 'pinch ran for')==TRUE ~ gsub('^.*pinch ran for', '', bat_text),
      sub_fl==1&str_detect(bat_text, 'pinch hit')==TRUE ~ gsub('^.*pinch hit for', '', bat_text),
      TRUE ~ ''),
    # Clean sub out
    sub_out=strip_punc(sub_out),
    
    
    # Game end
    game_end = game_end(game_pbp_id),
    
    # New game
    new_game=new_game(game_end),
    
    # Top inning
    #top_inning=ifelse(away_text=='', 0,1),
    # End of inning
    #inn_end = inn_end(top_inning),
    # Runner names
    r1_name=r1_name(bat_text, bat_name, r1_text, r1_name, end_inning_flag, game_end, sub_in, sub_out),
    r2_name =r2_name(bat_text, bat_name, r1_text, r1_name, r2_text, r2_name, end_inning_flag, game_end, sub_in, sub_out),
    r3_name =r3_name(bat_text, bat_name, r1_text, r1_name, r2_text, r2_name, r3_text, r3_name, end_inning_flag, game_end, sub_in, sub_out),
    # Clean runner names
    r1_name=replace(r1_name,is.na(r1_name),''),
    r2_name=replace(r2_name,is.na(r2_name),''),
    r3_name=replace(r3_name,is.na(r3_name),''),
    
    # Fix repeat bat names
    bat_name=case_when(
      bat_name!='' & stripwhite(bat_name)==stripwhite(r1_name)~ '',
      bat_name!='' & stripwhite(bat_name)==stripwhite(r2_name)~ '',
      bat_name!='' & stripwhite(bat_name)==stripwhite(r3_name)~ '',
      TRUE ~ bat_name),
    
    outs_on_play=case_when(
      event_cd %in% c(0,1) ~ 0,
      str_count(bat_text, 'triple play') == 1 ~ 3,
      str_count(bat_text, 'double play') == 1 ~ 2,
      (str_detect(bat_text, '( out|popped)') == TRUE) &  (str_detect(bat_text, '(reached)') == TRUE) ~ 0,
      # 1 out
      ((str_detect(bat_text, '( out |popped|infield fly)') == TRUE) & (str_detect(r1_text, '( out |popped)')==FALSE) & (str_detect(r2_text, '( out |popped)')==FALSE) &(str_detect(r3_text, '( out |popped)')==FALSE)) |
        ((str_detect(bat_text, '( out |popped|infield fly)') == FALSE) & (str_detect(r1_text, '( out |popped)')==TRUE) & (str_detect(r2_text, '( out |popped)')==FALSE) &(str_detect(r3_text, '( out |popped)')==FALSE)) |
        ((str_detect(bat_text, '( out |popped|infield fly)') == FALSE) & (str_detect(r1_text, '( out |popped)')==FALSE) & (str_detect(r2_text, '( out |popped)')==TRUE) &(str_detect(r3_text, '( out |popped)')==FALSE)) |
        ((str_detect(bat_text, '( out |popped|infield fly)') == FALSE) & (str_detect(r1_text, '( out |popped)')==FALSE) & (str_detect(r2_text, '( out |popped)')==FALSE) &(str_detect(r3_text, '( out |popped)')==TRUE)) ~ 1,
      #  2 outs
      ((str_detect(bat_text, '( out |popped|infield fly)') == TRUE) & (str_detect(r1_text, '( out |popped)')==TRUE) & (str_detect(r2_text, '( out |popped)')==FALSE) &(str_detect(r3_text, '( out |popped)')==FALSE)) |
        ((str_detect(bat_text, '( out |popped|infield fly)') == TRUE) & (str_detect(r1_text, '( out |popped)')==FALSE) & (str_detect(r2_text, '( out |popped)')==TRUE) &(str_detect(r3_text, '( out |popped)')==FALSE)) |
        ((str_detect(bat_text, '( out |popped|infield fly)') == TRUE) & (str_detect(r1_text, '( out |popped)')==FALSE) & (str_detect(r2_text, '( out |popped)')==FALSE) &(str_detect(r3_text, '( out |popped)')==TRUE)) |
        ((str_detect(bat_text, '( out |popped|infield fly)') == FALSE) & (str_detect(r1_text, '( out |popped)')==TRUE) & (str_detect(r2_text, '( out |popped)')==TRUE) &(str_detect(r3_text, '( out |popped)')==FALSE)) |
        ((str_detect(bat_text, '( out |popped|infield fly)') == FALSE) & (str_detect(r1_text, '( out |popped)')==TRUE) & (str_detect(r2_text, '( out |popped)')==FALSE) &(str_detect(r3_text, '( out |popped)')==TRUE)) |
        ((str_detect(bat_text, '( out |popped|infield fly)') == FALSE) & (str_detect(r1_text, '( out |popped)')==FALSE) & (str_detect(r2_text, '( out |popped)')==TRUE) &(str_detect(r3_text, '( out |popped)')==TRUE)) ~ 2,
      # 3 outs 
      ((str_detect(bat_text, '( out |popped|infield fly)') == TRUE) & (str_detect(r1_text, '( out |popped)')==TRUE) & (str_detect(r2_text, '( out |popped)')==TRUE) &(str_detect(r3_text, '( out |popped)')==FALSE)) |
        ((str_detect(bat_text, '( out |popped|infield fly)') == TRUE) & (str_detect(r1_text, '( out |popped)')==FALSE) & (str_detect(r2_text, '( out |popped)')==TRUE) &(str_detect(r3_text, '( out |popped)')==TRUE)) |
        ((str_detect(bat_text, '( out |popped|infield fly)') == TRUE) & (str_detect(r1_text, '( out |popped)')==TRUE) & (str_detect(r2_text, '( out |popped)')==FALSE) &(str_detect(r3_text, '( out |popped)')==TRUE)) |
        ((str_detect(bat_text, '( out |popped)') == FALSE) & (str_detect(r1_text, '( out |popped)')==TRUE) & (str_detect(r2_text, '( out |popped)')==TRUE) &(str_detect(r3_text, '( out |popped)')==TRUE)) ~ 3,
      TRUE ~ 0),
    
    # Outs before
    outs_before=outs_before(outs_on_play, new_game, top_inning_flag),
    # Outs after
    outs_after=outs_before+outs_on_play,
    
    # Base code
    base_cd_before=case_when(
      stripwhite(r1_name)!='' & r2_name=='' & r3_name=='' ~ 1,
      r1_name=='' & r2_name!='' & r3_name=='' ~ 2,
      r1_name!='' & r2_name!='' & r3_name=='' ~ 3,
      r1_name=='' & r2_name=='' & r3_name!='' ~ 4,
      r1_name!='' & r2_name=='' & r3_name!='' ~ 5,
      r1_name=='' & r2_name!='' & r3_name!='' ~ 6,
      r1_name!='' & r2_name!='' & r3_name!='' ~ 7,
      TRUE~0),
    
    # May be a nice to have
    # Hit type
    hit_type=case_when(
      event_cd==3 ~ 'K',
      str_detect(bat_text,'(bunt)')==TRUE ~ 'B',
      str_detect(bat_text, '(bunt)')==FALSE & str_detect(bat_text, '(SAC)')==TRUE & str_detect(bat_text, '(flied|popped)')==FALSE ~ 'B',
      str_detect(bat_text,'(grounded out|(p|3b|2b|ss|1b) to (p|3b|2b|ss|1b|c))')==TRUE ~ 'GO',
      str_detect(bat_text,'(flied|fouled out to (lf|rf))')==TRUE ~ 'FO',
      str_detect(bat_text,'(lined)')==TRUE ~ 'LO',
      str_detect(bat_text,'(popped|infield fly|fouled out to (p|3b|2b|ss|1b|c))')==TRUE ~ 'PO',
      TRUE ~ ''   ),
    
    ## May be needed idk ################################
    
    # Intentional walk 
    int_bb_fl=case_when(
      str_detect(description,'intentionally ') == TRUE ~ 1,
      TRUE ~ 0
    ),
    
    # Sac bunts
    sh_fl=case_when(
      str_detect(bat_text, '(SAC)')==TRUE & str_detect(bat_text, '(flied|popped)')==FALSE ~ 1,
      TRUE~0),
    
    # Sac flys
    sf_fl=case_when(
      str_detect(bat_text, '(SAC)')==TRUE & str_detect(bat_text, '(flied|popped)')==TRUE ~ 1,
      str_detect(bat_text, '(SAC)')==FALSE & str_detect(bat_text, '(flied|popped)')==TRUE & str_detect(bat_text, '(RBI)')==TRUE~1,
      TRUE~0 )
  )

pbp_data_frame <- pbp_dataframe_clean%>%
  mutate(base_cd_after = case_when(
      end_inning_flag != 1 ~ lead(base_cd_before,n=1)),
    runs_to_end = end_half_inning_runs - current_runs) %>%
  select(-bat_text, -r1_text, -r2_text, -r3_text)
    
#############################################################

View(pbp_data_frame)
#########################################

write.csv(x = pbp_data_frame,
          file = "single_pbp.csv",
          row.names = FALSE)
