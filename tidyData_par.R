#single threaded tidy took 152 seconds.  Using 8 cores it took 20.34.

#tidy data
# library(parallel)
library(lubridate)
library(dplyr)
library(data.table)
library(doSNOW)
library(foreach)
library(tictoc)

##defaults
#values to determine who gets credited with win
td_value = 100
yards_value = 1

#defatul team colors
pri_color<-"#121111"
sec_color<-"#ffffff"
last_team<-"none"
main_team<-"none"

tidy_add_team_data<-function(yards){
  #set all the values to the defalt value as a starting point
  yards$pri_color<-pri_color
  yards$sec_color<-sec_color
  yards$last_team<-last_team
  yards$main_team<-main_team
  
  yards$old_team_value<-yards$team
  
  #pull in the team colors
  teamColors<-read.csv("teamColors.csv", colClasses = "character")
  
  #create list of which team each player played the most games with.  Then order it so on top is the most played
  mainTeam<-yards %>%
    select(name, team) %>%
    group_by(name, team) %>%
    summarise(nTeam=n())
  mainTeam<-mainTeam[order(mainTeam$nTeam, decreasing = TRUE),]
  
  
  #update team detail
  yards<-yards[order(yards$date,decreasing = TRUE),]
  
  # cl<-snow::makeCluster(detectCores()-1) #change the 2 to your number of CPU cores
  cl<-snow::makeCluster(13)
  registerDoSNOW(cl)
  tic("sleeping")
  print("falling asleep...")
  
  foreach (i=1:length(yards$name)) %dopar% {
    #this gives us last team they played for
    team<-yards[yards$name==yards$name[i],][1,"team"]
    yards$last_team[i]<-team
    #this gives us the team they played the most for
    #since this comes second, it will write the color of the most played team to the data.
    team<-as.character(mainTeam[mainTeam$name==yards$name[i],][1,"team"])
    yards$main_team[i]<-team
  }
  
  print("waking up")
  toc()
  stopCluster(cl)
  
  #replace team with new value
  yards$team<-yards$main_team
  #Set the team colors
  foreach(i=1:length(teamColors$team)) %do% {
    yards$pri_color[yards$team==teamColors$team[i]]<-teamColors$priColor[i]
    yards$sec_color[yards$team==teamColors$team[i]]<-teamColors$sndColor[i]
  }
  
  #return
  yards
}

tidy_reduce_data <-function(yards){
  #limit to QB's with at least 5 years of 8+ games
  myNames <- unique(yards %>%
                      select(name, year,date) %>%
                      group_by(name, year) %>%
                      summarise(num_games=n()) %>%
                      filter(num_games >= 8) %>%
                      select(name, year) %>%
                      group_by(name) %>%
                      summarise(years=n()) %>%
                      filter(years>=5) %>%
                      select(name))
  yards<-yards[yards$name %in% myNames$name,]
  
  #return
  yards
}

tidy_add_player_value<-function(yards){
  #Create player value to be used to determine who is credited with win
  yards$player_value = (yards$passing_yards * yards_value) + (yards$passing_touchdowns * td_value)
  
  #return
  yards
}

tidy_fix_flipped_data<-function(yards){
  #the data has the completions and attempts flipped.
  yards$fixed_passing_completions<-yards$passing_attempts
  yards$fixed_passing_attempts<-yards$passing_completions
  
  #return
  yards
}

tidy_win_credit<-function(yards){
  #set default at no
  yards$credited_game_win = 0
  
  games <- yards %>%
    select(date, team) %>%
    group_by(date, team) %>%
    summarise(num_games = n())
  
  foreach (i=1:length(games$date), .packages=c("dplyr")) %do% {
    name<-yards[yards$date==games[i]$date & yards$team==games[i]$team,]
    name <- name %>%
      select(date, team, player_value) %>%
      group_by(date, team, name, player_value) %>%
      filter(player_value==max(player_value))
    yards$credited_game_win[
      yards$team==name[1]$team & yards$date==name[1]$date & yards$name==name[1]$name
      ]=1
  }
  
  
  yards$fixed_game_won[yards$game_won=="True"]<-1
  yards$fixed_game_won[yards$game_won=="False"]<-0
  
  #return
  yards
}

tidy_main_par<-function(yards){
  
  yards<-tidy_reduce_data(yards)
  
  yards<-tidy_add_player_value(yards)
  
  yards<-tidy_fix_flipped_data(yards)
  
  yards<-tidy_add_team_data(yards)
  
  # yards<-tidy_win_credit(yards)
  
  #remove all the data dumped into the enviroment.
  rm(last_team,main_team,pri_color,sec_color,td_value,yards_value,tidy_add_player_value,tidy_add_team_data,tidy_fix_flipped_data,tidy_reduce_data,tidy_win_credit)
  
  #return
  yards
}