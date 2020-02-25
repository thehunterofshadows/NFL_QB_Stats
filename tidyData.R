#single threaded tidy took 152 seconds.  Using 8 cores it took 20.34.

#tidy data
library(lubridate)
library(dplyr)
library(data.table)
library(tictoc)

tidy_data<-function(yards){
  
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
  
  
  #the data has the completions and attempts flipped.
  yards$fixed_passing_completions<-yards$passing_attempts
  yards$fixed_passing_attempts<-yards$passing_completions
  
  #pull in the team colors
  teamColors<-read.csv("teamColors.csv", colClasses = "character")
  
  #create list of which team each player played the most games with.  Then order it so on top is the most played
  mainTeam<-qbData %>%
    select(name, team) %>%
    group_by(name, team) %>%
    summarise(nTeam=n())
  mainTeam<-mainTeam[order(mainTeam$nTeam, decreasing = TRUE),]
  
  #Setup defaults
  yards$pri_color<-"#121111"
  yards$sec_color<-"#ffffff"
  yards$last_team<-"none"
  yards$main_team<-"none"
  
  #update team detail
  yards<-yards[order(yards$date,decreasing = TRUE),]
  tic("sleeping")
  print("falling asleep...")
  for (i in 1:length(yards$name)){
    #this gives us last team they played for
    team<-yards[yards$name==yards$name[i],][1,"team"]
    yards$last_team[i]<-team
    #this gives us the team they played the most for
    #since this comes second, it will write the color of the most played team to the data.
    team<-as.character(mainTeam[mainTeam$name==yards$name[i],][1,"team"])
    yards$main_team[i]<-team
    yards$pri_color[i]<-teamColors$priColor[teamColors$team==team]
    yards$sec_color[i]<-teamColors$sndColor[teamColors$team==team]
  }
  print("waking up")
  toc()
  yards$fixed_game_won[yards$game_won=="True"]<-1
  yards$fixed_game_won[yards$game_won=="False"]<-0
  
  
  #return the data to the function
  yards
}

topFive<-function(x){
  #constants
  min_games = 36
  top_years = 5
  min_top_games = 8
  
  #still trying to figure out how i'm going to do this
  #through it to iterate through each name and for each state pull up the data
  #then filter for top 5, then re-average or retotal
  results<-data.table(
    name = character(),
    passer_rating = numeric(),
    years = character()
  )
  namesList<-x %>%
    select(name) %>%
    group_by(name) %>%
    summarise(games=n()) %>%
    filter(games>min_games)
  names<-namesList$name
  
  for (myName in names){
    #passer rating
    #  Need to set it up to limit to only QB's with 5 seasons
    yards<- x %>%
      filter(myName==name) %>%
      filter(passing_attempts>11) %>%
      select(year, passing_rating, pri_color, sec_color) %>%
      group_by(year, pri_color, sec_color) %>%
      summarise(y=mean(passing_rating), games = n()) %>%
      filter(games>min_top_games)
    
    if (nrow(yards)>=top_years){
      yards<-yards[order(-yards$y),]
      myYears<-yards$year[1:top_years]
      
      yards<- x %>%
        filter(myName==name, year %in% myYears) %>%
        select(passing_rating, pri_color, sec_color) %>%
        group_by(pri_color, sec_color) %>%
        summarise(y=mean(passing_rating))
      
      passer_rating<-yards$y
      myName<-as.character(myName)
      myYear<-paste(myYears,collapse = " ")
      results<-rbind(results, data.table(name=myName, passer_rating=passer_rating, years=myYear))
    }
  }
  results<-results[order(results$passer_rating,decreasing = TRUE),]
  results
}


topRating<-function(x){
  #constants
  min_games = 36
  top_years = 5
  top_yearsTF = TRUE
  min_top_games = 8
  
  stat_value = "passing_rating"
  #stat_value = "passing_yards"
  #stat_value = "passing_touchdowns"
  stat_select = list(stat_value,"year","pri_color","sec_color")
  stat_group = list("year","pri_color","sec_color")
  #summ <- paste0('mean(', stat_value, ')')  # construct summary method, e.g. mean(mpg)
  #summ_name <- paste0('mean_', stat_value)
  type<-"sum("
  summ <- paste0(type, stat_value, ')')  # construct summary method, e.g. mean(mpg)
  #summ <- paste0('sum(', stat_value, ')')  # construct summary method, e.g. mean(mpg)
  summ_name <- paste0('sum_', stat_value)
  
  
  #after years are figured out
  noYear_stat_select = list(stat_value,"pri_color","sec_color")
  noYear_stat_group = list("pri_color","sec_color")
  
  
  #still trying to figure out how i'm going to do this
  #through it to iterate through each name and for each state pull up the data
  #then filter for top 5, then re-average or retotal
  #will probably need to create some of the other fields in the orginal data
  #such as compleation %.  the per game stuff, not sure how to handle that yet
  results<-data.table(
    name = character(),
    passer_rating = numeric(),
    years = character()
  )
  namesList<-x %>%
    select(name) %>%
    group_by(name) %>%
    summarise(games=n()) %>%
    filter(games>min_games)
  names<-namesList$name
  
  for (myName in names){
    #reset number of good years
    number_of_years<-top_years
    
    if (top_yearsTF){
      yards<- x %>%
        filter(myName==name) %>%
        filter(passing_attempts>11) %>%
        select_(.dots = stat_select) %>%
        group_by_(.dots = stat_group) %>%
        summarise_(y=(.dots = setNames(summ, summ_name)), games = (.dots="n()")) %>%
        filter(games>min_top_games)
      number_of_years<-nrow(yards)
      
      if (number_of_years>=top_years){
        yards<-yards[order(-yards$y),]
        myYears<-yards$year[1:top_years]
        
        yards<- x %>%
          filter(myName==name, year %in% myYears) %>%
          select_(.dots=noYear_stat_select) %>%
          group_by_(.dots=noYear_stat_group) %>%
          summarise_(y=(.dots = setNames(summ, summ_name)))
      }
    } else if (!top_yearsTF) {
      yards<- x %>%
        filter(myName==name) %>%
        select_(.dots=noYear_stat_select) %>%
        group_by_(.dots=noYear_stat_group) %>%
        summarise_(y=(.dots = setNames(summ, summ_name)))
      myYears<-"All"
    }
    if (number_of_years>=top_years){
      passer_rating<-yards$y
      myName<-as.character(myName)
      myYear<-paste(myYears,collapse = " ")
      number_of_years<-nrow(yards)
      
      results<-rbind(results, data.table(name=myName, passer_rating=passer_rating, years=myYear))
    }
  }
  results<-results[order(results$passer_rating,decreasing = TRUE),]
  results
}