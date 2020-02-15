#tidy data
library(lubridate)
library(dplyr)


tidyData<-function(yards){
  #the data has the completions and attempts flipped.
  yards$fixed_passing_completions<-yards$passing_attempts
  yards$fixed_passing_attempts<-yards$passing_completions
  
  #pull in the team colors
  teamColors<-read.csv("./data/teamColors.csv", colClasses = "character")
  
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
    
  
  #return the data to the function
  yards
}

topFive<-function(x){
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
    filter(games>36)
  names<-namesList$name
  
  for (myName in names){
    #passer rating
    #need to ensure avg is correct, as in if less then 16 games were played, can't avg seaon
    #need to split back out to all the games, and avg that.
    # result <- data.table(
    #   name = character(),
    #   passer_rating = numeric()
    # )
    #  Need to set it up to limit to only QB's with 5 seasons
    yards<- x %>%
      filter(myName==name) %>%
      filter(passing_attempts>11) %>%
      select(year, passing_rating, pri_color, sec_color) %>%
      group_by(year, pri_color, sec_color) %>%
      summarise(y=mean(passing_rating), games = n()) %>%
        filter(games>8)
      
    
    yards<-yards[order(-yards$y),]
    myYears<-yards$year[1:5]
    
    yards<- x %>%
      filter(myName==name, year %in% myYears) %>%
      select(passing_rating, pri_color, sec_color) %>%
      group_by(pri_color, sec_color) %>%
      summarise(y=mean(passing_rating))
    
    passer_rating<-yards$y
    myName<-as.character(myName)
    results<-rbind(results, data.table(name=myName, passer_rating=passer_rating, years=paste(myYears,sep = " ")))
    
  }
  results
}