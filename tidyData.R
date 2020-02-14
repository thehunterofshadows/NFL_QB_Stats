#tidy data
library(lubridate)


tidyData<-function(yards){
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