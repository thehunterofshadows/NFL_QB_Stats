#tidy data

#determine what other fields you need in query

#convert age into somethign useful.  Split out into year and days somehow.  Maybe somethign like 33.3

#convert draft_year to date

#made note game_number is game number that year, not total.
library(lubridate)

tidyData<-function(yards){
  teamColors<-read.csv("./data/teamColors.csv")
  # yards<-yards[order(-qbData$year),]
  # lapply(yards,function(x){
  #   yards[yards$name==x$name,][1,"year"]
  # })
  
  yards$pri_color<-"#121111"
  yards$sec_color<-"#ffffff"
  yards$last_team<-"none"
  # myNames<-unique(yards$name)
  # for (myName in myNames){
  #   #yards$last_team[yards$name==myName]<-yards[yards$name==myName,][1,"team"]
  #   myValue<-yards[yards$name==myName,][1,"team"]
  #   yards[yards$name==myName,"last_team" := myValue]
  # }
  
  yards<-yards[order(yards$date,decreasing = TRUE),]
  for (i in 1:length(yards$name)){
    team<-yards[yards$name==yards$name[i],][1,"team"]
    yards$last_team[i]<-team
    yards$pri_color[i]<-teamColors$priColor[teamColors$team==team]
    yards$sec_color[i]<-teamColors$sndColor[teamColors$team==team]
    
  }
    
  
  #qbData[qbData$name=="Aaron Rodgers ",][1,"year"]
  #setup colors

  
  # yards$current_team[yards$name=="Peyton Manning"]="Indianapolis Colts"
  # yards$current_team[yards$name=="Kirk Cousins"]="Minnesota Vikings"
  # yards$current_team[yards$name=="Warren Moon"]="Minnesota Vikings"
  # yards$current_team[yards$name=="Fran Tarkenton"]="Minnesota Vikings"
  # yards$name[yards$name=="Brett Favre "]="Brett Favre"
  # yards$current_team[yards$name=="Brett Favre"]="Green Bay Packers"
  # yards$current_team[yards$name=="Dan Marino"]="Miami Dolphins"
  # yards$current_team[yards$name=="John Elway"]="Denver Broncos"
  
  
  
  
  # yards$pri_color[yards$current_team=="Green Bay Packers"]="#203731"
  # yards$sec_color[yards$current_team=="Green Bay Packers"]="#FFB612"
  # 
  # yards$pri_color[yards$current_team=="Indianapolis Colts"]="#002C5F"
  # yards$sec_color[yards$current_team=="Indianapolis Colts"]="#A2AAAD"
  # 
  # yards$pri_color[yards$current_team=="Minnesota Vikings"]="#4F2683"
  # yards$sec_color[yards$current_team=="Minnesota Vikings"]="#FFC62F"
  # 
  # yards$pri_color[yards$current_team=="Detroit Lions"]="#0076B6"
  # yards$sec_color[yards$current_team=="Detroit Lions"]="#B0B7BC"
  # 
  # yards$pri_color[yards$current_team=="Atlanta Falcons"]="#A71930"
  # yards$sec_color[yards$current_team=="Atlanta Falcons"]="#000000"
  # 
  # yards$pri_color[yards$current_team=="New Orleans Saints"]="#D3BC8D"
  # yards$sec_color[yards$current_team=="New Orleans Saints"]="#101820"
  # 
  # yards$pri_color[yards$current_team=="New England Patriots"]="#002244"
  # yards$sec_color[yards$current_team=="New England Patriots"]="#C60C30"
  # 
  # yards$pri_color[yards$current_team=="Los Angeles Rams"]="#002244"
  # yards$sec_color[yards$current_team=="Los Angeles Rams"]="#866D4B"
  # 
  # yards$pri_color[yards$current_team=="Los Angeles Chargers"]="#002A5E"
  # yards$sec_color[yards$current_team=="Los Angeles Chargers"]="#FFC20E"
  # 
  # yards$pri_color[yards$current_team=="Miami Dolphins"]="#008E97"
  # yards$sec_color[yards$current_team=="Miami Dolphins"]="#FC4C02"
  # 
  # yards$pri_color[yards$current_team=="Denver Broncos"]="#FB4F14"
  # yards$sec_color[yards$current_team=="Denver Broncos"]="#002244"
  # 
  # yards$pri_color[yards$current_team=="Pittsburgh Steelers"]="#FFB612"
  # yards$sec_color[yards$current_team=="Pittsburgh Steelers"]="#101820"
  # 
  # yards$pri_color[yards$current_team=="New York Giants"]="#0B2265"
  # yards$sec_color[yards$current_team=="New York Giants"]="#A71930"
  
  yards
}