#plots
library(data.table)
library(dplyr)

qbData<-data.table(qbData)

yards<-select(qbData, name, passing_yards)
yards<-summarise(group_by(yards,name), passing_yards=sum(passing_yards))
yards<-yards[order(-yards$passing_yards),]

#fix this below using what's above
yards<-qbData %>%
  select(name, passing_yards) %>%
  group_by(name) %>%
  summarise(total_passing_yards=sum(passing_yards)) %>%
  order(total_passing_yards, decreasing = TRUE)

