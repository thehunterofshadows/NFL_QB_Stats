#plots
library(data.table)
library(dplyr)
library(ggplot2)

yards<-qbData %>%
  select(name, passing_yards,current_team) %>%
  group_by(name,current_team) %>%
  summarise(passing_yards=sum(passing_yards), num_games=n()) %>%
  filter(num_games >36)
            
yards$yards_per_games<-yards$passing_yards/yards$num_games

yards<-yards[order(-yards$yards_per_games),]

yards$pri_color<-"#121111"
yards$sec_color<-"#ffffff"

yards$pri_color[yards$current_team=="Green Bay Packers"]="#203731"
yards$sec_color[yards$current_team=="Green Bay Packers"]="#FFB612"

yards$pri_color[yards$current_team=="Indianapolis Colts"]="#002C5F"
yards$sec_color[yards$current_team=="Indianapolis Colts"]="#A2AAAD"



#v5
fillColor<-c(replicate(8,"#3480EB"),"#203731","#3480eb")
textColor<-c(replicate(8,"white"),"#FFB612","white")
qbPos<-c(0,2,4,6,8,10,12,14,16,18)
g<-ggplot(
  data = yards[1:10,],
  mapping = aes(x=reorder(name,-yards_per_games), 
                y=yards_per_games 
                )
)+
  geom_bar(
    stat="identity",
    fill=yards$pri_color[1:10]
    )+
  theme(
    axis.title.x=element_text(size=14, color="#993333", face="bold"),
    axis.title.y=element_text(size=14, color="#993333", face="bold"),
    plot.title=element_text(size=14, color="red", face="bold",hjust=.5),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  )+
  labs(
    title="Yards per Game",x="QB Names",y="Yards per Game"
  )+
  geom_text(
    mapping=aes(label=name,y=((yards_per_games*.9)-qbPos),vjust=.5),
    angle=90,size=8,colour=yards$sec_color[1:10]
  )+
  scale_colour_manual(
    values="white"
  )+
  coord_cartesian(
    ylim=c(200,280)
  )
g
