#plots
library(data.table)
library(dplyr)
library(ggplot2)

yards<-qbData %>%
  select(name, passing_yards) %>%
  group_by(name) %>%
  summarise(passing_yards=sum(passing_yards), num_games=n()) %>%
  filter(num_games >36)
            
yards$yards_per_games<-yards$passing_yards/yards$num_games

yards<-yards[order(-yards$yards_per_games),]

yards<-yards%>%
  select(name, yards_per_games)

#v1
g<-ggplot(data = yards[1:10,]) +
  geom_bar(mapping= aes(x=reorder(name,-yards_per_games), 
                        y=yards_per_games, fill=yards_per_games), stat="identity")+
  theme(
    axis.text.x=element_text(angle=35,hjust=1, size=12),
    axis.title.x=element_text(size=14, color="#993333", face="bold"),
    axis.title.y=element_text(size=14, color="#993333", face="bold"),
    plot.title=element_text(size=14, color="red", face="bold")
    ) +
  labs(title="Yard per Game",x="QB Names",y="Yards per Game")

#v2
g<-ggplot(data = yards[1:10,]) +
  geom_bar(mapping= aes(x=reorder(name,-yards_per_games), 
                        y=yards_per_games, fill=yards_per_games), stat="identity")+
  theme(
    axis.text.x=element_text(angle=35,hjust=1,vjust=1, size=12),
    axis.title.x=element_text(size=14, color="#993333", face="bold"),
    axis.title.y=element_text(size=14, color="#993333", face="bold"),
    plot.title=element_text(size=14, color="red", face="bold")
  ) +
  labs(title="Yard per Game",x="QB Names",y="Yards per Game")+
  geom_text(mapping=aes(label=yards_per_games,y=yards_per_games/2))

#v3
g<-ggplot(data = yards[1:10,],
          mapping= aes(x=reorder(name,-yards_per_games), 
                                           y=yards_per_games, 
                                           fill=yards_per_games)) +
  geom_bar( stat="identity")+
  theme(
    axis.text.x=element_text(angle=35,hjust=1,vjust=1, size=12),
    axis.title.x=element_text(size=14, color="#993333", face="bold"),
    axis.title.y=element_text(size=14, color="#993333", face="bold"),
    plot.title=element_text(size=14, color="red", face="bold")
  ) +
  labs(title="Yard per Game",x="QB Names",y="Yards per Game")+
  geom_text(mapping=aes(label=name,y=yards_per_games*.75),angle=90,size=8,colour="White") +
  scale_colour_manual(values="white")

#v4
#Create way to slide names down the list.  Maybe try getting len of qb name, and use that
#qbPos<-c(0,5,10,15,20,25,30,35,40,45)

#needed to reset these values as i used a ylim to start the plot at 150

#note use of the reorder to ensure bars are in the right order
#test
qbPos<-c(0,2,4,6,8,10,12,14,16,18)
g<-ggplot(
  data = yards[1:10,],
          mapping = aes(x=reorder(name,-yards_per_games), 
                       y=yards_per_games, 
                       fill=yards_per_games)
  )+
  geom_bar(stat="identity")+
  theme(
    axis.text.x=element_text(angle=35,hjust=1,vjust=1, size=12),
    axis.title.x=element_text(size=14, color="#993333", face="bold"),
    axis.title.y=element_text(size=14, color="#993333", face="bold"),
    plot.title=element_text(size=14, color="red", face="bold",hjust=.5)
  )+
  labs(
    title="Yards per Game",x="QB Names",y="Yards per Game"
    )+
  geom_text(
    mapping=aes(label=name,y=((yards_per_games*.9)-qbPos),vjust=.5),
    angle=90,size=8,colour="White"
    )+
  scale_colour_manual(
    values="white"
    )+
  coord_cartesian(
    ylim=c(150,280)
  )
