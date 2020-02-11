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
  geom_text(mapping=aes(label=name,y=yards_per_games/2),angle=90)
