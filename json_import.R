#import json
#use python to convert json first.  dont import json
library(data.table)




games<-fread('../../datascience/DataRodgersROF/nfl-football-player-stats/gamesv2.csv')
profiles<-fread('../../datascience/DataRodgersROF/nfl-football-player-stats/profiles.csv')

#now merge the two based on player id