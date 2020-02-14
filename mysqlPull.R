#read in mysql data
#install.packages("RMySQL")
library(RMySQL)
library(data.table)
source('~/datascience/RodgersROF/tidyData.R')
#build connection to database
mydb = dbConnect(MySQL(), user='justin', password='57Acer@1', dbname='nfl', host='localhost')  


myQuery<- "SELECT p.current_team,p.college,p.draft_position,p.draft_year,p.name,p.weight,p.draft_round,p.death_date,p.current_salary,p.height,p.birth_date,p.hof_induction_year,p.birth_place,p.player_id,p.position,p.high_school,p.draft_team,
g.kick_return_attempts,g.passing_rating,g.field_goal_makes,g.game_won,g.passing_interceptions,g.receiving_yards,g.kick_return_touchdowns,g.year,g.punt_return_attempts,g.game_location,g.passing_attempts,g.rushing_touchdowns,g.punting_attempts,g.receiving_receptions,g.passing_sacks_yards_lost,g.kick_return_yards,g.defense_safeties,g.rushing_attempts,g.passing_yards,g.defense_tackles,g.passing_sacks,g.defense_sacks,g.punting_blocked,g.receiving_targets,g.opponent,g.point_after_attemps,g.field_goal_attempts,g.punt_return_yards,g.point_after_makes,g.defense_interception_yards,g.defense_tackle_assists,g.passing_completions,g.date,g.passing_touchdowns,g.receiving_touchdowns,g.player_team_score,g.rushing_yards,g.punt_return_touchdowns,g.game_number,g.age,g.defense_interception_touchdowns,g.defense_interceptions,g.team,g.punting_yards,g.opponent_score 
FROM nfl.profiles as p
inner join nfl.games as g
on g.player_id=p.player_id
where p.position=\"QB\"
order by p.player_id
;"

#open query
rs = dbSendQuery(mydb, myQuery)

#pull all data from query, you could instead go row by row if needed
qbData = fetch(rs, n=-1)

#disconnect all connections to mysql databases
lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

rm(mydb, rs, myQuery)
qbData<-tidyData(qbData)

saveRDS(qbData,"./data/qbData.rds")
saveRDS(qbData,"./QBStats/data/qbData.rds")

#Data Details
#year is the season start year, not the actual year the game was played.  So game 17 played in 2018, would still show up as year 2017