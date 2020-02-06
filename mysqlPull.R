#read in mysql data
install.packages("RMySQL")
library(RMySQL)

#build connection to database
mydb = dbConnect(MySQL(), user='justin', password='57Acer@1', dbname='nfl', host='localhost')

#write query, and escapte things like double "
myQuery<- "SELECT p.name, p.draft_year, g.game_won, g.passing_rating, g.passing_attempts, g.passing_yards, g.passing_touchdowns, g.game_number, g.age 
FROM nfl.profiles as p
inner join nfl.games as g
on g.player_id=p.player_id
where p.position=\"QB\"
order by p.player_id
;"

#open query
rs = dbSendQuery(mydb, myQuery)

#pull all data from query, you could instead go row by row if needed
data = fetch(rs, n=-1)

#disconnect all connections to mysql databases
lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)