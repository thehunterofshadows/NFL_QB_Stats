#data discovery
#find out how many games are attached to more than one QB

dups <- qbData %>%
  filter(team == "GNB") %>%
  select(name, team, date) %>%
  group_by(team, date ) %>%
  summarise(num_of_repeat = n()) %>%
  filter(num_of_repeat >1)

brett<- qbData %>%
  filter(team == "GNB") %>%
  select(name, game_won, date) %>%
  filter(name == "Brett Favre ",
         date %in% dups$date)

who_won <- qbData %>%
  select(date, name, passing_attempts, team) %>%
  filter(date %in% brett$date,
         team == "GNB") %>%
  group_by(date, name, passing_attempts)

dups<-dups[order(dups$num_of_repeat, decreasing = TRUE),]

#what wins are Brett earned

brett_wins <-  qbData %>%
  select(name, date, team, game_won) %>%
  filter(game_won == "True",
         name == "Brett Favre ")
brett_wins <- brett_wins [order(brett_wins$date),]
