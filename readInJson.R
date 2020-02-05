#Read in kaggle json file
#install.packages("rjson")
library(rjson)

result <- fromJSON(file = "./Data/kagNflPlayer/games_1512362753.8735218.json")
kagDF<-as.data.frame(result)
