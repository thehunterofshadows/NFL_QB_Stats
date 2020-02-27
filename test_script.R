#generic test script file
set.seed(76535)
index<-sample(1:nrow(qbData),1000,replace=FALSE)
qbSamp<-qbData[index,]

saveRDS(qbSamp, "./data/qbSamp.rds")
qbSamp<-readRDS("./data/qbSamp.rds")
