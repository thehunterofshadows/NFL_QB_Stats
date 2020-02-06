#This is interesting code, but none of it ended up being needed.
#I ended up loading the data to mysql, and pull with a query


#Read in kaggle json file
#install.packages("rjson")
#install.packages("rJava")

# library(data.table)
# library(rjson)
# result <- fromJSON(file = "./Data/kagNflPlayer/profiles_1512362725.022629.json")
# kagDF<-as.data.frame(result)

setwd("C:/Users/justin/Documents/datascience/RodgersROF")
library(data.table)
library(sparklyr)
library(dplyr)
library(jsonlite)
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_241")
library(rJava)

Sys.setenv(SPARK_HOME="/usr/lib/spark")
# Configure cluster (c3.4xlarge 30G 16core 320disk)
conf <- spark_config()
conf$'sparklyr.shell.executor-memory' <- "50G"
conf$'sparklyr.shell.driver-memory' <- "50G"
conf$spark.executor.cores <- 20
conf$spark.executor.memory <- "50G"
conf$spark.yarn.am.cores  <- 20
conf$spark.yarn.am.memory <- "50G"
conf$spark.yarn.executor.memoryOverhead <- "1g"
conf$spark.executor.instances <- 20
conf$spark.dynamicAllocation.enabled <- "false"
conf$maximizeResourceAllocation <- "true"
conf$spark.default.parallelism <- 32

sc <- spark_connect(master = "local", config = conf, version = '2.2.0')
sample_tbl <- spark_read_json(sc,name="example",path="./Data/kagNflPlayer/profiles_1512362725.022629.json", header = TRUE, memory = FALSE,
                              overwrite = TRUE)
results<-as.data.frame(sample_tbl)
sdf_schema_viewer(sample_tbl)