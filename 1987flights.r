# kNN implementation for 1987 flight dataset
# Classification on-time/delayed flights
# Dataset 1987.csv taken from ASA Statistical Computing 
# and Statistical Graphics Data expo 2009.
# http://stat-computing.org/dataexpo/2009/the-data.html

setwd("~/Masters Applied Math/Classes/Spring 2015/STAT6620/Airline Data")

#SQL
library(RSQLite)
ontime <- dbConnect(RSQLite::SQLite(), dbname = "ontime.sqlite3")
from_db <- function(sql) {
  dbGetQuery(ontime, sql)
}

#Creating raw dataframe
data_raw <- from_db("SELECT * FROM ontime WHERE year=1987")
str(data_raw)

#Data Cleaning
col_sel <- c("DayofMonth","DayOfWeek","DepTime","CRSDepTime","ArrTime","CRSArrTime",
             "FlightNum","ActualElapsedTime","CRSElapsedTime","ArrDelay","DepDelay","Distance")
row_sel <- complete.cases(data_raw[,col_sel])
data <- data_raw[row_sel,col_sel]

#Creating Class: OnTime
data$OnTime <- as.numeric(data$ArrDelay > 15)
data$ArrDelay <- NULL
table(data$OnTime)
round(prop.table(table(data$OnTime))*100,digits=1)

# Time function
# Converting time to minutes
num2min  <- function(x){
  minutes <- 60*(x%/%100)+(x%%100)
  return (minutes)
}
f_col_sel = c("DepTime","CRSDepTime","ArrTime","CRSArrTime")
data[f_col_sel]=as.data.frame(lapply(data[f_col_sel],num2min))

# Normalizing function
normalize = function(x) {
  return ((x-min(x))/(max(x)-min(x)))
}
norm_col_sel = c("DayofMonth","DayOfWeek","DepTime","CRSDepTime","ArrTime",
                 "CRSArrTime","ActualElapsedTime","CRSElapsedTime")
data[norm_col_sel]=as.data.frame(lapply(data[norm_col_sel],normalize))

# Creating datasets for kNN Algorithm
idxs <- sample(1:nrow(data), as.integer(0.8*nrow(data)))
data_knn_train = data[idxs,]
data_knn_test = data[-idxs,]
knn_train_labels = data[idxs,"OnTime"]
knn_test_labels = data[-idxs,"OnTime"]

# Training kNN
library("class")
Sys.time()
data_pred = knn(train = data_knn_train, 
                test = data_knn_test, cl= knn_train_labels, k=9)
Sys.time()

# Showing results
library("gmodels")
CrossTable(x = knn_test_labels, y = data_pred, prop.chisq=F)
