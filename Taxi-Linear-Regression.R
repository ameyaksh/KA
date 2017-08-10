setwd("F:/kaggle/nyc") # set the working directory

library(dplyr)

train <-  read.csv("train.csv") # load the training data
test <- read.csv("test.csv") # load the test data

#str(train) # look at the structure of the training data

#str(test) # look at the structure of the test data

# check for NAs

#summary(train)
train$vendor_id <- as.factor(train$vendor_id)
#summary(test)

test$vendor_id <- as.factor(test$vendor_id)

train$Pickup_date <- as.Date(train$pickup_datetime)

test$Pickup_date <-   as.Date(test$pickup_datetime)


train$Dropoff_date <- as.Date(train$dropoff_datetime)


train$Pickup_time <-  as.POSIXct(train$pickup_datetime)


test$Pickup_time <-  as.POSIXct(test$pickup_datetime)

train$Dropoff_time <- as.POSIXct(train$dropoff_datetime)




#train$Pickup_time2 <-  as.numeric(train$Pickup_time)/10000000
#test$Pickup_time2 <-  as.numeric(test$Pickup_time)/10000000


train$wd <- as.integer(as.POSIXlt(train$Pickup_date)$wday)
test$wd <-as.integer( as.POSIXlt(test$Pickup_date)$wday)



library(geosphere)

train$haver <-  distHaversine(train[,6:7],train[,8:9])
test$haver <-  distHaversine(test[,5:6],test[,7:8])


#lr <- lm(trip_duration ~ vendor_id + passenger_count + pickup_longitude + pickup_latitude + dropoff_longitude + dropoff_latitude + store_and_fwd_flag +wd , data=train)
#summary(lr)

#train <- train[!(train$trip_duration > 1000000),]

#lr1 <- lm(trip_duration ~ vendor_id + passenger_count + pickup_longitude + pickup_latitude + dropoff_longitude + dropoff_latitude + store_and_fwd_flag +wd , data=train)
#summary(lr1)



#train$day <- as.integer( format( as.Date(train$pickup_datetime,format="%Y-%m-%d"), "%d"))
#test$day <- as.integer(format( as.Date(test$pickup_datetime,format="%Y-%m-%d"), "%d"))

#lr2 <- lm(trip_duration ~ vendor_id + passenger_count + haver + store_and_fwd_flag +wd + day, data=train)
#summary(lr2)

#sub4 <- predict(lr2,test)

#write.csv(sub4,file="sub4.csv")

# the p value of the day variable comes out to be 0.08 which is more than our threshold of 0.05 hence we remove it from the eqaution



#lr3 <- lm(trip_duration ~ vendor_id + passenger_count + haver + store_and_fwd_flag +wd, data=train)
#
#summary(lr3)

#lr4 <- lm(log(trip_duration) ~  haver , data=train)

#summary(lr4)

#summary(train$trip_duration)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.0   397.0   662.0   952.8  1075.0 86390.0 

outlier_calc <- IQR(train$trip_duration)
#[1] 1017

Q1 <- 397
Q3 <- 1075

upper_limit <- Q3 +outlier_calc*1.5
lowerlimit <- Q1 -outlier_calc*1.5

# out lier : greater than 1.5*IQR and less than 1.5*IQR


train2 <- train[!( train$trip_duration > upper_limit ),]