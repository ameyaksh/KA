setwd("F:/kaggle/nyc") # set the working directory

library(ggplot2)
library(dplyr)

train <-  read.csv("train.csv") # load the training data
test <- read.csv("test.csv") # load the test data

#str(train) # look at the structure of the training data
#str(test) # look at the structure of the test data

# date_time variable is considered as a factor
# let's split it in two variables : time & date

train$Pickup_date <- as.Date(train$pickup_datetime)
test$Pickup_date <-   as.Date(test$pickup_datetime)
train$Dropoff_date <- as.Date(train$dropoff_datetime)
train$Pickup_time <-  as.POSIXct(train$pickup_datetime)
test$Pickup_time <-  as.POSIXct(test$pickup_datetime)
train$Dropoff_time <- as.POSIXct(train$dropoff_datetime)

#let us check the datatype of the recently created variables

#str(train) # look at the structure of the training data
#str(test) # look at the structure of the test data

# Perfect ! Because R works in memory it will be helpful if remove unnecessary variables

train$pickup_datetime <- NULL
train$dropoff_datetime <-  NULL
test$pickup_datetime <- NULL

# Vendor id shows up as int . But it takes only 2 values . hence convert it to factor 

train$vendor_id <- as.factor(train$vendor_id)
test$vendor_id <- as.factor(test$vendor_id)

# We can calculate the distance between pickup and dropoff using the latitude and longitudes
# We use haversine distance to compute it
# we'll need geosphere package for that 

library(geosphere)

train$haver <-  distHaversine(train[,4:5],train[,6:7])
test$haver <-  distHaversine(test[,4:5],test[,6:7])

# We no longer need latitude , longitude columns 

train$pickup_longitude <-  NULL
train$pickup_latitude <-  NULL
train$dropoff_longitude <-  NULL
train$dropoff_latitude <-  NULL
test$pickup_longitude <- NULL
test$pickup_latitude <-  NULL
test$dropoff_longitude <-  NULL
test$dropoff_latitude <-  NULL

# Let us clear out all the outliers 

boxplot(train$trip_duration)
plot(train$trip_duration)

lr1 <- lm(trip_duration ~ haver, data=train)
summary(lr1)
plot(resid(lr1))
summary(resid(lr1))
boxplot(resid(lr1))

# clearly we have outliers in trip duration 
q <- quantile(train$trip_duration)

#q[4] gives the value for 3rd quartile 
#outlier = 

train <- train[!(train$trip_duration > q[4] + IQR(train$trip_duration)*1.5 ),]
train <- train[!(train$trip_duration < q[2] - IQR(train$trip_duration)*1.5 ),]

lr2 <- lm(trip_duration ~ haver, data=train)
summary(lr2)
boxplot(resid(lr2))

w <- quantile(train$haver)

train <- train[!(train$haver > w[4] + IQR(train$haver)*1.5 ),]
train <- train[!(train$haver < w[2] - IQR(train$haver)*1.5 ),]
plot(train$haver)
lr3 <- lm(trip_duration ~ haver, data=train)
summary(lr3)

lr4 <- lm(trip_duration ~ haver + passenger_count, data=train)
summary(lr4)

# lr 3= 0.4637 & lr4 = .4638 so hardly any improvement
# lets us 0 passenger records & 7 & above .. 

train <- train[!(train$passenger_count==0),]
train <- train[!(train$passenger_count>=7),]
plot(lr4)


#numeric_train <- data.frame(train$passenger_count,train$trip_duration,train$haver)
#cor(numeric_train, method="pearson")
#plot(x=numeric_train$train.trip_duration, y= numeric_train$train.haver, type="h")

#d1 <- density(numeric_train$train.trip_duration, from=0, to=2200)
#plot(d1)
#boxplot(numeric_train$train.trip_duration)

#                       train.passenger_count train.trip_duration train.haver
#train.passenger_count           1.000000000         0.008470988  0.01030558
#train.trip_duration             0.008470988         1.000000000  0.09477678
#train.haver                     0.010305577         0.094776781  1.00000000
