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

#boxplot(train2$trip_duration)


library(rpart)
tree1 <- rpart(trip_duration ~ vendor_id + passenger_count + haver + store_and_fwd_flag +wd, data = train2, method="anova")

printcp(tree1)
plotcp(tree1)
print(tree1)
plot(tree1)
text(tree1)
rsq.rpart(tree1)

tree1$cptable


library(rattle)

library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(tree4)

#t1 <- predict(tree1, newdata = test )

#t1_sub <-  data.frame(id = test$id, trip_duration = t1)

#write.csv(t1_sub, file = "t1_sub.csv", row.names = FALSE)

##
tree3 <- rpart(trip_duration ~ vendor_id + passenger_count + haver + store_and_fwd_flag +wd, data = train2, method="anova", control = rpart.control(minsplit =200, cp= 0.001 ))

printcp(tree3)
plotcp(tree3)
print(tree3)
plot(tree3)
text(tree3)
rsq.rpart(tree3)

t3 <- predict(tree3, newdata = test )

t3_sub <-  data.frame(id = test$id, trip_duration = t3)

write.csv(t3_sub, file = "t2_sub.csv", row.names = FALSE)


#

tree4 <- rpart(trip_duration ~ vendor_id + passenger_count + haver + store_and_fwd_flag +wd, data = train2, method="anova", control = rpart.control(minsplit =10, cp= 0.0001 ))

printcp(tree4)
plotcp(tree4)
print(tree4)
plot(tree4)
text(tree4)
rsq.rpart(tree4)

t4 <- predict(tree4, newdata = test )

t4_sub <-  data.frame(id = test$id, trip_duration = t4)

write.csv(t4_sub, file = "t4_sub.csv", row.names = FALSE)

ptree<- prune(tree4, cp= tree4$cptable[which.min(tree4$cptable[,"xerror"]),"CP"])
 fancyRpartPlot(ptree, uniform=TRUE, main="Pruned Classification Tree")

 t5 <- predict(ptree, newdata = test )
 
 t5_sub <-  data.frame(id = test$id, trip_duration = t5)
 
 write.csv(t5_sub, file = "t5_sub.csv", row.names = FALSE)

library(party)
ctree1 <- ctree(trip_duration ~ vendor_id + passenger_count + haver + store_and_fwd_flag +wd, data = train2)


