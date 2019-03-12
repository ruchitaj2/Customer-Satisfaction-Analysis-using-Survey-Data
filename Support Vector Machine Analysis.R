library(magrittr)
library(kernlab)

svm_df <- read.csv("flight_survey_updated.csv", stringsAsFactors = T)

#### used to create csv subset files for all airlines
createAirlineSubset <- function(airlineNames) {
  for(airline in airlineNames) {
    print(airline)
    airlineDf <- svm_df[which(svm_df$airline_name==airline),]
    write.csv(airlineDf, paste(airline,"csv", sep="."),row.names=F)
  }
}

airlineNames <- unique(svm_df$airline_name) %>% levels()
createAirlineSubset(airlineNames)


#### import the Cheapseats dataset

ksvm_df <- read.csv("Cheapseats.csv", stringsAsFactors = F)


#### label Cheapseats airline satisfaction

ksvm_df$satisfactionLabel <- ifelse(ksvm_df$satisfaction>3,"happy","unhappy")
ksvm_df$satisfactionLabel <- factor(ksvm_df$satisfactionLabel)


#### create test/train data

createTestTrain <- function() {
  n = dim(ksvm_df)[1]
  train_index = sample(1:n, size=.7*n, replace = F)  # create random sample with size 70% of n
  train <- ksvm_df[train_index,]  # set train data to the random indices generated 
  test <- ksvm_df[-train_index,] # set test data to exclude the random indices
  return(list(test,train))
}



#### build the svm model

test_train <- createTestTrain()
test <- test_train[[1]]
train <- test_train[[2]]
svm_output <- ksvm(satisfactionLabel~airline_status + age + gender + type_of_travel + arrival_delay_greater_5_mins + num_flights, data=train, model="rfbfot", kpar="automatic", C=10, cross=3, prob.model=T)
svm_output
svm_predict <- predict(svm_output, test, type="votes")
compTable <- data.frame(test[,29], svm_predict[1,])  # creates a dataframe from the from test$classify and the first row in the svm_predict matrix
table(compTable)



#### the svm produces an error rate of 20%, let's see if we can reduce that by removing the variables with less correlation to satisfaction
# "The r-squared value for gender: 0.017487"
# "The r-squared value for age: 0.049643"
# create new test/train data before running

test_train <- createTestTrain()
test <- test_train[[1]]
train <- test_train[[2]]
svm_output2 <- ksvm(satisfactionLabel~airline_status + type_of_travel + arrival_delay_greater_5_mins + num_flights, data=train, model="rfbfot", kpar="automatic", C=10, cross=3, prob.model=T)
svm_output2
svm_predict2 <- predict(svm_output2, test, type="votes")
compTable2 <- data.frame(test[,29], svm_predict2[1,])  # creates a dataframe from the from test$classify and the first row in the svm_predict matrix
table(compTable2)


#### prediction rate does not seem to improve with removing variables; nevertheless, let's remove all but the strongest predictors
# create new test/train data before running

test_train <- createTestTrain()
test <- test_train[[1]]
train <- test_train[[2]]
svm_output3 <- ksvm(satisfactionLabel~airline_status + type_of_travel, data=train, model="rfbfot", kpar="automatic", C=10, cross=3, prob.model=T)
svm_output3
svm_predict3 <- predict(svm_output3, test, type="votes")
compTable3 <- data.frame(test[,29], svm_predict3[1,])  # creates a dataframe from the from test$classify and the first row in the svm_predict matrix
table(compTable3)



