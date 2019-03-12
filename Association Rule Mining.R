library(magrittr)
library(arules)
library(arulesViz)
library(dplyr)

### Summary of analysis
#### The various rulesets created returned the following trends:
# There are relatively few customers that give a rating of 5, but those that do have the following attributes: male, business trip, economy class, flight not cancelled, price_sensitivity=1
# Female business travelers--while few--also give higher ratings (4-5).  They expect no cancellations and low delays.  No female business travelers were Blue status in any of the rules; this correlates well with the negative coefficient for Blue status in the linear model. 
# Female Blue status customers, traveling for personal reasons in the economy class give the lowest ratings

#### Miscellaneous Observations
# Surprisingly loyalty cards don't seem to influence higher satisfaction ratings. Loyalty cards were hardly present in pairings, and when they were, it was for 0 cards.

#### TODO
# Add the proportion of travelers in the assessment to increase the impact of the assessment.  Who cares about the assesment if it accounts for only a small proportion of the population

#### Unanswered business questions
# What locations recieve the best/worst ratings? (done)


#### import data

df <- read.csv("Cheapseats.csv", stringsAsFactors = T)

#### discretize the following values
# cut returns a factor with the levels specified in the array indicated with (start, finish]; start is exclusive, finish is inclusive

df$satisfaction <- cut(df$satisfaction, c(0,1,2,3,4,5))
df$age <- cut(df$age, c(14,34,46,58,86)) 
df$price_sensitivity <- cut(df$price_sensitivity, c(-1,0,1,2,3,4))
df$year_first_flight <- cut(df$year_first_flight, breaks=3)
df$num_flights <- cut(df$num_flights, c(-1,0,9,18,21,30,94))
df$percent_flight_other_airlines <- cut(df$percent_flight_other_airlines, c(0,4,7,10,51))
df$num_loyalty_cards <- cut(df$num_loyalty_cards, c(-1,0,1,2,3,8))
df$airport_shopping <- cut(df$airport_shopping, c(-1,0,27,30,745))
df$airport_dining <- cut(df$airport_dining, c(-1,0,31,61,69,91,765))
df$day_of_month <- cut(df$day_of_month, c(0,1,9,17,24,32))
df$scheduled_departure_hour <- cut(df$scheduled_departure_hour, c(0,1,10,14,18,23))
df$flight_time_in_minutes <- cut(df$flight_time_in_minutes, c(0,27,59,87,102,360))
df$flight_distance <- cut(df$flight_distance, c(0,148,365,587,710,957,2329))


#### remove arrival_delay_in_minutes column

df <- df[,-which(colnames(df)=="arrival_delay_in_minutes")]


#### check for NA values

sapply(df, function(x) sum(is.na(x)))


#### change departure_delay_in_minutes to a factor of yes or no if the departure delay is greater than 15 minutes
# change the name of the column to departure_delay_greater_5_mins
# 11.7 came from theh industry average for departure delay


df$departure_delay_in_minutes <- ifelse(df$departure_delay_in_minutes > 11.7,"yes","no")
colnames(df)[which(colnames(df)=="departure_delay_in_minutes")] = "departure_delay_greater_5_mins"
df$departure_delay_greater_5_mins <- factor(df$departure_delay_greater_5_mins)


#### create sparse matrix from df


dfX <- as(df, "transactions")


#### inspect the first 2 rows from the matrix

inspect(dfX[1:2,])

#### show the item frequency to see which categories are present the most in the matrix

itemFrequencyPlot(dfX, support=.1, cex.names=.7)


### Let's analyze flyers by the ratings groups of 1-5

#### customer satisfaction rating = 1
ruleset <- apriori(dfX, parameter=list(support=.01, confidence=.5, minlen=2, maxlen=6, maxtime=10),
                   appearance = list(lhs="satisfaction=(0,1]"))
plot(ruleset)

ruleset_filter <- subset(ruleset, subset=lhs %ain% c("satisfaction=(0,1]") & lift>1)
# plot(ruleset_filter)
# summary(ruleset_filter)
ruleset_filter <- sort(ruleset_filter, decreasing=T, by="count")
inspect(ruleset_filter)
plot(ruleset_filter, method="graph")


# there are relatively few customers who give a customer satisfaction rating of 1, but those that do have one or more of the following attributes: female, personal travel, arrivel delay > 5 mins, blue status, economy class, 0 loyalty cards, and 0 airport_shopping


#### customer satisfaction rating = 2

ruleset <- apriori(dfX, parameter=list(support=.1, confidence=.5, minlen=2, maxtime=10),
                   appearance = list(lhs="satisfaction=(1,2]"))

ruleset_filter <- subset(ruleset, subset=lhs %ain% c("satisfaction=(1,2]") & lift>1)
#plot(ruleset_filter)
summary(ruleset_filter)
ruleset_filter <- sort(ruleset_filter, decreasing=T, by="count")
inspect(ruleset_filter)
plot(ruleset_filter, method="graph")


# Virtually the same result from customers with ratings=1; female, personal travel, blue status, economy class, 0 loyalty cards, and 0 airport_shopping

#### customer satisfaction rating = 3

ruleset <- apriori(dfX, parameter=list(support=.1, confidence=.5, minlen=2, maxlen=6, maxtime=10),
                   appearance = list(lhs="satisfaction=(2,3]"))
#plot(ruleset)

ruleset_filter <- subset(ruleset, subset=lhs %ain% c("satisfaction=(2,3]") & lift>1)
plot(ruleset_filter)
summary(ruleset_filter)
ruleset_filter <- sort(ruleset_filter, decreasing=T, by="count")
inspect(ruleset_filter)
plot(ruleset_filter, method="graph")


# Similar assessment to customers with rating=2 *except* for one key difference: arrival and departure delays are NOT greater than 5 mins

#### customer satisfaction rating = 4
ruleset <- apriori(dfX, parameter=list(support=.05, confidence=.5, minlen=2, maxtime=10),
                   appearance = list(lhs="satisfaction=(3,4]"))
#plot(ruleset)

ruleset_filter <- subset(ruleset, subset=lhs %ain% c("satisfaction=(3,4]") & lift>1)
plot(ruleset_filter)
summary(ruleset_filter)
ruleset_filter <- sort(ruleset_filter, decreasing=T, by="count")
inspect(ruleset_filter)
plot(ruleset_filter, method="graph")


# Customers with one or more of the following attributes (business, departure/arrival delay < 5 min, flight not cancelled, price_sensitivity=1) give ratings=4

#### male satisfaction = 4
male_ruleset <- apriori(dfX, parameter=list(support=.1, confidence=.8, minlen=2, maxlen=6, maxtime=20))

male_filter <- subset(male_ruleset, subset=lhs %ain% c("gender=Male","satisfaction=(3,4]") & lift>1.3)
plot(male_filter)
summary(male_filter)
inspect(male_filter[1:20,])

#### customer satisfaction rating = 5
ruleset <- apriori(dfX, parameter=list(support=.01, confidence=.5, minlen=2, maxlen=6, maxtime=10),
                   appearance = list(lhs="satisfaction=(4,5]"))

ruleset_filter <- subset(ruleset, subset=lhs %ain% c("satisfaction=(4,5]") & lift>1)
plot(ruleset_filter)
summary(ruleset_filter)
ruleset_filter <- sort(ruleset_filter, decreasing=T, by="count")
inspect(ruleset_filter)
plot(ruleset_filter, method="graph")

# Customers with one or more of the following attributes give a rating=5: male, business trip, economy class, price sensitivity=1, flight not cancelled




