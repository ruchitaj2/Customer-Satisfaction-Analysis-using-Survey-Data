
#### TODO

#### import data and change column names

df = read.csv("satisfaction_survey.csv", stringsAsFactors=F)

new_colnames <- c('satisfaction', 'airline_status', 'age', 'gender', 'price_sensitivity', 'year_first_flight', 'num_flights', 'percent_flight_other_airlines', 'type_of_travel', 'num_loyalty_cards', 'airport_shopping', 'airport_dining', 'class', 'day_of_month', 'flight_date', 'airline_code', 'airline_name', 'origin_city', 'origin_state', 'destination_city', 'destination_state', 'scheduled_departure_hour', 'departure_delay_in_minutes', 'arrival_delay_in_minutes', 'flight_cancelled', 'flight_time_in_minutes', 'flight_distance', 'arrival_delay_greater_5_mins')

colnames(df) <- new_colnames

#### Check for leading and trailing whitespace for every vector in df

test_blanks <- c(paste(" ", 1:129889, " "))  # create an array of characters with spaces at the front and end
df <- cbind(df, test_blanks)  # add test array to df
sapply(df, function(x) sum(grepl("^\\s*.*\\s+$", x)))  # find all vectors in df with leading/trailing spaces

# Looks like our test case and <span style="color:orange">airline_name</span> has leading/trailing spaces; let's deal with the whitespace in *airline_name*



df$airline_name = trimws(df$airline_name)
colsKeep <- c('satisfaction', 'airline_status', 'age', 'gender', 'price_sensitivity', 'year_first_flight', 'num_flights', 'percent_flight_other_airlines', 'type_of_travel', 'num_loyalty_cards', 'airport_shopping', 'airport_dining', 'class', 'day_of_month', 'flight_date', 'airline_code', 'airline_name', 'origin_city', 'origin_state', 'destination_city', 'destination_state', 'scheduled_departure_hour', 'departure_delay_in_minutes', 'arrival_delay_in_minutes', 'flight_cancelled', 'flight_time_in_minutes', 'flight_distance', 'arrival_delay_greater_5_mins')
df <- df[,colsKeep]

# whitespace in *airline_name* is gone, run the sapply() command from above to verify

#### Look for anomalies by reviewing the unique values of the variables
# you're looking for anomalies--specifically in the levels

unique(df$satisfaction)

#### Satisfaction column has strange values, *"4.00.5"* and *"4.00.2.00"*. We can find how many rows have these values with:

which(df$satisfaction=="4.00.5")
which(df$satisfaction=="4.00.2.00")

#### Considering there are only **`r length(which(df$satisfaction=="4.00.5")) + length(which(df$satisfaction=="4.00.2.00")) `** rows with the eroneous data, let's delete them

library(magrittr)
drop_rows <- c(38898, 38899, 38900)
df <- df[-drop_rows,]
df$satisfaction %<>% as.numeric  # covert vector to numeric

# The dataset now has `r nrow(df)` rows after dropping the 3 with bad data

#### Find all NA values in df

sapply(df, function(x) sum(is.na(x)))

# Flight_time_in_minutes, departure_delay_in_minutes, and arrival_delay_in_minutes have NA values.  How should we deal with them? Our options are:
# Drop the entire row
# Set them to the average of the vector 

#### Set NA values to average of column

flight_time_in_minutes_avg <- mean(df$flight_time_in_minutes, na.rm=T)
departure_delay_in_minutes_avg <- mean(df$departure_delay_in_minutes, na.rm=T)
arrival_delay_in_minutes_avg <- mean(df$arrival_delay_in_minutes, na.rm=T)

df[which(is.na(df$flight_time_in_minutes)),"flight_time_in_minutes"] = round(flight_time_in_minutes_avg,1)
df[which(is.na(df$departure_delay_in_minutes)),"departure_delay_in_minutes"] = round(departure_delay_in_minutes_avg,1)
df[which(is.na(df$arrival_delay_in_minutes)),"arrival_delay_in_minutes"] = round(arrival_delay_in_minutes_avg,1)



#### Change the *flight_date* column to Date datatype.

df$flight_date <- as.Date(df$flight_date, "%m/%d/%y")

# date if formated: YYYY-mm-dd (i.e. 2014-03-18)

#### The arrangment of the categories does not follow an ordinal hierachy such as lowest to highest (i.e. Eco, Eco Plus, Business).  We will make the following changes:
#1. Change the values for the *type_of_travel* variable to the more concise names of: "Personal, Mileage, Business"
# Convert the *type_of_travel* variable to a factor with factor order of: "Personal, Mileage, Business"
#2. Change the values for the *class* variable to the more concise names of: "Economy, Plus, Business"
# Set the order of the factors for the *class* variable to: "Economy, Plus, Business"
#3. Change *gender* to a factor and change the factor order to: "Male, Female"
#4. Change *airline_status* to an "unordered" factor: "Blue, Silver, Gold, Platinum"
#5. Change *airline_name* to more consise names of: "'Cheapseats', 'Cool&Young', 'EnjoyFlying', 'FlyFast', 'FlyHere', 'FlyToSun', 'GoingNorth', 'Northwest', 'OnlyJets', 'Oursin', 'PaulSmith', 'Sigma', 'Southeast', 'West'"
#6. Change *flight_cancelled* to a factor with order of: "Yes, No"
#7. Change *arrival_delay_greater_5_mins* to a factor with order: "yes", "no"


# 1. Change the values for the *class* variable to the more concise names of: "Economy, Plus, Business"
df[which(df$type_of_travel=="Personal Travel"), "type_of_travel"] = "Personal"
df[which(df$type_of_travel=="Mileage tickets"), "type_of_travel"] = "Mileage"
df[which(df$type_of_travel=="Business travel"), "type_of_travel"] = "Business"
# change to factor and set order
df$type_of_travel <- factor(df$type_of_travel, levels=c("Personal","Mileage","Business"), ordered=TRUE)




# 2. Change the values for the *class* variable to the more concise names of: "Economy, Plus, Business"
df[which(df$class=="Eco"), "class"] = "Economy"
df[which(df$class=="Eco Plus"), "class"] = "Plus"
df$class <- factor(df$class, levels=c("Economy", "Plus", "Business"), ordered=TRUE)



# 3. Change *gender* to a factor and change the factor order to: "Male, Female"
df$gender <- factor(df$gender, levels=c("Male", "Female"), ordered=TRUE)



# 4. Change *airline_status* to a factor with order of: "Blue, Silver, Gold, Platinum"
df$airline_status <- factor(df$airline_status, levels=c("Blue", "Silver", "Gold", "Platinum"), ordered=TRUE)


#### 5. Change *airline_name* to more concise names of: "'Cheapseats', 'Cool&Young', 'EnjoyFlying', 'FlyFast', 'FlyHere', 'FlyToSun', 'GoingNorth', 'Northwest', 'OnlyJets', 'Oursin', 'PaulSmith', 'Sigma', 'Southeast', 'West'"


df[which(df$airline_name=="Cheapseats Airlines Inc."), "airline_name"] = "Cheapseats"
df[which(df$airline_name=="Cool&Young Airlines Inc."), "airline_name"] = "Cool&Young"
df[which(df$airline_name=="EnjoyFlying Air Services"), "airline_name"] = "EnjoyFlying"
df[which(df$airline_name=="FlyFast Airways Inc."), "airline_name"] = "FlyFast"
df[which(df$airline_name=="FlyHere Airways"), "airline_name"] = "FlyHere"
df[which(df$airline_name=="FlyToSun Airlines Inc."), "airline_name"] = "FlyToSun"
df[which(df$airline_name=="GoingNorth Airlines Inc."), "airline_name"] = "GoingNorth"
df[which(df$airline_name=="Northwest Business Airlines Inc."), "airline_name"] = "Northwest"
df[which(df$airline_name=="OnlyJets Airlines Inc."), "airline_name"] = "OnlyJets"
df[which(df$airline_name=="Oursin Airlines Inc."), "airline_name"] = "Oursin"
df[which(df$airline_name=="Paul Smith Airlines Inc."), "airline_name"] = "PaulSmith"
df[which(df$airline_name=="Sigma Airlines Inc."), "airline_name"] = "Sigma"
df[which(df$airline_name=="Southeast Airlines Co."), "airline_name"] = "Southeast"
df[which(df$airline_name=="West Airways Inc."), "airline_name"] = "West"

new_airline_names <- c('Cheapseats', 'Cool&Young', 'EnjoyFlying', 'FlyFast', 'FlyHere', 'FlyToSun', 'GoingNorth', 'Northwest', 'OnlyJets', 'Oursin', 'PaulSmith', 'Sigma', 'Southeast', 'West')

df$airline_name <- factor(df$airline_name, levels=new_airline_names, ordered=TRUE)


#### 6. Change *flight_cancelled* to a factor with order of: "Yes, No"

df$flight_cancelled <- factor(df$flight_cancelled, levels=c("Yes","No"), ordered=TRUE)


#### 7. Change *arrival_delay_greater_5_mins* to a factor with order: "yes", "no"

df$arrival_delay_greater_5_mins <- factor(df$arrival_delay_greater_5_mins, levels=c("yes","no"), ordered=TRUE)

#### Drop the outlier rows with percent_flight_other_airlines > 50

df <-  df[-which(df$percent_flight_other_airlines > 50),]

# dropped 451 of outliers data

#### Round up the values for satisfaction with decimal values

table(df$satisfaction)
df[which(df$satisfaction==2.5), "satisfaction"] = 3
df[which(df$satisfaction==3.5), "satisfaction"] = 4
df[which(df$satisfaction==4.5), "satisfaction"] = 5
table(df$satisfaction)


### Ending row length is: `r nrow(df)`, and number of columns is: `r length(df)`

#### Write csv to file

write.csv(df, "flight_survey_updated.csv", row.names=F)

