library(MASS)

#### TODO:
# Winsorize departure_delay_in_minutes and arrival_delay_in_minutes to handle outliers (done)
# Create scatter plots for variables, w/ abline
# Get correlation coefficints for all variables

#### import data from cleaning step
df <- read.csv("Cheapseats.csv", stringsAsFactors = T)


#### summary of analysis
# The independent variables: airline_status + age + gender + type_of_travel + arrival_delay_greater_5_mins + num_flights product a linear model that can explain 44% of the variance around the mean of the dependent variable "satisfaction""
# "Silver" status customers have the highest satisfaction ratings
# "Business" travelors have the highest satisfaction ratings 
# "Males" have higher ratings than females 
# Customers with delays less than five minutes have higher satisfaction ratings
# *All interpretations have the condition of keeping all other independent variables constant*

#### Refined business quesitons
# Why are Silver status customers more satisfied than the higher Platinum and Gold counterparts?
# Why are Males more satisfied than females?

#### Winsorize departure_delay_in_minutes and arrival_delay_in_minutes
# see how many rows fall above the 95th percentile

departure_delay_95 <- quantile(df$departure_delay_in_minutes, .95)
sprintf("%d rows will be changed to the 95th percentile of departure_delay_in_minutes", length(which(df$departure_delay_in_minutes > departure_delay_95)))
df[which(df$departure_delay_in_minutes > departure_delay_95), "departure_delay_in_minutes"] = departure_delay_95

arrival_delay_95 <- quantile(df$arrival_delay_in_minutes, .95)
sprintf("%d rows will be changed to the 95th percentile of arrival_delay_in_minutes", length(which(df$arrival_delay_in_minutes > arrival_delay_95)))
df[which(df$arrival_delay_in_minutes > arrival_delay_95),"arrival_delay_in_minutes"] = arrival_delay_95



#### get the r-squared values for all the variables as predictors of satisfaction

createLM <- function(mydf) {
  r_squares <- list()
  for(i in 2:ncol(mydf)) {
  model <- lm(formula=satisfaction~mydf[[i]], data=mydf)
  r_squares[[i]] <- summary(model)
  }

  model_names <- c('satisfaction','airline_status', 'age', 'gender', 'price_sensitivity', 'year_first_flight','num_flights', 'percent_flight_other_airlines', 'type_of_travel','num_loyalty_cards', 'airport_shopping', 'airport_dining','class', 'day_of_month', 'flight_date','origin_city', 'origin_state', 'destination_city','destination_state', 'scheduled_departure_hour', 'departure_delay_in_minutes', 'arrival_delay_in_minutes', 'flight_cancelled','flight_time_in_minutes', 'flight_distance', 'arrival_delay_greater_5_mins')

  names(r_squares) <- model_names  # name lists 

  return(r_squares)
}

df <- df[,-which(colnames(df)=="airline_code")]
df <- df[,-which(colnames(df)=="airline_name")]
r_squares <- createLM(df)

for(i in 2:length(r_squares)) {
  v_name <- names(r_squares)[i]
  print(sprintf("The r-squared value for %s: %f", v_name, r_squares[[i]]$r.squared))
}

# winsorizing the departure/arrival delays changed the coefficients from *.005 and .007* to *.010 and .014* respectively


#### perform stepwise regression (backwards) to find best predictors
model <- lm(satisfaction~airline_status+age+gender+type_of_travel+arrival_delay_greater_5_mins+num_flights+arrival_delay_in_minutes+departure_delay_in_minutes+num_loyalty_cards, data=df)
selected_model <- stepAIC(model, direction="backward", trace=TRUE)
summary(selected_model)


#### Interpretation of Adj R-squared and coefficients
# This model explains 44% (Adj. R-squared value) of the variance around the mean of the dependent variable satisfaction
# Customers who are either Gold, Platinum, or Silver have satisfaction ratings of (.44, .25, and .62) higher than customers that don't have a flight_status
# The "Silver" status has the strongest positive correlation with customer satisfaction amongst the airline status categories.  Its' coefficient is interpreted as, "for Silver status, and holding all other variables constant, the satisfaction score is .62 higher for Silver flyers than for all other airline_status categories"
# Holding all other variables constant, male customers' satisfaction scores are .13 higher than female customers
# Holdling all other variables constant, customers traveling for "Mileage" have satisfaction scores -.15 less than customers traveling for "Business"
# Holding all other variables constant, customers traveling for "Personal" have satisfaction scores -1.1 less than customers traveling for "Business"
# Holding all other variables constant, customers with flights delays greater than 5 minutes have satisfaction scores  -.33 less than customers with flight delays less than 5 minutes


#### reorder the factors for airline_status and type_of_travel to see what the coefficients are for the current reference variables "Blue","Business"

df$airline_status <- factor(df$airline_status, levels=c("Silver","Gold","Platinum", "Blue"))
df$type_of_travel <- factor(df$type_of_travel, levels=c("Personal","Business","Mileage"))
# create the model again 
model <- lm(satisfaction~airline_status+age+gender+type_of_travel+arrival_delay_greater_5_mins+num_flights+arrival_delay_in_minutes+departure_delay_in_minutes+num_loyalty_cards, data=df)
selected_model <- stepAIC(model, direction="backward", trace=TRUE)
summary(selected_model)


#### According to google :), a lower AIC is better, so the last "Step" is the best model:
# satisfaction ~ airline_status + age + gender + type_of_travel + arrival_delay_greater_5_mins + num_flights (best linear model)
# this model explains 44% of the variance around the mean of the dependent variable satisfaction







