

library(ggplot2)
library(dplyr)

#### Import cleaned dataset 

df <- read.csv("flight_survey_updated.csv", stringsAsFactors=T)
df$satisfaction <- factor(df$satisfaction)
df$year_first_flight <- factor(df$year_first_flight)
df$num_loyalty_cards <- factor(df$num_loyalty_cards)


#### Summary of anlayis
# Higher ratings are centered around 40 year old customers
# Older customers (80 and above) and younger customers (20 and below) give lower ratings
# Cheapseats is assumed to be the busiest airline (most flights); they have the most observations in the dataset
# Cool&Young is assumed to be the least busiest airline (least flights); they have the least amount of observations in the dataset
# departure_delay_in_minutes and arrival_delay_in_minutes are heavily right skewed; their outliers should be handled before building the linear model. recommend using the winsor method to set the upper limit to a chose percentile (i.e. 95%)
# most flyers do not have a loyalty card, and most that do have between 1-5. 

#### Revised business questions
# There are `r which(df$num_loyalty_cards == 1) %>% length()` with only 1 loyalty card.  Are they more/less satisfied with those with 0 or greater than 1?
  # There are `r which(df$num_loyalty_cards >=1 & df$num_loyalty_cards<=5) %>% length()` with greater than 1 loyalty card.  Are they more/less satisfied?
  # Who has higher ratings, males or females?
  # Why do customers centered around the age of 40 have higher ratings?
  # Why do older customers (80 and above) and younger customers (20 and below) give lower ratings?
  
  #### TODO
  # Pivot tables or dplyr summary statistics

##### Plost histograms using createHist(<vector of column names>)

library(magrittr)
getBinWidth <- function(v) {  
  my_min <- min(v)
  my_max <- max(v)
  l <- length(unique(v))
  return((my_max - my_min)/sqrt(l))
}

createIntHist <- function(df,cols,airlineName) {
  for(c in cols) {
    myVector <- df[[c]]  # df must be in local environment
    bins_binwidth <- getBinWidth(myVector)  # call getBinWidth() to get binwidth argument value
    g <- ggplot(df) +  # create ggplot instance 
      geom_histogram(aes(x=myVector), binwidth=bins_binwidth, color="black", fill="white") +
      labs(title=airlineName, x=c) +
      theme(plot.title = element_text(hjust=0.5)) 
    
    # from github: https://stackoverflow.com/questions/47000494/how-to-add-mean-and-mode-to-ggplot-histogram
    data<- g %+% ggplot_build(g)$data  
    hist_peak<-data[[1]]%>%filter(y==max(y))%>%.$x
    
    g <- g + geom_vline(aes(xintercept=mean(myVector)),col="black", lwd=1, linetype=2)
    g <- g + annotate("text", label=round(hist_peak,1), x=hist_peak, y=0,vjust="bottom",col='orange',size=5)
    g <- g + annotate("text", label=round(mean(myVector),1), x=round(mean(myVector),1), y=0,vjust=-1,col='red',size=5)
    print(g)
  }
}


#### plot all continuous variables for df

createIntHist(df,c("num_flights","percent_flight_other_airlines","departure_delay_in_minutes","arrival_delay_in_minutes","flight_time_in_minutes","flight_distance","airport_shopping","airport_dining"),"All Airlines")



# The dashed line with red number is the mean satisfaction rating and the orange number is the mode (most frequent rating)

#### Create barplots for factor datatypes


createFactorBarPlot <- function(df,cols,airlineName) {
  for(col in cols) {
    myVector <- df[[col]]
    if(col=="airline_name") {
      g <- ggplot(df, aes(x=myVector)) +
        geom_bar(color="black",fill="white") +
        geom_text(stat="count", aes(x=myVector, label=..count..), vjust=0) +
        labs(title=col, x=col) +
        theme(axis.text.x = element_text(angle=90, vjust=0)) 
      print(g)
    } else {
      g <- ggplot(df, aes(x=myVector)) +
        geom_bar(color="black",fill="white") +
        geom_text(stat="count", aes(x=as.numeric(myVector), label=..count..), vjust=0) +
        labs(title=airlineName, x=col)
      print(g)
    }
  }
  #return(data)
}


#### create barplots for discrete variables

createFactorBarPlot(df,c("satisfaction","gender","type_of_travel","class","airline_name","flight_cancelled","year_first_flight","num_loyalty_cards","airline_code"),"All Airlines")



#### Analyze satisfaction by age


#df$satisfaction <- factor(df$satisfaction, levels=c(1,2,3,4,5), ordered = T)
g <- ggplot(df, aes(x=age,y=factor(satisfaction))) +
  geom_col(aes(fill=satisfaction))
g


# looks like the distribution for satisfied customers is centered around 40 year olds



g <- ggplot(df, aes(x=type_of_travel)) +
  geom_bar(aes(fill=gender)) +
  geom_text(stat="count", aes(x=type_of_travel, label=..count..), vjust=0) 
g


# Males and females fly almost equally for Business and Mileage trips, but females fly more for Personal travel

#### creat histogram of satisfaction by airline

createSatHist <- function(airlineDf, airlineName) {
  airlineDf$satisfaction <- airlineDf$satisfaction %>% as.numeric()
  g <- ggplot(airlineDf) +  # create ggplot instance 
    geom_bar(aes(x=satisfaction), color="black", fill="white") +
    geom_text(stat="count", aes(x=satisfaction, label=..count..), vjust=0) +
    scale_x_discrete(limits=c(1,2,3,4,5)) +
    labs(title=airlineName, x=airlineName, y="satisfaction") +
    theme(plot.title = element_text(hjust=0.5)) 
  
  data <- g %+% ggplot_build(g)$data  
  hist_peak<-data[[1]]%>%filter(y==max(y))%>%.$x
  
  g <- g + geom_vline(aes(xintercept=mean(airlineDf$satisfaction)),col="black", lwd=1, linetype=2)
  g <- g + annotate("text", label=round(hist_peak,1), x=hist_peak,y=0,vjust="bottom",col='orange',size=5)
  g <- g + annotate("text", label=round(mean(airlineDf$satisfaction),1), x=round(mean(airlineDf$satisfaction),1), y=0,vjust=-1,col='red',size=5)       
  # get the ratio of 4&5 ratings/all ratings
  good_ratings_ratio <- round(length(which(airlineDf$satisfaction >= 4))/nrow(airlineDf),4)
  #cat(sprintf("%s: %.4f\n", airlineName, good_ratings_ratio))
  g <- g + annotate("text", label="High ratings ratio:", x=min(airlineDf$satisfaction), y=length(which(airlineDf$satisfaction==4)))
  g <- g + annotate("text", label=good_ratings_ratio, x=min(airlineDf$satisfaction)+1, y=length(which(airlineDf$satisfaction==4)))
  print(g)
}


#### get satisfaction barplot for all airlines


createSatHist(df, "All Airlines")



#### create histogram for all airline ratings
+ The dashed line with red number is the mean satisfaction rating and the orange number is the mode (most frequent rating)


airlineNames <- c('Cheapseats', 'Cool&Young', 'EnjoyFlying', 'FlyFast', 'FlyHere', 'FlyToSun', 'GoingNorth', 'Northwest', 'OnlyJets', 'Oursin', 'PaulSmith', 'Sigma', 'Southeast', 'West')
for(airline in airlineNames) {
  airlineDf <- df[which(df$airline_name==airline),]
  airlineDf$satisfaction <- as.numeric(airlineDf$satisfaction)
  createSatHist(airlineDf, airline)
}


#### get male/female customer count for CheapAirlines


cheap_male_female <- df %>% dplyr::select(airline_name,gender) %>% dplyr::filter(airline_name=="Cheapseats") %>% dplyr::group_by(gender) %>% dplyr::summarise(count=n())
g <- ggplot(cheap_male_female, aes(x=gender,y=count)) +
  geom_bar(stat="identity", color="black",fill="white") +
  labs(title="Cheapseats gender distribution", x="Gender") 
g


# The dashed line with red number is the mean satisfaction rating and the orange number is the mode (most frequent rating)
# GoingNorth and OnlyJets have the lowest high ratings ratio at 48% and 49% respectively 
# West and Cool&Young have the highest high ratings ratio at 57% and 55% respectively, but they are are the smallest airlines

#### Business quesitons
# Who has higher ratings, males or females?
  # Why do customers centered around the age of 40 have higher ratings?
  # Why do older customers (80 and above) and younger customers (20 and below) give lower ratings?
  
  #### how many female, blue, economy, personal travelers are there?
  
  
df %>% dplyr::select(satisfaction,airline_name,gender,airline_status,type_of_travel) %>%
  dplyr::filter(gender=="Female",airline_name=="Cheapseats",airline_status=="Blue",type_of_travel=="Personal") %>%
  dplyr::group_by(satisfaction) %>%
  dplyr::summarise(count=n()) %>%
  dplyr::arrange(satisfaction)


#### how many male, blue, economy, personal travelers are there?


df %>% dplyr::select(satisfaction,airline_name,gender,airline_status,type_of_travel) %>%
  dplyr::filter(gender=="Male",airline_name=="Cheapseats",airline_status=="Blue",type_of_travel=="Personal") %>%
  dplyr::group_by(satisfaction) %>%
  dplyr::summarise(count=n()) %>%
  dplyr::arrange(satisfaction)


#### how many blue, cheapseats, personal travelers are there in total

df %>% dplyr::select(satisfaction,airline_name,gender,airline_status,type_of_travel) %>%
  dplyr::filter(airline_name=="Cheapseats",airline_status=="Blue",type_of_travel=="Personal") %>%
  dplyr::group_by(satisfaction) %>%
  dplyr::summarise(count=n()) %>%
  dplyr::arrange(satisfaction)



#### run summary statistics for Cheapseats
# factor variables

cheapseatsDf <- df[which(df$airline_name=="Cheapseats"),]
createFactorBarPlot(cheapseatsDf,c("airline_status","satisfaction","gender","type_of_travel","class","flight_cancelled","year_first_flight","num_loyalty_cards","airline_code"),"Cheapseats")


#### get blue status by gender

g <- ggplot(cheapseatsDf, aes(x=airline_status)) +
  geom_bar(aes(fill=gender)) +
  geom_text(stat="count", aes(x=airline_status, label=..count..), vjust=0) +
  labs(title="Cheapseats status/gender")
g


#### get loyalty cards by gender


g <- ggplot(cheapseatsDf, aes(x=num_loyalty_cards)) +
  geom_bar(aes(fill=gender)) +
  geom_text(stat="count", aes(x=num_loyalty_cards, label=..count..), vjust=0) +
  labs(title="Cheapseats loyalty/gender")
g


#### cheapseats satisfaction/gender


g <- ggplot(cheapseatsDf, aes(x=satisfaction)) +
  geom_bar(aes(fill=gender)) +
  geom_text(stat="count", aes(x=satisfaction, label=..count..), vjust=0) +
  labs(title="Cheapseats satisfaction/gender")
g


#### cheapseats satisfaction/status

g <- ggplot(cheapseatsDf, aes(x=satisfaction)) +
  geom_bar(aes(fill=airline_status)) +
  geom_text(stat="count", aes(x=satisfaction, label=..count..), vjust=0) +
  theme_bw() +
  scale_fill_manual("airline_status", values = c("Blue" = "dodgerblue", "Gold" = "gold", "Platinum" = "slategrey", "Silver" = "grey89")) +
  labs(title="Cheapseats satisfaction/status")
g



# continuous variables

createIntHist(cheapseatsDf,c("num_flights","percent_flight_other_airlines","departure_delay_in_minutes","arrival_delay_in_minutes","flight_time_in_minutes","flight_distance","airport_shopping","airport_dining"),"Cheapseats")


#### Winsorize departure_delay_in_minutes and arrival_delay_in_minutes
# see how many rows fall above the 95th percentile

departure_delay_95 <- quantile(df$departure_delay_in_minutes, .95)
sprintf("%d rows will be changed to the 95th percentile of departure_delay_in_minutes", length(which(df$departure_delay_in_minutes > departure_delay_95)))
df[which(df$departure_delay_in_minutes > departure_delay_95), "departure_delay_in_minutes"] = departure_delay_95

arrival_delay_95 <- quantile(df$arrival_delay_in_minutes, .95)
sprintf("%d rows will be changed to the 95th percentile of arrival_delay_in_minutes", length(which(df$arrival_delay_in_minutes > arrival_delay_95)))
df[which(df$arrival_delay_in_minutes > arrival_delay_95),"arrival_delay_in_minutes"] = arrival_delay_95



