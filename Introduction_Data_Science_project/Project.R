#Read CSV into R
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")

library(readxl)
library(ggplot2)
library(dplyr)
install.packages("rjson")
library(rjson)

unclean_data<-data.frame(read.csv(file.choose()), stringsAsFactors=FALSE)
unclean_data$Satisfaction<-as.numeric(as.character(unclean_data$Satisfaction))

str(unclean_data)


#Number of missing values
sum(is.na(unclean_data$Departure.Delay.in.Minutes))
sum(is.na(unclean_data$Arrival.Delay.in.Minutes))
sum(is.na(unclean_data$Flight.time.in.minutes))
sum(is.na(unclean_data$Satisfaction))

#Summary of the three variables with missing variables
summary(unclean_data$Departure.Delay.in.Minutes)
summary(unclean_data$Arrival.Delay.in.Minutes)
summary(unclean_data$Flight.time.in.minutes)
summary(unclean_data$Satisfaction)

#min and 1Q and median are 0 so most values are 0, max are outliers (only for departure and arrival)
#median is better to use to replace the NAs, bc median is not sensitive to outliers

#Replacing the NA's
unclean_data$Departure.Delay.in.Minutes[is.na(unclean_data$Departure.Delay.in.Minutes)] <- median(unclean_data$Departure.Delay.in.Minutes, na.rm=TRUE)
unclean_data$Arrival.Delay.in.Minutes[is.na(unclean_data$Arrival.Delay.in.Minutes)] <- mean(unclean_data$Arrival.Delay.in.Minutes, na.rm=TRUE)
unclean_data$Flight.time.in.minutes[is.na(unclean_data$Flight.time.in.minutes)] <- median(unclean_data$Flight.time.in.minutes, na.rm=TRUE)
unclean_data$Satisfaction[is.na(unclean_data$Satisfaction)] <- mean(unclean_data$Satisfaction, na.rm=TRUE)

#Summary of the three variables after removing the missing variables
summary(unclean_data$Departure.Delay.in.Minutes)
summary(unclean_data$Arrival.Delay.in.Minutes)
summary(unclean_data$Flight.time.in.minutes)
summary(unclean_data$Satisfaction)

data <- as.data.frame(unclean_data)
data$effectiveDelay<-abs(data$Departure.Delay.in.Minutes - data$Arrival.Delay.in.Minutes)


install.packages("ggthemes")
install.packages("extrafont")
install.packages("plyr")
install.packages("scales")

library("ggthemes")
library("extrafont")
library("plyr")
library("scales")

#day of month vs. satisfaction
#days at end of month seem to be less satisfied
summary(data$Day.of.Month)
plot14 <- ggplot(data=data, aes(x=Day.of.Month, y=Satisfaction)) + geom_bar(stat='identity')
plot14

#flight date
summary(data$Flight.date)
plot15 <- ggplot(data=data, aes(x=Flight.date, y=Satisfaction)) + geom_bar(stat='identity')  + theme(axis.text.x = element_text(angle=90))
plot15

#airline code
summary(data$Airline.Code)

counts <- table(data$Satisfaction, data$Airline.Code)

plot16 <- barplot(counts, main="Satisfaction depending on Airline Code",
        col=c("darkblue","purple", "yellow", "orange", "pink", "chartreuse", "brown1", "darkgoldenrod1", "burlywood3", "darkslategray2", "azure1", "bisque1", "blueviolet", "goldenrod4"), las=2,
        legend=rownames(counts), args.legend = list(x = "top", ncol=3, inset=c(0, 0), cex=.6, title = "Satisfaction Rating"))

#airline name
summary(data$Airline.Name)

counts2 <- table(data$Satisfaction, data$Airline.Name)
counts2
plot17 <- barplot(counts2, main="Satisfaction depending on Airline Name",
        col=c("darkblue","purple", "yellow", "orange", "pink", "chartreuse", "brown1", "darkgoldenrod1", "burlywood3", "darkslategray2", "azure1", "bisque1", "blueviolet", "goldenrod4"), 
        las=2,
        cex.names = 0.5,
        legend=rownames(counts2), args.legend = list(x = "topright", ncol=3, inset=c(0, 0), cex=.6, title = "Satisfaction Rating"))

#variables to use: 

str(data)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    8.00   16.00   15.72   23.00   31.00 
summary(data$Day.of.Month)
monthBucket <- replicate(length(data$Day.of.Month), "Middle")
#if values are above 21, will return end of month
monthBucket[data$Day.of.Month > 21] <- "End"
#if values are below 10, will return beginning of month
monthBucket[data$Day.of.Month < 10] <- "Beginning"

table(monthBucket)
#most of the people travel during the middle of the month
#Beginning    End    Middle 
# 37566      39409     52914 

monthBucket <- sapply(monthBucket, as.factor)

summary(data$Satisfaction)
satisfactionBucket <- replicate(length(data$Satisfaction), "Average")
satisfactionBucket[data$Satisfaction > 4] <- "High"
satisfactionBucket[data$Satisfaction < 4] <- "Low"
table(satisfactionBucket)
#Average    High     Low 
# 53758   12554   63577 
#there were mostly low satisfaction rates

satisfactionBucket<- sapply(satisfactionBucket,as.factor)


table(satisfactionBucket, monthBucket)
#                   monthBucket
#satisfactionBucket    Middle   End    Beginning
#           Average     22204   16512     15042
#              High      5089    3804      3661
#               Low      25621   19093     18863

#52914 people travelled in middle of the month
#39409 people travelled at end of month
#37566 people travelled at beginning of month

prop.table(table(satisfactionBucket, monthBucket))
#                     monthBucket
#satisfactionBucket     Middle        End        Beginning
#           Average   0.17094596   0.12712393    0.11580657
#              High    0.03917961  0.02928654    0.02818560
#               Low    0.19725304  0.14699474    0.14522400

#do not use below
#ggplot(data= data, aes(x=monthBucket, y=satisfactionBucket)) + geom_bar(stat='identity')

#stacked bar graph:
counts3 <- table(satisfactionBucket, monthBucket)
barplot(counts3, main="Satisfaction depending Day of Month",
        col=c("darkblue","red", "yellow"),
        legend=rownames(counts3), args.legend = list(x = "topright", ncol=2, inset=c(0, 0), cex=.4))


install.packages("arules")
install.packages("arulesViz")

library(arules)
library(arulesViz)

#DO NOT DISCRETIZE THESE
newdata$Type.of.Travel <- discretize(newdata$Type.of.Travel, methods = NULL, default = NULL)
newdata$Class <-discretize(newdata$Class, methods = NULL, default = NULL)
newdata$Flight.date <- discretize(newdata$Flight.date, methods = NULL, default = NULL)
newdata$Airline.Code <- discretize(newdata$Airline.Code, methods = NULL, default = NULL)
newdata$Airline.Name <-discretize(newdata$Airline.Name, methods = NULL, default = NULL)
newdata$Origin.State <- discretize(newdata$Origin.State, methods = NULL, default = NULL)
newdata$Orgin.City <- discretize(newdata$Orgin.City, methods = NULL, default = NULL)
newdata$Destination.City <- discretize(newdata$Destination.City, methods = NULL, default = NULL)
newdata$Destination.State <-discretize(newdata$Destination.State, methods = NULL, default = NULL)
newdata$Flight.cancelled <-discretize(newdata$Flight.cancelled, methods = NULL, default = NULL)
newdata$Arrival.Delay.greater.5.Mins <- discretize(newdata$Arrival.Delay.greater.5.Mins, methods = NULL, default = NULL)
newdata$Gender <- discretize(newdata$Gender, methods = NULL, default = NULL)


#have to make the continuous variables into categorical 
newdata$Satisfaction <- discretize(newdata$Satisfaction, methods = NULL, default = NULL)
newdata$Age <- discretize(newdata$Age, methods = NULL, default = NULL)
newdata$Price.Sensitivity <- discretize(newdata$Price.Sensitivity, breaks=2)
newdata$Year.of.First.Flight <-discretize(newdata$Year.of.First.Flight, methods = NULL, default = NULL)
newdata$No.of.Flights.p.a. <- discretize(newdata$No.of.Flights.p.a., methods = NULL, default = NULL)
newdata$X..of.Flight.with.other.Airlines <- discretize(newdata$X..of.Flight.with.other.Airlines, methods = NULL, default = NULL)
newdata$No..of.other.Loyalty.Cards <- discretize(newdata$No..of.other.Loyalty.Cards, breaks = 1)
newdata$Shopping.Amount.at.Airport <- discretize(newdata$Shopping.Amount.at.Airport, breaks = 1)
newdata$Eating.and.Drinking.at.Airport <- discretize(newdata$Eating.and.Drinking.at.Airport, methods = NULL, default = NULL)
newdata$Day.of.Month <- discretize(newdata$Day.of.Month, methods = NULL, default = NULL)
newdata$Scheduled.Departure.Hour <- discretize(newdata$Scheduled.Departure.Hour, methods = NULL, default = NULL)
newdata$Flight.time.in.minutes <- discretize(newdata$Flight.time.in.minutes, methods = NULL, default = NULL)
newdata$Flight.Distance <- discretize(newdata$Flight.Distance, methods = NULL, default = NULL)
newdata$effectiveDelay <- discretize(newdata$effectiveDelay, breaks = 2)


newdata <- data[,1:13]
newdata <- newdata[,-2]
newdata <- newdata[,-4:-11]

#only focus on age, gender, and class (good predictors)
newdata$Satisfaction <- discretize(newdata$Satisfaction, methods = NULL, default = NULL)
newdata$Age <- discretize(newdata$Age, methods = NULL, default = NULL)

flightSurvey <- as(newdata, "transactions")

inspect(flightSurvey)

itemFrequency(flightSurvey)
freqplot <- itemFrequencyPlot(flightSurvey, support = 0.1, cex.names=0.5)

summary(flightSurvey)


apriori(flightSurvey, parameter = list(support = 0.005, confidence = 0.5))



#find what factors influence higher satisfaction rates
#shows men at the age between 37-53 had higher satisfaction (it was counted 14068 times)
ruleset <- apriori(data=flightSurvey, parameter = list(support = .01, confidence = 0.5), appearance = list(default="lhs", rhs="Satisfaction=[4,5]"))
rules_conf <- sort(ruleset, by="confidence", decreasing = TRUE)
inspect(head(rules_conf))

#THE HIGHER THE LIFT THE MORE INTERESTING

rules25 <- plot(ruleset)
inspect(head(ruleset))

#WANT HIGH SUPPORT AND HIGH CONFIDENCE

goodrules <- ruleset[quality(ruleset)$lift > 1.3]

plot(goodrules)
rules_lift <- sort(goodrules, by="lift", decreasing = TRUE)
inspect(head(rules_lift))
inspect(goodrules)



#low satisfaction
rulesetlow <- apriori(flightSurvey, parameter = list(support = 0.01, confidence = 0.1), appearance = list(default="lhs", rhs="Satisfaction=[1,3)"))
lowrules_conf <- sort(rulesetlow, by="confidence", decreasing = TRUE)
inspect(head(lowrules_conf))

#credit: http://r-statistics.co/Association-Mining-With-R.html
#credit: https://stackoverflow.com/questions/45724468/in-arules-return-the-smallest-support-items-from-a-lot-of-rules

#clean column names
cnames <- colnames(data)
cnames[2] <- "Airline Status"
cnames[5] <- "Price Sensitivity"
cnames[6] <- "Year of First Flight"
cnames[7] <- "Number of Flights"
cnames[8] <- "Percent of Flight with other Airlines"
cnames[9] <- "Type of Travel"
cnames[10] <- "No. of other Loyalty Cards"
cnames[11] <- "Shopping Amount at Airport"
cnames[12] <- "Eating and Drinking at Airport"
cnames[14] <- "Day of Month"
cnames[15] <- "Flight of Date"
cnames[16] <- "Airline Code"
cnames[17] <- "Airline Name"
cnames[18] <- "Origin City"
cnames[19] <- "Origin State"
cnames[20] <- "Destination City"
cnames[21] <- "Destination State"
cnames[22] <- "Scheduled Departure Hour"
cnames[23] <- "Departure Delay in Minutes"
cnames[24] <- "Arrival Delay in Minutes"
cnames[25] <- "Flight Cancelled"
cnames[26] <- "Flight Time in Minutes"
cnames[27] <- "Flight Distance"
cnames[28] <- "Arrival Delay greater 5 Minutes"




usa <- map_data("state")

data$Origin.State <- tolower(data$Origin.State)

summary(data$Origin.State)
data$Origin.State <- as.factor(data$Origin.State)

mapsimple <- ggplot(data, aes(map_id = state))
mapsimple <- mapsimple + geom_map(map = usa, fill="white", color="black")
mapsimple <- mapsimple + expand_limits(x = usa$long, y=usa$lat)
mapsimple
