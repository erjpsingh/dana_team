#Setting the environment. 

getwd()
setwd('/home/jasp/DANA_4800/vancouver-crime-report/dana_team')


#read data  this is of type list ie combination of vectors in R 
data <- read.csv('crime_records.csv')

#this is of type list ie combination of vectors in R 
typeof(data)

#selected particular data
data_2 <- data[c('YEAR','HOUR','TYPE','NEIGHBOURHOOD')]

#summarise our data
summary(data_2)

#remove NA in hour column, since there are many(61033) NA in the hour of the day 
data_3 <- na.omit(data_2)

summary(data_3)

#forming a new column Time_Of_Day based on hour column, here we are dividing 24 hours to 8 parts of day
data_3$Time_Of_Day[data_3$HOUR >0 & data_3$HOUR <=3] <-1
data_3$Time_Of_Day[data_3$HOUR >3 & data_3$HOUR <=6] <-2
data_3$Time_Of_Day[data_3$HOUR >6 & data_3$HOUR <=9] <-3
data_3$Time_Of_Day[data_3$HOUR >9 & data_3$HOUR <=12] <-4
data_3$Time_Of_Day[data_3$HOUR >12 & data_3$HOUR <=15] <-5
data_3$Time_Of_Day[data_3$HOUR >15 & data_3$HOUR <=18] <-6
data_3$Time_Of_Day[data_3$HOUR >18 & data_3$HOUR <=21] <-7
data_3$Time_Of_Day[data_3$HOUR >21 & data_3$HOUR <=24] <-8

#subset data by removing 2003 and 2019 partial data 
data_4 <- subset(data_3, YEAR >2003 & YEAR <2019)


#write data to csv
write.csv(data_3,"/home/jasp/DANA_4800/vancouver-crime-report/dana_team/cleaned_data.csv")

summary(data_4)

#########################################################################################
###                    VISUALIZATION                                                  ###

###UNIVARIATE 

# How have the quantity of reported crimes changed over the years?
univariate_year_table <- table(data_3$YEAR)
barplot(univariate_year_table,
        main = "Rate of Crime over the years",
        xlab='Years', 
        ylab = 'Number of Crimes',
        ylim = c(0,50000),
        xaxp = c(300,700,5),
        border = "white",
        col = "tomato3")


# Which periods of each day tend to have the most total reported crimes?

time_of_day_table <- table(data_3$Time_Of_Day)
barplot(time_of_day_table,
        main = "Crimes as per period fo the day",
        xlab='Time of day', 
        ylab = 'Number of Crimes',
        ylim = c(0,120000),
        xaxp = c(300,700,5),
        border = "white",
        col = "tomato3")


#Which types of crimes have been the most commonly reported?


category_table <- table(data_3$TYPE)
barplot(category_table,
        main = "Crimes as per period fo the day",
        xlab='Time of day', 
        ylab = 'Type of Crimes',
        ylim = c(0,210000),
        xaxp = c(300,700,5),
        border = "white",
        col = "tomato3")




# How many crimes have been reported in total over the population’s period for each neighbourhood?

neighbourhood_table <- table((data_3$NEIGHBOURHOOD))
barplot(neighbourhood_table,
        main = "Crimes as per period fo the day",
        xlab='Time of day', 
        ylab = 'Type of Crimes',
        ylim = c(0,210000),
        xaxp = c(300,700,5),
        border = "white",
        col = "tomato3"        )  


### BI-VARIATE

# How does the time of day affect the type of crime reported?

time_vs_type_of_crime_table <- table(data_3$Time_Of_Day, data_3$TYPE)
barplot(time_of_day_table,
        col=c("darkblue","red"),
        beside = TRUE)

