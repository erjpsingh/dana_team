<<<<<<< HEAD
######################################################################################################
####                                     CLEANING DATA                                             ###


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

#forming a new column Time_Of_Day based on hour column, here we are dividing 24 hours to 4 parts of day
data_3$Time_Of_Day[data_3$HOUR >=0 & data_3$HOUR <=6] <-1
data_3$Time_Of_Day[data_3$HOUR >6 & data_3$HOUR <=12] <-2
data_3$Time_Of_Day[data_3$HOUR >12 & data_3$HOUR <=18] <-3
data_3$Time_Of_Day[data_3$HOUR >18 & data_3$HOUR <=24] <-4

#subset data by removing 2003 and 2019 partial data 
data_4 <- subset(data_3, YEAR >2003 & YEAR <2019)


#write data to csv
write.csv(data_4,"/home/jasp/DANA_4800/vancouver-crime-report/dana_team/cleaned_data.csv")

summary(data_4)


####################################################################################################
###                             SAMPLING TECHNIQUES                                              ###

#link to learn about sample method     http://www.programmingr.com/examples/neat-tricks/sample-r-function/

#first simple random sample with 10 % of data
SRS_1_index <-sample (1:nrow(data_4), size=48232)
SRS_1<-data_4[SRS_1_index,]



######################################################################################################
###                  EXPERIMENTING TO DRAW CIRCULAR BARCHART                                       ###
#referrences https://www.r-graph-gallery.com/297-circular-barplot-with-groups.html

install.packages("tidyverse")
library(tidyverse)

time <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
crime_rate <-c(2751, 1333, 1004,  916,  793,  762,  772, 1091, 1591, 1667, 1568, 1636,
               2447, 1916, 2082, 2435, 2739, 3201, 3739, 3069, 2694, 2785,2909,2332)
data_frame_data <- data.frame(time, crime_rate)


#Adding labels to the plot

number_of_bar <- nrow(data_frame_data)
angle <-  90 - 360 * (data_frame_data$time-0.5) /number_of_bar 
data_frame_data$hjust<-ifelse( angle < -90, 1, 0)
data_frame_data$angle<-ifelse(angle < -90, angle+180, angle)




# this is a function
initial_plot <- ggplot(data_frame_data, aes(x =data_frame_data$time, y=data_frame_data$crime_rate)) +
                geom_bar(stat="identity", fill=alpha("navyblue", 0.3))  +
                ylim(-2000,4000) +
                theme_minimal() +
                theme(
                        axis.text = element_blank(),
                        axis.title = element_blank(),
                        panel.grid = element_blank(),
                        plot.margin = unit(rep(-2,4), "cm")     # This remove unnecessary margin around plot
                ) +
                
                # This makes the coordinate polar instead of cartesian.
                coord_polar(start = 0)+
                # This add labels to data 
        geom_text(data=data_frame_data, aes(x=time, y=10, label=time, hjust=hjust), color="white", fontface="bold",alpha=0.6, size=4.5, angle= data_frame_data$angle, inherit.aes = FALSE )




initial_plot  # with this we have completed our "Crime Clock"

######################################################################################################
###                             VISUALIZATION WITH SRS_1  (Simple Random Sample)                   ###


###UNIVARIATE 

# How have the quantity of reported crimes changed over the years?
univariate_year_table_SRS_1 <- table(SRS_1$YEAR)
barplot(univariate_year_table_SRS_1,
        main = "Rate of Crime over the years",
        xlab='Years', 
        ylab = 'Number of Crimes',
        ylim = c(0,5000),
        xaxp = c(300,700,5),
        border = "white",
        col = "tomato3")


# Which periods of each day tend to have the most total reported crimes?

time_of_day_table_SRS_1 <- table(SRS_1$Time_Of_Day)
barplot(time_of_day_table_SRS_1,
        main = "Crimes as per period fo the day",
        xlab='Time of day', 
        ylab = 'Number of Crimes',
        ylim = c(0,12000),
        xaxp = c(300,700,5),
        border = "white",
        col = "tomato3")


#Which types of crimes have been the most commonly reported?


#elemenating Homicide and Offence Against a Person
var_type <- c('Break and Enter','Mischief','Other Theft','Theft from Vehicle'
              ,'Theft of Vehicle','Vehicle Collision')
var_number <-c(9068, 7172, 5746, 17896, 6103,2247)

table_for_type <-as.table(as.matrix(var_type,var_number))

data_frame_TYPE <- data.frame(var_type, var_number)

barplot(data_frame_TYPE$var_number,
        main = "Crimes as per period fo the day",
        
        xlab='Type of Crime ', 
        ylab = 'Number of Crimes',
        ylim = c(0,21000),
        xaxp = c(300,700,5),
        border = "white",
        col = "tomato3",
        las=2)

axis(1, at=1:6, labels=var_type)



# How many crimes have been reported in total over the population’s period for each neighbourhood?

neighbourhood_table_SRS_1 <- table((SRS_1$NEIGHBOURHOOD))
barplot(neighbourhood_table_SRS_1,
        main = "Crimes as per period fo the day",
        xlab='Time of day', 
        ylab = 'Type of Crimes',
        ylim = c(0,21000),
        xaxp = c(300,700,5),
        border = "white",
        col = "tomato3"        )  




########################################################################################
###                   BI VARIATE ANALYSIS WITH Simple Random Sample                  ###

# importing another library
library(ggplot2)

# How does the time of day affect the type of crime reported?

time_vs_type_data <- table(SRS_1$Time_Of_Day, SRS_1$TYPE)
time_vs_type_graph <- ggplot(time_vs_type_data)  + geom_bar()




# How does the neighbourhood affect the time (of day) the crime was reported?




# How does the neighbourhood affect the type of crime reported?
                       
                                   


#########################################################################################
###                    VISUALIZATION  OF POPULATION                                   ###

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

=======
######################################################################################################
####                                     CLEANING DATA                                             ###


#Setting the environment. 

#getwd()
#setwd('/home/jasp/DANA_4800/vancouver-crime-report/dana_team')


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

#forming a new column Time_Of_Day based on hour column, here we are dividing 24 hours to 4 parts of day
data_3$Time_Of_Day[data_3$HOUR >=0 & data_3$HOUR <=6] <-1
data_3$Time_Of_Day[data_3$HOUR >6 & data_3$HOUR <=12] <-2
data_3$Time_Of_Day[data_3$HOUR >12 & data_3$HOUR <=18] <-3
data_3$Time_Of_Day[data_3$HOUR >18 & data_3$HOUR <=24] <-4

# Categorizing neighborhoods into 5 main ones
data_3$NEIGHBOURHOOD <- gsub('Stanley Park', 'Downtown', data_3$NEIGHBOURHOOD)
data_3$NEIGHBOURHOOD <- gsub('West End', 'Downtown', data_3$NEIGHBOURHOOD)
data_3$NEIGHBOURHOOD <- gsub('Central Business District', 'Downtown', data_3$NEIGHBOURHOOD)

data_3$NEIGHBOURHOOD <- gsub('Arbutus Ridge', 'West Van', data_3$NEIGHBOURHOOD)
data_3$NEIGHBOURHOOD <- gsub('Dunbar-Southlands', 'West Van', data_3$NEIGHBOURHOOD)
data_3$NEIGHBOURHOOD <- gsub('Kerrisdale', 'West Van', data_3$NEIGHBOURHOOD)
data_3$NEIGHBOURHOOD <- gsub('Kitsilano', 'West Van', data_3$NEIGHBOURHOOD)
data_3$NEIGHBOURHOOD <- gsub('Musqueam', 'West Van', data_3$NEIGHBOURHOOD)
data_3$NEIGHBOURHOOD <- gsub('Shaughnessy', 'West Van', data_3$NEIGHBOURHOOD)
data_3$NEIGHBOURHOOD <- gsub('West Point Grey', 'West Van', data_3$NEIGHBOURHOOD)

data_3$NEIGHBOURHOOD <- gsub('Fairview', 'Central Van', data_3$NEIGHBOURHOOD)
data_3$NEIGHBOURHOOD <- gsub('Marpole', 'Central Van', data_3$NEIGHBOURHOOD)
data_3$NEIGHBOURHOOD <- gsub('Mount Pleasant', 'Central Van', data_3$NEIGHBOURHOOD)
data_3$NEIGHBOURHOOD <- gsub('Oakridge', 'Central Van', data_3$NEIGHBOURHOOD)
data_3$NEIGHBOURHOOD <- gsub('Riley Park', 'Central Van', data_3$NEIGHBOURHOOD)
data_3$NEIGHBOURHOOD <- gsub('South Cambie', 'Central Van', data_3$NEIGHBOURHOOD)

data_3$NEIGHBOURHOOD <- gsub('Grandview-Woodland', 'Northeast Van', data_3$NEIGHBOURHOOD)
data_3$NEIGHBOURHOOD <- gsub('Hastings-Sunrise', 'Northeast Van', data_3$NEIGHBOURHOOD)
data_3$NEIGHBOURHOOD <- gsub('Strathcona', 'Northeast Van', data_3$NEIGHBOURHOOD)

data_3$NEIGHBOURHOOD <- gsub('Kensington-Cedar Cottage', 'Southeast Van', data_3$NEIGHBOURHOOD)
data_3$NEIGHBOURHOOD <- gsub('Killarney', 'Southeast Van', data_3$NEIGHBOURHOOD)
data_3$NEIGHBOURHOOD <- gsub('Renfrew-Collingwood', 'Southeast Van', data_3$NEIGHBOURHOOD)
data_3$NEIGHBOURHOOD <- gsub('Victoria-Fraserview', 'Southeast Van', data_3$NEIGHBOURHOOD)
data_3$NEIGHBOURHOOD <- gsub('Sunset', 'Southeast Van', data_3$NEIGHBOURHOOD)

# Remove blank neighbors (not NA)
data_3 <- data_3[!(data_3$NEIGHBOURHOOD==""), ]

#subset data by removing 2003 and 2019 partial data 
data_4 <- subset(data_3, YEAR >2003 & YEAR <2019)

# Make all columns "Factor" (categorical)
data_4$Time_Of_Day <- factor(data_4$Time_Of_Day)
data_4$NEIGHBOURHOOD <- factor(data_4$NEIGHBOURHOOD)
data_4$YEAR <- factor(data_4$YEAR)
data_4$TYPE <- factor(data_4$TYPE)
data_4$HOUR <- factor(data_4$HOUR)

#write data to csv
#write.csv(data_4,"/home/jasp/DANA_4800/vancouver-crime-report/dana_team/cleaned_data.csv")
write.csv(data_4,"cleaned_data.csv")

summary(data_4)


####################################################################################################
###                             SAMPLING TECHNIQUES                                              ###

#link to learn about sample method     http://www.programmingr.com/examples/neat-tricks/sample-r-function/

#first simple random sample with 10 % of data
SRS_1_index <-sample (1:nrow(data_4), size=48232)
SRS_1<-data_4[SRS_1_index,]



######################################################################################################
###                  EXPERIMENTING TO DRAW CIRCULAR BARCHART                                       ###
#referrences https://www.r-graph-gallery.com/297-circular-barplot-with-groups.html

install.packages("tidyverse")
library(tidyverse)

time <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
crime_rate <-c(2751, 1333, 1004,  916,  793,  762,  772, 1091, 1591, 1667, 1568, 1636,
               2447, 1916, 2082, 2435, 2739, 3201, 3739, 3069, 2694, 2785,2909,2332)
data_frame_data <- data.frame(time, crime_rate)


#Adding labels to the plot

number_of_bar <- nrow(data_frame_data)
angle <-  90 - 360 * (data_frame_data$time-0.5) /number_of_bar 
data_frame_data$hjust<-ifelse( angle < -90, 1, 0)
data_frame_data$angle<-ifelse(angle < -90, angle+180, angle)




# this is a function
initial_plot <- ggplot(data_frame_data, aes(x =data_frame_data$time, y=data_frame_data$crime_rate)) +
                geom_bar(stat="identity", fill=alpha("navyblue", 0.3))  +
                ylim(-2000,4000) +
                theme_minimal() +
                theme(
                        axis.text = element_blank(),
                        axis.title = element_blank(),
                        panel.grid = element_blank(),
                        plot.margin = unit(rep(-2,4), "cm")     # This remove unnecessary margin around plot
                ) +
                
                # This makes the coordinate polar instead of cartesian.
                coord_polar(start = 0)+
                # This add labels to data 
        geom_text(data=data_frame_data, aes(x=time, y=10, label=time, hjust=hjust), color="white", fontface="bold",alpha=0.6, size=4.5, angle= data_frame_data$angle, inherit.aes = FALSE )




initial_plot  # with this we have completed our "Crime Clock"

######################################################################################################
###                             VISUALIZATION WITH SRS_1  (Simple Random Sample)                   ###


###UNIVARIATE 

# How have the quantity of reported crimes changed over the years?
univariate_year_table_SRS_1 <- table(SRS_1$YEAR)
barplot(univariate_year_table_SRS_1,
        main = "Rate of Crime over the years",
        xlab='Years', 
        ylab = 'Number of Crimes',
        ylim = c(0,5000),
        xaxp = c(300,700,5),
        border = "white",
        col = "tomato3")


# Which periods of each day tend to have the most total reported crimes?

time_of_day_table_SRS_1 <- table(SRS_1$Time_Of_Day)
barplot(time_of_day_table_SRS_1,
        main = "Crimes as per period fo the day",
        xlab='Time of day', 
        ylab = 'Number of Crimes',
        ylim = c(0,12000),
        xaxp = c(300,700,5),
        border = "white",
        col = "tomato3")


#Which types of crimes have been the most commonly reported?


#elemenating Homicide and Offence Against a Person


category_table_SRS_1 <- table(SRS_1$TYPE)
barplot(category_table_SRS_1,
        main = "Crimes as per period fo the day",
        xlab='Time of day', 
        ylab = 'Type of Crimes',
        ylim = c(0,21000),
        xaxp = c(300,700,5),
        border = "white",
        col = "tomato3")




# How many crimes have been reported in total over the population’s period for each neighbourhood?

neighbourhood_table_SRS_1 <- table((SRS_1$NEIGHBOURHOOD))
barplot(neighbourhood_table_SRS_1,
        main = "Crimes as per period fo the day",
        xlab='Time of day', 
        ylab = 'Type of Crimes',
        ylim = c(0,21000),
        xaxp = c(300,700,5),
        border = "white",
        col = "tomato3"        )  




########################################################################################
###                   BI VARIATE ANALYSIS WITH Simple Random Sample                  ###

# importing another library
library(ggplot2)

# How does the time of day affect the type of crime reported?

#time_vs_type_data <- table(SRS_1$Time_Of_Day, SRS_1$TYPE)
#time_vs_type_graph <- ggplot(time_vs_type_data)  + geom_bar()

ggplot(SRS_1, 
       aes(x = factor(SRS_1$Time_Of_Day,labels = c("12am - 6am","6am - 12pm","12pm - 6pm", "6pm - 12am")),
           fill = factor(SRS_1$TYPE
           ))) + geom_bar(position = "dodge") + labs(y = "Frequency", 
                                            fill = "Crime Types",
                                            x = "Time Periods",
                                            title = "Time of Day vs Crime Type") +
        theme_minimal()

chisq.test(SRS_1$Time_Of_Day, SRS_1$TYPE)

# How does the neighbourhood affect the time (of day) the crime was reported?

ggplot(SRS_1, 
       aes(x = SRS_1$NEIGHBOURHOOD, 
           fill = factor(SRS_1$Time_Of_Day, 
                         labels = c("12am - 6am","6am - 12pm","12pm - 6pm", "6pm - 12am")
                                ))) + 
        geom_bar(position = "dodge") + labs(y = "Frequency", 
                                            fill = "Time Periods",
                                            x = "City Neighborhoods",
                                            title = "Neighborhood vs Crime Report Time") +
        theme_minimal()

chisq.test(SRS_1$NEIGHBOURHOOD, SRS_1$Time_Of_Day)

# How does the neighbourhood affect the type of crime reported?
                       
ggplot(SRS_1, 
       aes(x = SRS_1$NEIGHBOURHOOD, 
           fill = factor(SRS_1$TYPE 
                         
           ))) + 
        geom_bar(position = "dodge") + labs(y = "Frequency", 
                                            fill = "Crime Types",
                                            x = "City Neighborhoods",
                                            title = "Neighborhood vs Crime Type") +
        theme_minimal()

chisq.test(SRS_1$NEIGHBOURHOOD, SRS_1$TYPE)

#########################################################################################
###                    VISUALIZATION  OF POPULATION                                   ###

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

>>>>>>> 46a409a143be291a22c84be9e16a9b2bc29962a8
