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

#write data to csv
write.csv(data_3,"/home/jasp/DANA_4800/vancouver-crime-report/dana_team/cleaned_data.csv")

#crimes per year 
year_temp <- table(data_3[1])
barplot(year_temp)

# crimes per neighbourhood

neighbourhood_temp <- table((data_3[4]))
barplot(neighbourhood_temp)  

# crimes per category 

category_temp <- table(data_3[3])
barplot(category_temp)

# crimes per time of day

time_of_day_temp <- table(data_3[2])
barplot(time_of_day_temp)
