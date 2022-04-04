# Introduction to Data Science 11372
# Greene Vu 3209044
# Description of what this code id for.

setwd("/Users/greenevu/IntroDataScience/Assignment1")


##################### PART A #####################
#Load these files into working directory, one by one
help(package = "datasets")
install.packages("tidyverse")   
install.packages("plyr")                                          
install.packages("readr")                                        

library("dplyr")                                                 
library("plyr")  
library("readr")   
library("tidyverse")

data1 <- read.csv("data/201808.csv", skip = 7)
data2 <- read.csv("data/201809.csv", skip = 7)
data3 <- read.csv("data/201810.csv", skip = 7)
data4 <- read.csv("data/201811.csv", skip = 7)
data5 <- read.csv("data/201812.csv", skip = 7)
data6 <- read.csv("data/201901.csv", skip = 7)
data7 <- read.csv("data/201902.csv", skip = 7)
data8 <- read.csv("data/201903.csv", skip = 7)
data9 <- read.csv("data/201904.csv", skip = 7)
data10 <- read.csv("data/201905.csv", skip = 7)
data11 <- read.csv("data/201906.csv", skip = 7)
data12 <- read.csv("data/201907.csv", skip = 7)
data13 <- read.csv("data/201908.csv", skip = 7)
data14 <- read.csv("data/201909.csv", skip = 7)
data15 <- read.csv("data/201910.csv", skip = 7)
data16 <- read.csv("data/201911.csv", skip = 7)
data17 <- read.csv("data/201912.csv", skip = 7)
data18 <- read.csv("data/202001.csv", skip = 7)
data19 <- read.csv("data/202002.csv", skip = 7)

#Concatenate all the data of these file into one dataframe 
setwd("/Users/greenevu/IntroDataScience/Assignment1/data")

list.files()
mydf <- ldply(list.files(), read.csv, skip=7, header=TRUE)
View(mydf)       

#check problems while loading and parsing the data
problems(mydf)
stop_for_problems(mydf)

##################### PART B #####################
#1.Remove the variables, which have no data at all 
mydf <- mydf[ , colSums(is.na(mydf)) < nrow(mydf)]  

#2.Drop the variables, which have few data
mydf <- mydf[,!sapply(mydf, function(x) mean(is.na(x)))>0.9]

#3.Change the column names to have no spaces between words and replace 
#these places with underscore
#mydf2 <-head(mydf)
names(mydf) <- gsub("\\.", "_", names(mydf))
View(mydf)       

#4. Change the type of the column called "Date" from character to Date
#data type
mydf$Date <- as.Date(paste(mydf$Date), format= "%d/%m/%Y")
class(mydf$Date)

#5. Add two new columns for the month and year of he data in each file,
#you may extract the contents of this column from the Date column. Please
#note that the data are collected for the 19 months across 3 years
mydf <- mydf %>% separate(Date, c("Year","Month","Date"),"-")
View(mydf)

#6. Change the type of the "Month" and "Year" columns from character to
#Ordinal with levels as the number of months in a year (12) and no of year(3) 
mydf$Year <- as.numeric(mydf$Year)
levels(mydf$Year)=c(2018, 2019, 2020)

mydf$Month <- as.numeric(mydf$Month)
levels(mydf$Month)=c(1,2,3,4,5,6,7,8,9,10,11,12)

#7. For all the numeric columns, replace the remaining NAs with the median of
#the values in the column, if exist

mydf$X9am_cloud_amount__oktas_[is.na(mydf$X9am_cloud_amount__oktas_)] <- 
  median(mydf$X9am_cloud_amount__oktas_, na.rm = TRUE)

mydf$X3pm_cloud_amount__oktas_[is.na(mydf$X3pm_cloud_amount__oktas_)] <-
  median(mydf$X3pm_cloud_amount__oktas_, na.rm = TRUE)

mydf$Speed_of_maximum_wind_gust__km_h_[is.na(mydf$Speed_of_maximum_wind_gust__km_h_)] <-
  median(mydf$Speed_of_maximum_wind_gust__km_h_, na.rm = TRUE)

##################### PART C #####################


#1a. Print the summary of 'Minimum_temperature'
summary(mydf$Minimum_temperature)

#1b. Print the summary of '9am_temperature'
summary(mydf$X9am_Temperature)

#1c. Print the summary of 'Speed_of_maximum_wind_gust_(km/h)'
summary(mydf$Speed_of_maximum_wind_gust__km_h_)

#2.Extracting the average of minimum temperature per month and year
#monthly
aggregate( Minimum_temperature ~ Month + Year , mydf , mean)
#yearly
aggregate( Minimum_temperature ~ Year , mydf , mean)

#3. Extracting the average of speed of maximum wind gust by direction 
#of maximum wind gust
average_windgust <-
  aggregate(Speed_of_maximum_wind_gust__km_h_ ~ Direction_of_maximum_wind_gust , mydf , mean)
view(average_windgust)

#4. Which month was dry, if any? And in which year?
monthly_rainfall <- aggregate(Rainfall__mm_  ~ Month + Year , mydf , FUN = sum )
if (0.0 %in% monthly_rainfall$Rainfall__mm_){
   driest_month <- 
     summarise(monthly_rainfall, year= Year[which.min(monthly_rainfall$Rainfall__mm_)],
               driest_month= Month[which.min(Rainfall__mm_)],
               driest_value= min(Rainfall__mm_))
} else {
  print("There is no dry month")
}
  
#5. What about the humidity, which month in the ACT has the highest humidity level in 2019
#data of 2019 only
mydf_2019 <- filter(mydf, mydf$Year =="2019")

monthly_humidity2019 <- aggregate(X9am_relative_humidity____+X3pm_relative_humidity____
                              ~ Month + Year , mydf_2019 , FUN = sum )

highest_humidity_2019 <- summarise(monthly_humidity, 
                                     max_month= Month[which.max(`X9am_relative_humidity____ + X3pm_relative_humidity____`)], 
                                     max_value= max(`X9am_relative_humidity____ + X3pm_relative_humidity____`))

view(highest_humidity_2019)


#6. For 2019, extract the minimum, maximum and average temperature, wind speed and humidity per month and per quarter in 2019 only
#Change data type of "X3pm_wind_speed__km_h_" and "X9am_wind_speed__km_h_" to numeric as they were character
mydf_2019$X3pm_wind_speed__km_h_ <- as.numeric(mydf_2019$X3pm_wind_speed__km_h_)
mydf_2019$X9am_wind_speed__km_h_ <- as.numeric(mydf_2019$X9am_wind_speed__km_h_)


  monthly_min_temperature2019 <- aggregate(Minimum_temperature
                                       ~ Month + Year , mydf_2019 , min )
  monthly_max_temperature2019 <- aggregate(Maximum_temperature
                                       ~ Month + Year , mydf_2019 , max )
  monthly_mean_temperature2019 <- aggregate((Maximum_temperature+Minimum_temperature)/2
                                        ~ Month + Year , mydf_2019 , mean )
  
  daily_df_2019 <- transform(mydf_2019, min_hum =pmin(X9am_relative_humidity____, X3pm_relative_humidity____),
                             max_hum = pmax(X9am_relative_humidity____, X3pm_relative_humidity____),
                             min_wind_speed= pmin(X9am_wind_speed__km_h_, X3pm_wind_speed__km_h_),
                            max_wind_speed= pmax(X9am_wind_speed__km_h_, X3pm_wind_speed__km_h_))
  
  monthly_min_huminity2019 <- aggregate(min_hum, Month + Year ,daily_df_2019, min)
  





#7. Plot the histogram/bar-charts for each variable of the previous question
  











