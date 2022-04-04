# Introduction to Data Science 11372
# Greene Vu 3209044
setwd("/Users/greenevu/IntroDataScience/Assignment2")

##################### Task 1 #####################

#Load and read the data from the CSV files and store them into dataframes named appropriately.
covid19_df <- read.csv("data/Covid19.csv")
tests_df <- read.csv("data/Tests.csv")
countries_df <- read.csv("data/countries.csv")
recovered_df <- read.csv("data/Recovered.csv")

#Tidy up “Recovered.csv” to be compatible with the dataframe driven from the file “Covid19.csv”,
recovered_df<-recovered_df %>%
  pivot_longer(X2020.01.22:X2020.05.05, names_to = "Date", values_to = "Recovered") %>%
  arrange(Country.Region, Date)

#Change the column names in the dataframes
colnames(covid19_df)<- c("Code", "Country", "Continent", "Date", "NewCases", "NewDeaths")
colnames(tests_df) <- c("Code", "Date", "NewTests")
colnames(countries_df) <- c("Code", "Country", "Population", "GDP", "GDPCapita")
colnames(recovered_df) <- c("Country", "Date", "Recovered")

#Ensure that all dates variables are of date data type and with the same format across the dataframes.
recovered_df <- transform(recovered_df,Date=as.Date(as.character(Date),"X%Y.%m.%d"))
covid19_df <- transform(covid19_df,Date=as.Date(as.character(Date),"%Y-%m-%d"))
tests_df <- transform(tests_df,Date=as.Date(as.character(Date),"%Y-%m-%d"))

#add new 5 variables “Recovered”, “NewTests”, “Population”, “GDP”, “GDPCapita” to it from other files (Recovered.csv, Tests.csv, countries_df.csv)
#Add "Recovered" from recovered_df
covid19_df <- merge(covid19_df, recovered_df, all.x = TRUE)
#add "NewTests" from tests_df
covid19_df <- merge(covid19_df, tests_df, all.x = TRUE)
#add “Population”, “GDP”, “GDPCapita” from "countries_df"
covid19_df <- merge(covid19_df, countries_df,  all.x = TRUE)

#Check for Nas in all dataframes and change them to Zero.
covid19_df[is.na(covid19_df)] <- 0

#Using existing “Date” variable; add month and week variables to the master dataframe. 
covid19_df <- covid19_df %>% 
       mutate(Month = lubridate::month(Date), 
              Week = lubridate::week(Date))


##################### Task 2 #####################

#Add four new variables to the master dataframe (“CumCases”, “CumDeaths”, “CumRecovered”, “CumTests”)
#detached plyr as it affects on "group_by" fuction
detach("package:plyr", unload = TRUE)
#"CumCases"
covid19_df <- covid19_df%>% arrange(Date, Country) %>%
                  group_by(Country) %>% 
                  mutate(CumCases = cumsum(NewCases),CumDeaths = cumsum(NewDeaths),
                         CumRecovered = cumsum(Recovered),CumTests = cumsum(NewTests))

#Add two new variables to the master dataframe (“Active”, “FatalityRate”)
#Active
covid19_df <- transform(covid19_df, Active = CumCases - (CumDeaths + CumRecovered))
#FatalityRate
covid19_df <- transform(covid19_df, FatalityRate = CumDeaths/CumCases)

#Add four new variables to the master dataframe (“Cases_1M_Pop”, “Deaths_1M_Pop”, “Recovered_1M_Pop”, “Tests_1M_Pop”)
#Cases_1M_Pop
covid19_df$CumTests
covid19_df <- transform(covid19_df, Cases_1M_Pop = CumCases*(10^6) / Population)
#Deaths_1M_Pop
covid19_df <- transform(covid19_df, Deaths_1M_Pop = CumDeaths*(10^6) / Population)
#Recovered_1M_Pop
covid19_df <- transform(covid19_df, Recovered_1M_Pop = CumRecovered*(10^6) / Population)
#Tests_1M_Pop
covid19_df <- transform(covid19_df, Tests_1M_Pop = CumTests*(10^6) / Population)

#Find the day with the highest reported death toll across the world. Print the date and the death toll of that day.
daily_death <- covid19_df %>% group_by(Date) %>%
  summarise(sumDeath = sum(NewDeaths))
print(summarise(daily_death, max_death_day = Date[which.max(sumDeath)],
                            max_death = max(sumDeath)))

#Build a graph to show how the cumulative data of (Infected Cases, Deaths, Recovered, Tests) change over the time for the whole world collectively.
daily_info <- covid19_df %>% group_by(Date) %>%
  summarise(Cases = sum(CumCases), Deaths = sum(CumDeaths), Recovereds = sum(CumRecovered), Tests = sum(CumTests))

myplot_df<- gather(daily_info, key = key, value = value, -Date)
  
ggplot(myplot_df, aes(x = Date, y = value, color = key) )+
  geom_line()+
  xlab("Year 2020") +
  ylab("Number of People")+
  scale_y_log10()
  
#Extract the last day (05/05/2020) data and save it in a separate dataframe called “lastDay_data”
lastDay_data <- filter(covid19_df, Date == "2020-05-05")

#extract the whole records of the top 10 countries worldwide that have current active cases, total confirmed cases, and fatality rate in separate dataframes (i.e. top10activeW, top10casesW, top10fatalityW, top10testsMW).
#top10activeW
top10activeW <- head(lastDay_data[order(lastDay_data$Active, decreasing = TRUE),] , n=10) 
#top10casesW
top10casesW <- head(lastDay_data[order(lastDay_data$CumCases, decreasing = TRUE),] , n=10) 
#top10fatalityW
top10fatalityW <- head(lastDay_data[order(lastDay_data$FatalityRate, decreasing = TRUE),] , n=10) 
#top10testsMW
top10testsMW <- head(lastDay_data[order(lastDay_data$CumTests, decreasing = TRUE),] , n=10) 

#print the up to date confirmed, death, recovered cases as well as the tests for every continent.
print(lastDay_data %>% group_by(Continent) %>%
        summarise(Confirmed_Death = max(CumDeaths),
                  Confirmed_Cases = max(CumCases),
                  Confirmed_Recovered = max(CumRecovered)))

#9.Build a graph to show the total number of cases over the time for the top 10 countries that have been obtained in question 7
top10_cases_byCountry <- filter(covid19_df, Country %in% c( "United States of America",
                                                 "Spain","Italy","United Kingdom","Germany","Russia",
                                                 "France","Turkey","Brazil","Iran"))
      
#gather informaton to plot
top10_cases_byCountry <- top10_cases_byCountry %>% arrange(Date, Country) %>% group_by(Country) %>%
                          select(Date,Country,CumCases)
#Plot
ggplot(top10_cases_byCountry, aes(x = Date, y = CumCases, color = Country) )+
  geom_line()+
  xlab("Month 2020") +
  ylab("Cumulative Cases")+
  scale_y_log10()

#10. Build a graph for the top 10 countries with current highest active cases 
top10_active_byCountry <- filter(covid19_df, Country %in% top10activeW$Country)

top10_active_byCountry <- top10_active_byCountry %>% arrange(Date, Country) %>% group_by(Country) %>%
  select(Date,Country,NewCases, NewDeaths, Recovered)
top10_active_byCountry$Recovered

#Choose Data for the graph
top10_active_byCountry <- gather(top10_active_byCountry, key = key, value = value, -Date, -Country)

ggplot(top10_active_byCountry, aes(x = Date, y = value, color = key) )+
  geom_line()+
  facet_grid(Country~.)+
  xlab("Month 2020") +
  ylab("Number of people")+
  scale_y_log10()

##################### Task 3 #####################

#Based on lastDay_data, create a separate dataframe named "cor_data" with the data of these variables (CumCases, CumTests, Population, GDP, GDPCapita).
cor_data <- lastDay_data %>% 
  select(CumCases, CumTests, Population, GDP, GDPCapita)

#Compute the correlation matrix between the variables of the “cor_data” and visualise this correlation matrix.
#correlation
M <-cor(cor_data)
#loading package
library(corrplot)
#Visualise
corrplot(M, type="lower")

#Divide the cor_data into training and testing, where training data represent 65% of the number of rows.
#loading package
library(caret)
#use caret function to split, SplitRatio for 65%:35% splitting
cor_data1<-createDataPartition(cor_data$CumCases,p=.65,list=FALSE)
#subsetting into Train data
train<-cor_data[index,]
#subsetting into Test data
test<-cor_data[-index,]   

#Train a linear regression model to predict cumulative cases from the GDP of the countries.
#building model
lr_model<-lm(CumCases~GDP,data=train)
summary(lr_model)
#evaluate this model on the test data and print the root mean square error value.
test$PreditedCumCases<-predict(lr_model,test)
head(test,c(“CumCases”,“PreditedCumCases”)









