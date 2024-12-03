#Final project
#get everything organized under the right folder
getwd()
setwd("C:/GitHub/joyse/Final Project")
#Apologize in advance for the temperature, and goats headers being so long and in your face, it helps me visualize
  #where different parts of the code begin as ai work on different sections

#TEMPERATUREEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE

#make a data frame out of my temperature and precipitation over the years
Temp<-read.csv("acrc_USW00025309_annual_temp_1732196294437.csv", header=FALSE)
abiotic.tibble<-read.csv("acrc_USW00025309_annual_temp_1732196294437.csv", header = FALSE)
temp<- as.data.frame(abiotic.tibble)

temps <- temp[-1:-4,]

library(dplyr)
library(stringr)

# Create a new column called 'Temp_Ratio'

df$Temp_Ratio <- temps$`Annual Maximum Temperature (degF)` /temps$Year


#GOATSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS

#make a data frame out of individual goat mortality
read.csv("mtn_goat_invdividual_mortality_db_final_2023_0625.csv")

goats.tibble<-read.csv("mtn_goat_invdividual_mortality_db_final_2023_0625.csv")
goats <- as.data.frame(goats.tibble)



# Add a cumulative count of goat deaths
  #all correspond to their dates, total deaths increase as time goes on
goats$new_column <- (goats$cumulative_deaths <- cumsum(goats$Avalanche_Mort))

#divide this by 258, which is the total number of goats in this study to create a ratio, then multiply by 100
  #so we have a percentage so itll be easier to understand
goats$life_ratio <- goats$cumulative_deaths / 258*100




#plot this number over time
  #theoretically number of deaths will increase over time
#set date to make sure it works in plot
goats$Date_Death <-as.date(goats$Date_Death, "%m/%d%Y")


plot(goats$Date_Death, goats$life_ratio,
     main = "Plot of Date_Death vs life_ratio",
     xlab = "Date",
     ylab = "life ratio",
     col = "blue",  # Change color
     xlim = as.Date(c("2006-02-10", "2022-01-04"))  # Set y-axis limits
     ylim = c(0, 100))  # Set y-axis limits)  

?POSIXt




















































