#Final project
#get everything organized under the right folder
getwd()
setwd("C:/GitHub/joyse/Final Project")


#make a data frame out of my temperature and precipitation over the years
Temp<-read.csv("acrc_USW00025309_annual_temp_1732196294437.csv", header=FALSE)
abiotic.tibble<-read.csv("acrc_USW00025309_annual_temp_1732196294437.csv", header = FALSE)
temp<- as.data.frame(abiotic.tibble)

temps <- temp[-1:-4,]

library(dplyr)
library(stringr)

# Create a new column called 'Temp_Ratio'

df$Temp_Ratio <- temps$`Annual Maximum Temperature (degF)` /temps$Year




#make a data frame out of individual goat mortality
read.csv("mtn_goat_invdividual_mortality_db_final_2023_0625.csv")

goats.tibble<-read.csv("mtn_goat_invdividual_mortality_db_final_2023_0625.csv")
goats <- as.data.frame(goats.tibble)

#create a new column called total life, which is a count of all 

#create a new column called 'rising_death' which 























































