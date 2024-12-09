#Final project
#get everything organized under the right folder
getwd()
setwd("C:/GitHub/joyse/Final Project")
#Apologize in advance for the temperature, and goats headers being so long and in your face, it helps me visualize
  #where different parts of the code begin as ai work on different sections

#TEMPERATURE---------------------------------------------------------------------------------------------------------------------------

#make a data frame out of my temperature and precipitation over the years
Temp<-read.csv("acrc_USW00025309_annual_temp_1732196294437.csv", header=FALSE)
abiotic.tibble<-read.csv("acrc_USW00025309_annual_temp_1732196294437.csv", header = FALSE)
temp<- as.data.frame(abiotic.tibble)

temps <- temp[-1:-4,]


#CHANGING THE COLUMN NAMES

# Use the first row as column names
colnames(temps) <- temps[1, ]

# Remove the first row (now used as column names)
temperature <- temps[-1, ]



#GOATS-----------------------------------------------------------------------------------------------------------------------------------------------------

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

#Now were getting the exact year the goats have dies because it is a lot easier to format that into a graph
  #and it will fit better in the year data when paired against the temperature data set

install.packages("stringr")
library(stringr)

# Add a new column with the year after the '/'
goats$Year <- sapply(str_split(goats$Year_Death, "/"), function(x) x[2])


#okay things got funky becuase multiple goats would die in the same year which messed up the merged data so were
  #gonna have to add all cumulative deaths per year and compare them to those years

# Summarize cumulative deaths by year
yearly_cumulative_deaths <- aggregate(cumulative_deaths ~ Year, data = goats, max)
#okay now were adding a new column to this new goat data set to create the life ratio pt. 2 
yearly_cumulative_deaths$life_ratio <- yearly_cumulative_deaths$cumulative_deaths / 258*100

#okay things got real funky, so i need to change some things manually. the Year 2005, had said it was 1013? and
  #and my cumulative deaths shouldnt have decreased so i reordered the numbers
yearly_cumulative_deaths[1,1] <- 2005
yearly_cumulative_deaths[16,2] <- 82
yearly_cumulative_deaths[17,2] <- 87
yearly_cumulative_deaths[18,2] <- 93

#okay now you looked at my code and everything seemed alright but there was an abnormally large jump
  #in deaths, and then the cumulative went down and back up again, so i just manually put in the rest
yearly_cumulative_deaths[7,2] <- 14
yearly_cumulative_deaths[8,2] <- 20
yearly_cumulative_deaths[9,2] <- 29
yearly_cumulative_deaths[10,2] <- 34
yearly_cumulative_deaths[11,2] <- 39
yearly_cumulative_deaths[12,2] <- 42
yearly_cumulative_deaths[13,2] <- 55
yearly_cumulative_deaths[14,2] <- 63
yearly_cumulative_deaths[15,2] <- 66
yearly_cumulative_deaths[16,2] <- 78
yearly_cumulative_deaths[17,2] <- 86
yearly_cumulative_deaths[18,2] <- 93
  
  
  
#now redo this
yearly_cumulative_deaths$life_ratio <- yearly_cumulative_deaths$cumulative_deaths / 258*100
print (yearly_cumulative_deaths)


#Merging the two data sources --------------------------------------------------------------------------------------------

# Select only the columns of interest
temp_selected <- temperature[, c("Year", "Annual Average Temperature (degF)")]
goat_selected <- yearly_cumulative_deaths[, c("Year", "life_ratio")]

# Merge the two datasets by "Year"
combined_data <- merge(temp_selected, goat_selected, by = "Year")

#FINALLY plot that sucker
#first goat deaths by year

plot(yearly_cumulative_deaths$Year, yearly_cumulative_deaths$cumulative_deaths,
     main = "Plot of Year vs Cumulative Deaths",
     xlab = "Year",
     ylab = "# of Deaths",
     col = "blue",  # Change color
     xlim = c(2005, 2022),  # Set y-axis limits
     ylim = c(0, 100))  # Set y-axis limits)  

#second temperature by year
plot(temperature$Year, temperature$`Annual Average Temperature (degF)`,
     main = "Plot of Year vs Average Temp",
     xlab = "Year",
     ylab = "Average Temp",
     col = "blue",  # Change color
     xlim = c(2005, 2022),  # Set y-axis limits
     ylim = c(40, 45))  # Set y-axis limits) 

#third goats by temperature 
plot(combined_data$`Annual Average Temperature (degF)`, combined_data$life_ratio,
     main = "Plot of Average Temperature vs Life Ratio of Mountain Goats",
     xlab = "Average Temp",
     ylab = "Life Ratio",
     col = "blue",  # Change color
     xlim = c(40, 45),  # Set y-axis limits
     ylim = c(0,40))  # Set y-axis limits)  

#try and add trend lines to this <---------------------------------------------------------





#Analysis time baby ------------------------------------------------------------------------------------------------------------------------
  #mind you this is all for goat life ratio versus temp
  #this may have takena  few extra steps but it was a learning process and all were necessary for me to get to this point
  #need to first convert the values of temp to be numeric because they werent before
str(combined_data)
class(combined_data)
combined_data[, "Annual Average Temperature (degF)"] <- as.numeric(combined_data[, "Annual Average Temperature (degF)"])
colnames(combined_data)
str(combined_data)

# Fit the linear model
model <- lm(life_ratio ~ `Annual Average Temperature (degF)`, data = combined_data)

# View the summary of the model
summary(model)
  #Residual standard error: 12.5 on 3 degrees of freedom
  #Multiple R-squared:  0.8134,	Adjusted R-squared:  -0.05739 
  #F-statistic: 0.9341 on 14 and 3 DF,  p-value: 0.6068
    #not statistically signifacnt
library(ggplot2)

# Scatterplot with linear regression line
ggplot(combined_data, aes(x = `Annual Average Temperature (degF)`, y = life_ratio)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Linear Regression Analysis") +
  xlab("Annual Average Temperature (degF)") +
  ylab("Life Ratio")
#red line from plot above is the trendline with the grey being the confidence interval



#AIC index
# Fit the linear regression model
AIC_model <- lm(life_ratio ~ `Annual Average Temperature (degF)`, data = combined_data)
AIC(AIC_model)

# Calculate AIC for the model
aic_value <- AIC(model)
print(aic_value)
  #142.5098

#GLM
# Fit a GLM

combined_data[, "Annual Average Temperature (degF)"] <- as.numeric(combined_data[, "Annual Average Temperature (degF)"])
colnames(combined_data)
str(combined_data)

glm_model <- glm(life_ratio ~ `Annual Average Temperature (degF)`, 
                 data = combined_data, 
                 family = gaussian(link = "identity"))

# Summary of the GLM model
summary(glm_model)





#Now doing a GLM on cumulative deaths versus year
  #need to contunue the as.numeric because R keeps trying to get sassy with me

yearly_cumulative_deaths[, "Year"] <- as.numeric(yearly_cumulative_deaths[, "Year"])
colnames(yearly_cumulative_deaths)
str(yearly_cumulative_deaths)

glm_goats <- glm(cumulative_deaths ~ Year, 
                 data = yearly_cumulative_deaths, 
                 family = gaussian(link = "identity"))
summary(glm_goats)


#Now doing a GLM on temps versus year
  #can used combined data for this because has avg temps and year
  #again continue with as.numeric

combined_data[, "Annual Average Temperature (degF)"] <- as.numeric(combined_data[, "Annual Average Temperature (degF)"])
colnames(combined_data)
str(combined_data)

combined_data[, "Year"] <- as.numeric(combined_data[, "Year"])
colnames(combined_data)
str(combined_data)


glm_temps <- glm(`Year` ~ `Annual Average Temperature (degF)`, 
                 data = combined_data, 
                 family = gaussian(link = "identity"))
summary(glm_temps)
