# For this week it's time to start exploring your own ideas and questions in R.
  # There are at least five options in the dataset to create the following comparisons.

# (Q1 - 12 pts) Use the dataset from the tutorial to complete one redundancy analysis (RDA) with variance partitioning on a different community (NOT the nematodes).
    # Explain the ecological importance of your significant predictor variables, or the importance if none are significant for your community.
#the predictor variables in my data setare abiotic soil factors that compare different plant species across different plots of land, either ancient or post agricultural, that show how the presence of multiple different species effect competition for sought after nutrients in the soil to see who thrives the best.
#for example, whuchever plant gets the most soil nutirents will most likely have the longest stem


library(readxl)
library(vegan)

setwd("C:/GitHub/joyse/week 9")

    # First, read in the abiotic data:
    # Make sure the excel file is NOT open on your computer or it will generate an error (unlike read.csv)
abiotic.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Abiotic factors")
abiotic <- as.data.frame(abiotic.tibble)

veg_trans.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet ="Vegetation_transects")
veg_trans <- as.data.frame(veg_trans.tibble)
head(veg_trans)

abiotic$names <- paste(abiotic$Parcel, abiotic$Land_Use)
head(abiotic)


veg_trans$names <- paste(veg_trans$Parcel, veg_trans$Landuse)
abiotic.means <- aggregate(x= abiotic, by = list(abiotic$names), FUN ="mean")
head(abiotic.means)

veg_trans.means <- aggregate(x=veg_trans, by = list(veg_trans$names), FUN =  "mean")

AGH <- merge(abiotic, veg_trans, by="names")

ord <- rda(AGH [,18:54] ~ pH + totalN + Perc_ash + Kalium +Magnesium + Ca + Al + TotalP +OlsenP, AGH)
ord

#Significance test?




# (Q2 - 12 pts) Then use the dataset from the tutorial to create a linear model related to your RDA. Try multiple predictors to find the best fit model.
  # Explain the ecological importance of the significant predictors, or lack of significant predictors.

#Kalium, and Ca, along with Total P, proved to be the most significant predictor in plant species sucess across differnt land types, meaning that these abiotic nutrients have the biggest impact on whichevr plant is able to utilize them most, making them thrive more than others.
#...Making them grow taller?
abiotic.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Abiotic factors")
abiotic <- as.data.frame(abiotic.tibble)

urtica.tibble <-read_excel("penaetal_2016_data.xlsx", sheet = "Data_experiment_urtica")
urtica <- as.data.frame(urtica.tibble)

urtica$names <- paste(urtica$Parcel , urtica$Land_use)
abiotic$names <- paste(abiotic$Parcel, abiotic$Land_Use)

abiotic.means <- aggregate(x=abiotic, by = list (abiotic$names), FUN = "mean")
urtica.means <- aggregate(x = urtica, by = list(urtica$names), FUN = "mean")
                          
abiotic.means$Parcel <- unique(abiotic$Parcel)

urtica.merged <-merge(abiotic.means, urtica, by = "Parcel")

urtica.merged1 <- urtica.merged [,-4:-3]
urtica.merged2 <- as.data.frame(urtica.merged1[,-4])

library(fitdistrplus)
library(logspline)

fit.weibull <- fitdist(urtica.merged$Length_main_stem, distr = "weibull")
fit.norm <- fitdist(urtica.merged$Length_main_stem, distr = "norm")
fit.gamma <- fitdist(urtica.merged$Length_main_stem, distr = "gamma")
fit.lnorm <- fitdist(urtica.merged$Length_main_stem, distr = "lnorm")
fit.nbinom <- fitdist(urtica.merged$Length_main_stem, distr = "nbinom")
fit.logis <- fitdist(urtica.merged$Length_main_stem, distr = "logis")
fit.geom <- fitdist(urtica.merged$Length_main_stem, distr = "geom")

gofstat(list(fit.weibull, fit.norm,fit.gamma, fit.lnorm, fit.logis))

colnames(urtica.merged2)

mod1 <- lm(Length_main_stem ~ Kalium + Ca + Al + TotalP, urtica.merged2)
summary(mod1)
anova(mod1)
AIC(mod1)

plot(mod1$residuals)


# (Q3 - 6 pts) Provide a 3-4 sentence synthesis of how these results relate to one another and the value of considering both together for interpreting biotic-abiotic interactions.

#Based off the interactions ive witnessed in running these models, I have found that the most significant abiotic factors in determining a plants successful is Kalium, and Ca. #and P
#they had the most significant correlation between plant success across the tested species of plants put of all abiotic factors included within this experiment
#based on the eigenvalues we see a decent amount of variability in the success of the plant species tested without the presence of abiotic factors.
#not gonna lie i am still very confused on question 1
  #We can walk through it together if you want.

