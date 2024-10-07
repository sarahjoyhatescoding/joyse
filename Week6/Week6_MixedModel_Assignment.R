# Read in the "Toscano_Griffen_Data.csv" data from GitHub and load the three packages we used in the tutorial this week.
# The paper these data came from is uploaded to Canvas as "Toscano&Griffen_2014_JAE..."

setwd("C:/Users/13216/OneDrive - Susquehanna University/Desktop/joyse/Week6")
df <- read.csv("Toscano_Griffen_Data.csv")

install.packages("MASS")
install.packages("MuMIn")
install.packages("mgcv")

library(MASS)
library(MuMIn)
library(mgcv)


# First create models with the same (y) and method (GLMM) as the published paper, using the GLMM function from this week's tutorial. 
  #Create two different models using the same 3 predictor (x) variables from the dataset. (4 points each) 
    # In one model only include additive effects.
    # In the other model include one interactive effect.
    # Use a binomial distribution and block as a random effect in both models to match the paper's analyses. Remember ?family to find distribution names.

# The authors used proportional consumption of prey as the (y) in their model, but did not include this in the dataset.
  # So we are going to create it - run the following line, assuming df= your data frame (feel free to change that):
df$prop.cons <- df$eaten/df$prey 

glmm.additive <- glmmPQL (prop.cons~temperature+carapace.width+claw.width, family = binomial, random =~ 1 | block, data = df)
summary(glmm.additive)

glmm.interactive <- glmmPQL (prop.cons~temperature+carapace.width*claw.width, family = binomial, random=~1 | block, data= df)
summary(glmm.interactive)

# (Q1) - The code I've provided in line 13 above is performing two operations at once. What are they? (2 pts)
# the functions added a new column to th data while simulataneously comparing some of the data points to others via intactive functions

# (Q2) - Did the interactive effect change which variables predict proportional consumption? How, SPECIFICALLY, did the results change? (5 pts)
#Yes they did, additive had a signficant difference as repsentative by their p value, whereas interactive did not.

# (Q3) - Plot the residuals of both models. Do you think either model is a good fit? Why or why not? (3 pts)
plot(glmm.additive)
plot(glmm.interactive)
 #both models do not look random as many points ppear in a straight line, not being random is bad so i dont think either are a good fit

# Re-run both models as generalized additive models instead (using gam). Then compare the AIC of both models. (4 points each)
gam.additive <- gam (prop.cons~temperature+carapace.width+claw.width, family = binomial, random =~ 1 | block, data = df)
summary(gam.additive)

gam.interactive <- gam (prop.cons~temperature+carapace.width*claw.width, family = binomial, random=~1 | block, data= df)
summary(gam.interactive)

AIC(gam.additive)
AIC(gam.interactive)

# (Q4) - Which model is a better fit? (2 pt)
#interactive is the best fit because it has a lower AIC number

# (Q5) - Based on the residuals of your generalized additive models, how confident are you in these results? (2 pts)
#not very, because they don't seem very random, and they don't have any signifcat values except for their intercept








