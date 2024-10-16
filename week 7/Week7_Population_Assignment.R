#@'s refer to slots, whereas $'s refer to columns

# Load the "anytime" and "ggplot2" packages to complete this week's assignment.
install.packages("anytime")
install.packages("ggplot2")

library("anytime")
library("ggplot2")

# Read the "Plankton_move_average" CSV in from GitHub. 
# These are data from the Great Lakes Environmental Research Laboratory plankton sampling.
setwd("C:/GitHub/joyse/week 7")
data <-read.csv("C:/GitHub/joyse/week 7/Plankton_move_average.csv")

#Used the following lines to format the date and remove NAs from the dataset:
data$Date <- as.Date(data$Date, origin = "0001-01-01") # Setting values to "day zero".
data <- na.omit(data)

#Plot these population data over time with the following code:
ggplot(data)  +
  xlab("Numeric Date") + ylab("Density Individuals")+
  geom_line(data=data, aes(Date, D.mendotae), color="black", alpha = 0.7, size=1)+
  geom_line(data=data, aes(Date, LimncalanusF+LimncalanusM), color="orange",  alpha = 0.7, size=1)+ # adding males and females together, hint: this is actually spelled Limnocalanus
  geom_line(data=data, aes(Date, Bythotrephes), color="sky blue",  alpha = 0.7, size=1)+
  geom_line(data=data, aes(Date, Bythotrephes), color="sky blue",  alpha = 0.7, size=1)+
  theme_bw() 

# Export this plot to have on hand for reference in the next section of the assignment (and upload with your script). (8 pts)
#done

# (1) - Which species is most likely to be r-selected prey and which its primary predator? (2 pts)
  #both sexes of the Limncalanus are the r selected prey as they cause the D.mendotae to react to their spikes in movement
# What is one relationship the third species MIGHT have to the first two? (2 pts)
  #the third species, Bythotrephes, are relatively stable in their movement but seem to have a reactive response to when the D.mendotae have a spike in their movement, perhaps they are scavengers
  #picking up the bits from when D.mendotae feeds on Limncalanus, which causes them to have a rise in movement as more of their food is available. i know very little about plankton 

#Now copy/paste in the Lotka-Volterra function, plotting script, and load the "deSolve" package from the tutorial:
library("deSolve")


LotVmod <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dx = x*(alpha - beta*y)
    dy = -y*(gamma - delta*x)
    return(list(c(dx, dy)))
  })
}

Pars <- c(alpha = 2, beta = 0.5, gamma = .2, delta = .6)

Pars <- c(alpha = 4, beta = 1, gamma = .4, delta = 1.2) 

Pars <- c(alpha = 2, beta = 0.5, gamma = .8, delta = 2.4) 

State <- c(x = 10, y = 10)#For now keep this the same.
Time <- seq(0, 100, by = 1)#For now keep this the same.
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))


matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Limncalanus", "D.mendotae"), lty = c(1,2), col = c(1,2), box.lwd = 0)


# (2) - What do alpha, beta, gamma, and delta represent in this function? (4 pts)
  #alpha represents growth rate of the prey population
  #beta represents the rate at which predators consume their prey
  #gamma represents the death rate of the predator population when there is a lack of prey
  #delta represents the rate at which the predator population increases as they consume more prey

# (3) - By only changing values for alpha, beta, gamma, and/or delta
# change the default parameters of the L-V model to best approximate the relationship between Limncalanus and D.mendotae, assuming both plots are on the same time scale.
# What are the changes you've made to alpha, beta, gamma, and delta from the default values; and what do they say in a relative sense about the plankton data? (4 pts)
  #I've altered the original rates of alpha, beta, gamma and delta by doubling the parameters set in the tutorial. I've noticed that this has caused
  #there to be more peaks and valleys present in the predator prey dynamic within the same time frame, meaning both sets of parameters balance out the same,
  #with a more variable environment causing these frequent checks and balances systems to play out
# Are there other parameter changes that could have created the same end result? (2 pts)
  #since i doubled the original data, i had reverted my par line of code back to that of the tutorial. since alpha is related to delta, i quadrupled the delta coefficient while keeping alpha 
  #at its original value, thus making them both average out to being relatively double of what the original line of code once was.
  #I did the same thing for beta and gamma, kept beta the same as the original code, quadrupled gamma and got relatively the smae kind of graph i was looking for.
# Export your final L-V plot with a legend that includes the appropriate genus and/or species name as if the model results were the real plankton data, 
# and upload with your script. (hint - remember which one is the predator and which is the prey) (8 pts)




