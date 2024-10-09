# First, recreate Figure 4 from Herron et al. (2019). De novo origins of multicellularity in response to predation. Scientific reports.
  # Search datadryad.org by the paper title and download the dataset. It will include .csv files and R scripts, organized by figure.
  # Save the script and change the working directory on lines 8 and 115 to match your computer
  # Upload the plot you've created to GitHub. (4 points) #What's with the csv name?
  # Zoom into your plot to look at the distribution for different strains.

# Do all of the strains in the plot have the same distributions (yes/no)? (1 pt)
#no

# Based on these observations of your strain distributions, why did the authors use a Kruskal-Wallis test rather than ANOVA to compare the strains? (3 pts)
#they might decide to use a Kruskal-Wallis test to make the data easier to interpret and becuase ANOVA is typically used when data is normally distributed
#Because they *can't* use an ANOVA on these data.

# Use the fitdist() and gofstat() functions to compare the poisson, negative binomial, and logistic distributions for:

getwd()
data <- read.csv(file=("C:/Users/13216/OneDrive - Susquehanna University/Desktop/joyse/Week5/Figure4Data.csv"))#use your github repo.


install.packages("fitdistrplus")
install.packages("logspline")
library(logspline)
library(fitdistrplus)

View(data$Num.Cells.Progeny)
cells.progeny<-c(na.omit(data$Num.Cells.Progeny))
cfnb<- fitdist(cells.progeny, "nbinom")
cfpn<- fitdist(cells.progeny, "pois")
cfld<- fitdist(cells.progeny, "logis")

time.sec<-c(na.omit(data$RepTime.sec))
tfnb<- fitdist(time.sec,"nbinom")
tfpn<- fitdist(time.sec,"pois")
tfld<- fitdist(time.sec,"logis")

ofstat(list(cfnb, cfpn, cfld), chisqbreaks=c(1,2,4,8,16,32,64), fitnames=c("nbinom","pois","logis"))

gofstat(list(tfnb, tfpn, tfld), chisqbreaks=c(1,2,4,8,16,32,64), fitnames=c("nbinom","pois","logis"))

#No cheating - If I catch this again it's a zero.


  # (1) - The number of cells of progeny (data$Num.Cells.Progeny)
  # (2) - The replication time (data$RepTime.sec)
    #HINT- "Num.Cells.Progeny" has defined breaks. To display results, use the formula with the "chisqbreaks" argument as follows:
      #gofstat(list(fit.1, fit.2, fit.3, etc), chisqbreaks=c(1,2,4,8,16,32,64))


# Based on the AIC scores, which distribution is the best fit for: (5 pts each)
  # (1) - The number of cells of progeny (data$Num.Cells.Progeny)?
#the negative binomial because it has the lowest AIC score of 7315.569
  # (2) - The replication time (data$RepTime.sec)?
#the negative binomial because it has the lowest AIC score of 56169.02


# Plot a generic histogram for the replication time (data$RepTime.sec) (4 pt)
hist(data$RepTime.sec)

# Based on the patterns of this histograms and Figure 4:
  #Give one hypothesis for an evolutionary process represented by the two tallest bars in your histogram. (8 pts)
  # Don't cheat by looking at the paper! 
    # This hypothesis does not need to be correct - it only needs to be ecologically rational based these two figures.
#tere are spikes in organism frequency as their population increases to beyond a reasonable amount so that an outside predator must prey upon them and keep their population in check,thus significantly reducing their population until they eventually even out,seen as the spikes are slowly decreasing in height
#perhaps spikes in frequency are concurrent with the strains in figure 4 with the largest distribution






