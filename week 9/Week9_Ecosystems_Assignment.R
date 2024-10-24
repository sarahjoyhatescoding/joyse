# For this week it's time to start exploring your own ideas and questions in R.
  # There are at least five options in the dataset to create the following comparisons.

# (Q1 - 12 pts) Use the dataset from the tutorial to complete one redundancy analysis (RDA) with variance partitioning on a different community (NOT the nematodes).
    # Explain the ecological importance of your significant predictor variables, or the importance if none are significant for your community.

library(readxl)

setwd("C:/GitHub/joyse/week 9")

    # First, read in the abiotic data:
    # Make sure the excel file is NOT open on your computer or it will generate an error (unlike read.csv)
abiotic.tibble <- read_excel(_____)






#TOOOK THIS OUT PUT IN EXECL SHEET LATER








    #Unfortunately, the read_excel function transforms our data into a "tibble" format
    # Tibbles are almost as evil as pie charts. 
    # Or, like cats, tibbles might look pretty but some day they will bite you when you least expect it.
    # This can easily be fixed with our old friend as.data.frame:
abiotic <- as.data.frame(abiotic.tibble)

invert.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Invertebrate_community")
invert <- as.data.frame(invert.tibble)
head(invert)

abiotic$names <- paste(abiotic$Parcel, abiotic$Land_Use)

head(abiotic)
#Same for the nematodes, but with different column names:
colnames(df)<-df[3,]
invert$names <- paste(invert$Parcel, invert$Landuse)

abiotic.means <- aggregate(x = abiotic, by = list(abiotic$names), FUN = "mean")
head(abiotic.means)

invert.means <- aggregate(x = invert, by = list(invert$names), FUN = "mean")
head(invert.means)


#make separate frames for each category


    # For our multivariate analysis we need to remove the NA and plot columns:
abiotic.means1 <- abiotic.means[,-16] # NA column
abiotic.means2 <- abiotic.means1[,-1:-6] # Plot and NA columns
abiotic.means2 <- sapply(abiotic.means2, as.numeric ) # Make sure everything is numeric.
abiotic.means2 <- as.data.frame(abiotic.means2) # Make sure it's in the right format.

invert.means1 <- invert.means[,-41] # Remove NAs
invert.means2 <- as.data.frame(invert.means1[,-1:-4]) # Remove plot and NAs
invert.means2 <- sapply(invert.means2, as.numeric )

    # And we can FINALLY compare the abiotic data against the biotic communities:
library(vegan)
colnames(abiotic.means2)
ord <- rda(nema.means2 ~ pH + totalN + Perc_ash + Kalium + Magnesium + Ca + Al + TotalP + OlsenP, abiotic.means2)
ord






# (Q2 - 12 pts) Then use the dataset from the tutorial to create a linear model related to your RDA. Try multiple predictors to find the best fit model.
  # Explain the ecological importance of the significant predictors, or lack of significant predictors.

# (Q3 - 6 pts) Provide a 3-4 sentence synthesis of how these results relate to one another and the value of considering both together for interpreting biotic-abiotic interactions.


