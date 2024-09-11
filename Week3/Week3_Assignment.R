# (1) Approximately how many hours ahead of Sunbury was the peak flow in Lewisburg during the 2011 flood? (2 pt)
#Roughly 8 hours or so, Lewisburg hit is peak around 5pm on the 8th, whereas sunbury reached its peak around 1am the 9th 


# (2) Give one reason why information on the time between peak flow events up- and downstream could be valuable information? (4 pts)
#It can provide us with vital information about the maximum flow of water a channel or body of water can undergo in a flooding situation

# Package scavenger hunt! (12 pts each)

## (3) Using Google and ONLY packages from GitHub or CRAN:
    # Find a package that contains at least one function specifically designed to measure genetic drift.
          #driftR
    # Copy-paste into your script - and run - an example from the reference manual for a function within this package related to a measure of genetic drift. 
pkgs <- c("plyr","reshape","ggplot2","magrittr","viridis","shiny")
dl_pkgs <- subset(pkgs,!pkgs %in% rownames(installed.packages()))
if(length(dl_pkgs)!=0){
  for(i in dl_pkgs) install.packages(i)
}
library(shiny)
runGitHub(username="cjbattey",repo="driftR")
        # Depending on the function, either upload a plot of the result or use print() and copy/paste the console output into your script.
    # After running the function example, manipulate a parameter within the function to create a new result.
#HEY MATT, remember me, i did it shiny way
        # Common options might be allele frequency, population size, fitness level, etc. 
        # Add the results of this manipulation to your script (if in the console) or upload the new plot.
       
          # By manipulating these parameters you can see how it impacts the results.
          # This type of manipulation is one example of how theoretical ecology and modelling are used to predict patterns in nature.



## (4) Using Google and ONLY packages from GitHub or CRAN:
    # Find a package that will generate standard diversity metrics for community ecology, specifically Simpson's Diversity Index.
    # Copy-paste into your script - and run - an example from the reference manual for a function to calculate Simpson's diversity. 
        # Depending on the example usage of the function, either upload a plot of the result or use print() and copy/paste the console output into your script.
    # After running the function example, modify your script to generate another diversity metric that is NOT part of the example. 
        # If there are multiple diversity metrics in the example script, none of these will count as the modified script.
        # Hint: If the function can "only" calculate Simpson's diversity, the inverse of Simpson's diversity is another common metric. 
        # Add the results of this manipulation to your script (if in the console) or upload the new plot.
        
          # Diversity metrics are frequently used in community ecology for reasons ranging from a quick comparison between sites to understanding community stability.
          # Their calculation can be very tedious by hand - and very fast with a package designed for the operation.



