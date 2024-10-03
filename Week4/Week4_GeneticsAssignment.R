# Look at the plot and model results for our Dryad data in the tutorial. 
  # Part 1: Without knowing which points represent which groups,give one explanation for why these data might be difficult
  # to draw spatial inferences about genes.(4 points)
#they can be difficult to interpret because there are no definitive axis labels, or a legend to understand what each clumps of differing colors represent. This can be better defined in the methods of the experiment for readers and interpreters to better understand
  # Part 2: Despite the drawbacks, give the result or interpretation that you feel most confident in (4 points), and EXPLAIN WHY (6 points).
#I believe it is a graph of how the presence of Single Nucleotide Polymorphism (SNP's) can effect the adaptation of maritime pine, of which the colors of the dots are representative of these trees at different sites of the same region.
#I have no idea if any of this is actually correct, It is a very difficult data set to interpret, i just made inferences based of the abstract of the link provided and the exel data indication the different  populations used in the study. All in all 3/10 bad graph :(
#I agree bad graph - but what can you learn from it, or guess at from it? Maybe there's some geographic feature influencing how/where they could sample regions, i.e. mountains or oceans.

# For your scripting assignment we will use the "ge_data" data frame found in the "stability" package.
  # Install the "stability" package, load it into your R environment, and use the data() function to load the "ge_data". (2 points)

install.packages("stability")
library("stability")

data("ge_data")
df

# Create two linear models for Yield Response: one related to the Environment and one to the Genotype. (2 points each)
  # 'Yield Response' in this dataset is a measure of phenotype expression.
  # Hint: Look at the help file for this dataset.

#model for environment
environment<- lm(Yield~Env, data=ge_data)

#model for genotype
genotype<- lm(Yield~Gen, data=ge_data)

# Test the significance of both models and look at the model summary. (4 points each)
summary(environment)
summary(genotype)

  # Which model is a better fit to explain the yield response, and WHY? (6 points)
#The yield response would be better explained by the genotype dataset because it has a much larger sample size than the  genotype data frame, and is thus more representative, even though it has a slighlty larger p value, both are well under 0.05 and can thus be considered significant
  # Hint: Does one model seem more likely to be over-fitted?
#Backward! too many non-significant factors in the genotype...environment was a good fit.
# Which environment would be your very WORST choice for generating a strong yield response? (2 points)
#within the environment data set, there are two environments that are not marked as being significant data. of those are the intercepts: EnvSargodha, and EnvKhanewal. This is because they do not have the little star next to them in the data fram and are the only ones to produce more positive estimates. 
#The little star doesn't mean significant...the p-value does.