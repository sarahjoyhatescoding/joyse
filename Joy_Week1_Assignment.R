# Now it is time to create your own data frame using the tools we have learned this week.
# First, resave this script as: yourlastname_Week1_Assignment [1 point]
  # e.g. mine would be Wilson_Week1_Assignment


# Create 3 numeric vectors and 2 character vectors that are each 15 values in length with the following structures: [15 points; 3 each]
  # One character vector with all unique values
e<- c('d','e','f','g','h','i','j','k','l','m','n','o','p','q','r')
e
  # One character vector with exactly 3 unique values
d<- c('a','a','a','a','a','b','b','b','b','b','c','c','c','c','c')
d
  # One numeric vector with all unique values
a<- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
a 
 # One numeric vector with some repeated values (number of your choosing)
b<- c(1,1,1,2,2,6,8,8,9,11,13,13,14,14,15)
b 
 # One numeric vector with some decimal values (of your choosing)
c<- c(1.1,2.2,3.3,4.4,5.5,6.6,7.7,8.8,9.9,10.1,11.11,12.12,13.13,14.14,15.15)
c

# Bind the vectors into a single data frame, rename the columns, and make the character vector with unique values the row names.[3 points]

data<- cbind(e,d,a,b,c)
data

df<- as.data.frame(data)
df

colnames(df) <- c('unique','three','ordered','repeats','decimals')
df

row.names(df) <- df$unique
df

# Remove the character vector with unique values from the data frame.[2 points]

df.b <- df[,-1]
df.b

# Add 1 row with unique numeric values to the data frame.[2 points]

hate<- data.frame('I','hate','this','sm')
hate

colnames(hate) <- colnames(df.b)
df.r <- rbind(df.b,hate)
df.r


# Export the data frame as a .csv file [2 points]

write.csv(df.r,file = "joy_week1_assignment.csv")

# Generate summary statistics of your data frame and copy them as text into your script under a new section heading. [2 points]

summary(df.r)

####    three             ordered            repeats            decimals        
####Length:16          Length:16          Length:16          Length:16         
####Class :character   Class :character   Class :character   Class :character  
####Mode  :character   Mode  :character   Mode  :character   Mode  :character  

# Push your script and your .csv file to GitHub in a new "Week1" folder. [3 points]


