# With the data frame you created last week you will:


e<- c('d','e','f','g','h','i','j','k','l','m','n','o','p','q','r')
e

d<- c('a','a','a','a','a','b','b','b','b','b','c','c','c','c','c')
d

a<- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
a 

b<- c(1,1,1,2,2,6,8,8,9,11,13,13,14,14,15)
b 

c<- c(1.1,2.2,3.3,4.4,5.5,6.6,7.7,8.8,9.9,10.1,11.11,12.12,13.13,14.14,15.15)
c


data<- cbind(e,d,a,b,c)
data

df<- as.data.frame(data)
df

colnames(df) <- c('unique','three','ordered','repeats','decimals')
df

row.names(df) <- df$unique
df



df.b <- df[,-1]
df.b


# Create a barplot for one numeric column, grouped by the character vector with 3 unique values (10 points)

df.b$ordered <- as.numeric(as.character(df.b$ordered))
df.b$repeats <- as.numeric(as.character(df.b$repeats))
df.b$decimals <- as.numeric(as.character(df.b$decimals))


df.mean <- aggregate(df.b$decimals ~df.b$three, FUN = "mean")
df.mean

colnames(df.mean) <- c("Factor","Mean")
df.mean

barplot(df.mean$Mean)
barplot(df.mean$Mean, names.arg = df.mean$Factor)
# Add error bars with mean and standard deviation to the plot

df.sd <- aggregate(df.b$decimals ~df.b$three, FUN = "sd")
colnames(df.sd) <- c("Factor","StanDev")
df.sd

b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Factor)

arrows(b.plot, df.mean$Mean-df.sd$StanDev,
       b.plot, df.mean$Mean+df.sd$StanDev,angle=90,code=3)
b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Factor, ylim = c(0,20))

arrows(b.plot, df.mean$Mean-df.sd$StanDev,
       b.plot, df.mean$Mean+df.sd$StanDev,angle=90,code=3)
barplot(df.mean$Mean, names.arg = df.mean$Factor, ylim = c(0,5), horiz = TRUE)

  # Change the x and y labels and add a title

b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Factor, ylim = c(0,20),xlab = "Explanatory", ylab = "Response", main="BAR PLOT")



  # Export the plot as a PDF that is 4 inches wide and 7 inches tall.

setwd("C:/GitHub/joyse/plots")

getwd()






# Create a scatter plot between two of your numeric columns. (10 points)

plot(df.b$ordered ~ df.b$repeats)

  # Change the point shape and color to something NOT used in the example.
?pch

plot(df.b$ordered ~ df.b$repeats, xlab = "Explanatory", ylab = "Response", main = "Scatterplot", 
     cex.axis=0.75, cex.main = 0.75, cex.lab = 0.75, pch=10, col="firebrick4")

  # Change the x and y labels and add a title

plot(df.b$ordered ~ df.b$repeats, xlab = "Explanatory", ylab = "Response", main = "Scatterplot", 
     cex.axis=0.75, cex.main = 0.75, cex.lab = 0.75, pch=10, col="firebrick4")
  # Export the plot as a JPEG by using the "Export" button in the plotting pane.

getwd()
setwd("C:/GitHub/joyse/plots")


# Upload both plots with the script used to create them to GitHub. (5 points)
  # Follow the same file naming format as last week for the script.
  # Name plots as Lastname_barplot or Lastname_scatterplot. Save them to your "plots" folder. (5 points)
