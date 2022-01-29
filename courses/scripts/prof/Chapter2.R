
# --- Chapter 2: Visualizing data ---

# - Introduction -


# - Categorical variables - 

##  - Frequency table -
area.freq <- table(temperature$Area)
area.freq
# or we can use xtabs
xtabs(~temperature$Area)
# Same, but relative frequencies
area.Rfreq <- table(temperature$Area)/nrow(temperature)
round(area.Rfreq, 2)
# More sophisticated table using gmodels package
install.packages("gmodels")
library(gmodels)
# Generate table for "Area" variable (temperature dataset)
CrossTable(temperature$Area, digits = 2, prop.r = FALSE, prop.c = FALSE, prop.chisq = FALSE)


## - Bar charts - 

par(mfrow = c(1,2))
# Draw a bar chart for `Area` variable with absolute frequencies
area.freq <- table(temperature$Area) # Absolute frequency
bp.f <- barplot(area.freq, xlab = "Area", ylab = "Absolute frequency", ylim = c(0,10), col = 3)
# Draw a bar chart for `Area` variable with relative frequencies
area.Rfreq <- table(temperature$Area)/nrow(temperature) # Relative frequency
bp.Rf <- barplot(area.Rfreq, xlab = "Area", ylab ="Relative frequency", ylim = c(0,0.30),col=3)

par(mfrow = c(1,1))
# Barplot sorted by frequency
barplot(sort(area.freq, decreasing = TRUE))



## - Pie charts -
# Pie chart for "Area" variable
pp.f <- pie(area.freq, main = "Area", col = rainbow(length(area.freq)))



## - Dot charts -
par(mfrow=c(1,2))
# Dot chart for "Area" variable
area.freq <- table(temperature$Area) # This is a frequency table
area.df <- data.frame(area.freq)
dp.f <- dotchart(area.df$Freq, labels = area.df$Var1, color = 4, pt.cex = 2)
# Now the same, but with relative frequency
area.Rfreq <- table(temperature$Area)/nrow(temperature)
areaR.df <- data.frame(area.Rfreq)
dp.Rf <- dotchart(areaR.df$Freq, labels = areaR.df$Var1, xlim = c(0.20, 0.30), color=6, pt.cex=2)



# - Continuous variables - 
## - Histogram -
par(mfrow=c(2,2))
# Basic histogram for "annual" variable
hist(temperature$annual)
# Define no. of bins
hist(temperature$annual, breaks = 10)
# Define specific bins
hist(temperature$annual, breaks = seq(0, 20, 2))
# Use relative frequency
hist(temperature$annual, probability = TRUE)
# Add empirical denstiy curve
lines(density(temperature$annual))




## - Boxplot -
par(mfrow = c(1,2))
# Basic boxplot
boxplot(temperature$annual, col= 11)
# Grouped boxplots
boxplot(temperature$annual ~ temperature$Area, col = 12)


# - Scatterplots - 
par(mfrow=c(1,1))
# Scatterplot for "Annual" and "Latitude" variables 
plot(temperature$annual ~ temperature$Latitude,col=2, cex=2)
# All bivariate scatterplots for dataset
plot(temperature)

