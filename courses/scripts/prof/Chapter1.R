
# --- Chapter 1: Descriptive Statistics ---

# Import temp_warm.txt
temperature <- read.table(file=file.choose(), header=TRUE)

# Check whether import was as expected
head(temperature)
# Check names of variables
names(temperature)


# - Measure of Central Value - 

## - The mean - 
# Compute average October temperature
mean(temperature$October)
summary(temperature$October)
# Summary statistics for October temperature by Area
library(dplyr)
by_area <- group_by(temperature, Area)
summarise(by_area, mean(October), n = n())


## - The median -
median(temperature$October)
by_area <- group_by(temperature, Area)
summarise(by_area, mean(October), n = n(), median(October))


## - Percentiles / Quartiles - 
q1 <- quantile(temperature$October, 0.25)
q1
q3 <- quantile(temperature$October, 0.75)
q3
quant <- quantile(temperature$October)
quant
summarise(group_by(temperature, Area), median(October), n = n(), quantile(October, 0.25), quantile(October, 0.75))


## - The mode -
# Frequency table of the variable 'Area'
table <- table(temperature$Area)
table
# Computing the mode
which.max(table)


# - Measure of dispersion (or variability) - 

## - The range - 
max(temperature$October) - min(temperature$October)

## - The variance - 
varT <- var(temperature$October)
varT


## - The standard deviation - 
sdT <- sd(temperature$October)
sdT


## - Interquartile range - 
IQRT <- IQR(temperature$October)
IQRT


