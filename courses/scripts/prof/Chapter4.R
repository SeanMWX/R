
# --- Chapter 4: Confidence intervals ---

# - Construction of a 95% confidence interval for µ, in case sigma is known. -
# Example temperature 
# import temp_warm.txt
temperature <- read.table(file=file.choose(), header=TRUE)
names(temperature)
westT <- subset(temperature, temperature$Area=="West", select = October)
summary(westT) # summary statistics


## - Changing the confidence level (in case sigma is known) -
confidence_level <- 0.95
alpha <- 1 - confidence_level
sigma <- 1.5
n <- 9
westT <- subset(temperature, temperature$Area=="West", select = October)
xmean <- mean(westT$October)
lcl <- xmean - qnorm(1-alpha/2)*sigma/sqrt(n)
ucl <- xmean + qnorm(1-alpha/2)*sigma/sqrt(n)
result <- list(mean = xmean, lcl = lcl, ucl = ucl)
result

# package BSDA
library(BSDA)
z.test(westT$October, sigma.x = 1.5)


# - Confidence interval in case sigma is not known (real life) - 

## - Exercise -
westT <- subset(temperature, temperature$Area=="West", select = October)
t.test(westT$October)

# Check for normality
shapiro.test(westT$October)


# - Calculating sample sizes (in case sigma is known) - 
# - Confidence interval for a proportion - 
## - In R: use prop.test(x, n) -
prop.test(240, 400)



# - Exercises -

## - Bio-informatics: Solution -
conf <- 0.99
alpha <- 1-conf
sigma <- 8
n <- 4
xmean <- 101.4
B <- 2
lcl <- xmean-qnorm(1-alpha/2)*sigma/sqrt(n)
ucl <- xmean+qnorm(1-alpha/2)*sigma/sqrt(n)
n <- (qnorm(1-alpha/2)*sigma/B)^2
result <- list(lcl = lcl, ucl = ucl, n = n)
result


library(BSDA)
zsum.test(mean.x = 101.4, sigma.x = 8, n.x = 4, conf.level = 0.99)


