
# --- Chapter 5: Hypothesis testing ---

# - Hypothesis test for the mean, when standard deviation is assumed to be known - 
## - The Z-test -
### - Example one-sided Z-test -
# Example temperature
temperature <- read.table(file=file.choose(), header=TRUE)
names(temperature)
westT <- subset(temperature, temperature$Area == 'West', select = October)
sigma <- 1.5
# Step 1
mu0 <- 12
# Step 2
alpha <- 0.05
# Step 3: Select the test statistic
# Step 4
# Step 4.1: Compute test statistic
n <- length(westT$October)
xmean  <-mean(westT$October)
z  <- (xmean-mu0)/(sigma/sqrt(n))
# Step 4.2: Compute p-value
onesided.pvalue  <- pnorm(z)
result1 <- list(z = z, pvalue= onesided.pvalue)
result1

# Another way
library(BSDA)
z.test(westT$October, alternative = "less", mu = 12, sigma.x = 1.5, conf.level = 0.95)



### - Example two-sided Z-test -
# Example temperature
sigma <- 1.5
# Step 1
mu0 <- 10
# Step 2
alpha <- 0.05
# Step 3: Select the test statistic
# Step 4
# Step 4.1: Compute test statistic
n <- length(westT$October)
xmean  <-mean(westT$October)
z  <- (xmean-mu0)/(sigma/sqrt(n))

# Step 4.2: Compute p-value
twosided.pvalue  <- 2*(1-pnorm(abs(z)))
result2 <- list(z = z, pvalue = twosided.pvalue)
result2

# Another way
z.test(westT$October, alternative = "two.sided", mu = 10, sigma.x = 1.5, conf.level = 0.95)


### - Exercises - 
#### - Exercise 1: Golub data, CCND3 Cyclin D3 gene -
golub <- read.table(file=file.choose(), header = TRUE)
mu0 <- 0
alpha <- 0.05
sigma <- 0.5
n <- length(golub)
xmean <- mean(golub)
z <- (xmean - mu0)/(sigma/sqrt(n))
z
onesided.pvalue <- 1-pnorm(z)
result1 <- list(z = z, pvalue = onesided.pvalue)
result1


#### - Exercise 2: Golub data, Gdf5 gene -
mu0 <- 0
alpha <- 0.05
sigma <- 0.25
n <- length(golub)
xmean <- mean(golub)
z <- (xmean - mu0)/(sigma/sqrt(n))
z
twosided.pvalue <- 2*(1-pnorm(abs(z)))
result2 <- list(z = z, pvalue = twosided.pvalue)
result2



## - Relation confidence interval - hypothesis test -
# Example temperature
conf <- 0.95
sigma <- 1.5
n <- 9
xmean <- mean(westT$October)
alpha <- 1 - conf
lcl <- xmean-qnorm(1-alpha/2)*sigma/sqrt(n)
ucl <- xmean+qnorm(1-alpha/2)*sigma/sqrt(n)
result <- list(mean = xmean, lcl = lcl, ucl = ucl)
result


# - Hypothesis test for the mean when sigma is unknown -

## - The t-test -
t.test(westT$October, mu = 12, conf.level = 0.95, alternative = "less")

t.test(golub, mu = 0, conf.level = 0.95)


## - A non-parametric alternative: the sign test -
# Checking normality
shapiro.test(westT$October)
# Binomial Test
nr.success <- sum(westT$October - 12 > 0)
binom.test(nr.success, n = 9, alternative = "less")
# Sign test
library(BSDA)
SIGN.test(westT$October, md = 12, alternative = "less")


# - Inference for two independent samples: two-sample t-test -
## - Hypothesis test: Compare means between two independent groups -
westA <- subset(temperature, temperature$Area=='West', select = annual)
eastA <- subset(temperature, temperature$Area=='East', select = annual)
meanW <- mean(westA$annual)
meanE <- mean(eastA$annual)
varW <- var(westA$annual)
varE <- var(eastA$annual)
nW <- length(westA$annual)
nE <- length(eastA$annual)
result <- list(meanW = meanW, meanE = meanE, varW = varW, varE = varE, nW = nW, nE = nE)
result

# Boxplot 
E.or.W <- subset(temperature, temperature$Area=='West' | temperature$Area =='East')
boxplot(annual ~ Area, data = E.or.W, ylab = " ", xlab = " ")


## - Two-sample t-test in R -
### - Checking the assumptions -
shapiro.test(westA$annual)
shapiro.test(eastA$annual)
var.test(westA$annual, eastA$annual)
### - t-test -
# for equal variances
t.test(westA$annual, eastA$annual, var.equal = TRUE)
# In case the variances are not equal 
t.test(westA$annual, eastA$annual, var.equal = FALSE)



## - F-test for testing equality of variances - 
var.test(westA$annual, eastA$annual)


## - Test for normality - 
shapiro.test(westA$annual)
shapiro.test(eastA$annual)



## - Exercise - 
# Golub data
# Descriptive statistics
result <- list(meanAML = meanAML, mealALL = meanALL, varAML = varAML, varALL = varALL, nAML = nAML, nALL = nALL)
result
# Checking the assumptions
shapiro.test(golub_all)
shapiro.test(golub_aml)
var.test(golub_all, golub_aml)
# T-test
t.test(golub_all, golub_aml, var.equal = TRUE)


# - Distribution free test: Wilcoxon rank sum test -
## - Wilcoxon rank sum test - 
westeastA <- subset(temperature, temperature$Area == "West" | temperature$Area == "East", select = c(City, annual, Area))
library(doBy)
sortannual <- orderBy( ~ annual, data = westeastA)
sortannual
wilcox.test(westA$annual, eastA$annual)



# - Test for paired data - 
## - Paired t-test - 
### - Example -
# Import decatlon.txt
combine_decatlon <- read.table(file=file.choose(), header=TRUE)
# Check whether import was as expected
head(combine_decatlon)
# Check names of variables
names(combine_decatlon)

summary(combine_decathlon)
olympic <- combine_decathlon[,3]
decastar <- combine_decathlon[,2]
t.test(decastar, olympic, paired = TRUE, alternative = "greater")

# Check normality of the differences
difference <- decastar - olympic
shapiro.test(difference)


## - Distribution-free alternative: Sign test - 
### - Example -
small_decathlon <- combine_decathlon[1:6,]
small_decathlon

small_decathlon$difference <- sign(small_decathlon$decastar_1500 - small_decathlon$olympic_1500)
small_decathlon

sum.pos <- sum(small_decathlon$difference>0)
sum.pos
binom.test(sum.pos, 6, alternative = "greater")

library(BSDA)
SIGN.test(small_decathlon$decastar_1500, small_decathlon$olympic_1500, alternative = "greater")


# - Test for proportions - 
## - Hypothesis test for one proportion - 
### - Normal approximation - 
n <- length(temperature$annual)
n
number <- sum(temperature$annual>10)
number
prop.test(x = 14, n = 35, p = 0.5)


### - Normal approximation cannot be used - 
binom.test(x = 14, n = 35, p = 0.5, alternative = "two.sided")



## - Hypothesis test for two proportions - 
westA <- subset(temperature, temperature$Area=='West', select = annual)
eastA <- subset(temperature, temperature$Area=='East', select = annual)
nW <- length(westA$annual)
nE <- length(eastA$annual)
numberW <- sum(westA >= 10)
numberE <- sum(eastA >= 10)
list(nW=nW, above10W=numberW, nE=nE, above10E= numberE, propW=numberW/nW, propE=numberE/nE)

x <- c(numberW, numberE)
n <- c(nW, nE)
prop.test(x, n)


# - Chi-square goodness of fit test -
birthdays <- c(10, 12, 10 ,16)
predprob <- c(0.25, 0.25, 0.25, 0.25)
chisq.test(birthdays, p = predprob)





# - Power and sample size (in case sigma is known) - 
## - Power of the one-sample t-test -
westT <- subset(temperature, temperature$Area=='West', select=October)
n <- length(westT$October)
delta <- 1
sd <- 1.5 # sd is assumed to be known
sig.level <- 0.05
power.t.test(n = n, delta = delta, sd = sd, sig.level = sig.level, power = NULL, type = "one.sample", alternative = "two.sided")
# Vary the values of the alternative hypothesis
delta2 <- seq(from = 1, to = 2.5, by = 0.25)
power.t.test(n =n, delta = delta2, sd = sd, sig.level = sig.level, power = NULL, type = "one.sample", alternative = "two.sided")
# Change in standard deviation 
sd <- 1 
power.t.test(n =n, delta = delta, sd = sd, sig.level = sig.level, power = NULL, type = "one.sample", alternative = "two.sided")
sd <- 2 
power.t.test(n =n, delta = delta, sd = sd, sig.level = sig.level, power = NULL, type = "one.sample", alternative = "two.sided")
# Effect of sample size
sd <- 1.5
n <- seq(from = 10, to = 40, by = 5)
power.t.test(n =n, delta = delta, sd = sd, sig.level = sig.level, power = NULL, type = "one.sample", alternative = "two.sided")


## - Sample size computation - 
delta <- 1
sd <- 1.5
sig.level <- 0.05
power.t.test(n = NULL, delta = delta, sd = sd, sig.level = sig.level, power = 0.8, type = "one.sample", alternative = "two.sided")


## - Relationship between power and sample size - 
power.prop.test(n = 8, p1 = 0.4, p2 = 0.1, power = NULL, alternative = "two.sided")


# Example gene expression 
n <- 9
delta <- 0.3
sd <- 0.44
sig.level <- 0.05
power.t.test(n = n, delta = delta, sd = sd, sig.level = sig.level, power = NULL, type = "one.sample", alternative = "one.sided")
# Vary alternative hypothesis
delta2 <- seq(from = 0.3, to = 1, by  = 0.1)
power.t.test(n = n, delta = delta2, sd = sd, sig.level = sig.level, power = NULL, type = "one.sample", alternative = "one.sided")
# Compute size of sample needed
delta <- 0.3
sd <- 0.44
sig.level <- 0.05
power.t.test(n = NULL, delta = delta, sd = sd, sig.level = sig.level, power = 0.8, type = "one.sample", alternative = "one.sided")


