
# --- Chapter 9: Analysis of Variance (ANOVA) ---

# - One-way ANOVA - Introductory example -
# import pollutie2.txt
pollution <- read.table(file=file.choose(), header=TRUE)
summary(pollution)
head(pollution, n = 15)

## - Descriptive statistics -
install.packages("psych")
library(psych)
rain <- pollution$Rain
region <- pollution$regio

describe <- describeBy(rain, region, mat = TRUE)
describe.st <- subset(describe, select = c("group1", "n", "mean", "sd", "median", "min", "max"))
describe.st
boxplot(rain ~ region, col = 2, names = levels(pollution$regio))

# Another way
library(tidyverse)
by_region <- group_by(pollution, regio)
summarise(by_region, Avgrain = mean(Rain))




# - F-test for multiple means in one-way ANOVA -
## - Example -
glm1 <- lm(Rain  ~ regio, data = pollution)
summary(glm1)



# - One-way ANOVA as a linear model -
## - Use of treament coding -
summary(glm1)
model.matrix(glm1)

## - Use of sum coding -
glm2 <- lm(Rain ~ regio, data = pollution, contrasts = list(regio = "contr.sum"))
summary(glm2)
head(model.matrix(glm2))




# - Model diagnostics -
## - Check assumption of homogeneity of variance -
library(car)
leveneTest(rain ~ region)
# In case there is no homogeneity of variance, a modification of the F-test can be used:
oneway.test(Rain ~ regio, data = pollution, var.equal = FALSE)

## - Check assumption of normality -
# What are the residuals? 
by_region <- group_by(pollution, regio)
summarise(by_region, n = n(), mean = mean(Rain), sd = sd(Rain), median = median(Rain), min = min(Rain), max = max(Rain))
head(pollution, n = 5)
# Residuals obtained with R
glm1 <- lm(Rain  ~ regio, data = pollution)
combine <- data.frame(city = pollution$city, region = pollution$regio, residuals = glm1$residuals)
head(combine, n = 5)
# Test normality of the within-group residuals:
shapiro.test(glm1$residuals)
hist(glm1$residuals)



## - Checking for influential observations -
# Plot Cook's distance versus observation number
plot(cooks.distance(glm1))




# - Pairwise comparisons of treatment effects - 
## - Questions post-hoc -
poll.aov1 <- aov(Rain ~ regio, data = pollution) # required for multiple comparisons in R
summary(poll.aov1)

## - Planned comparison of means in ANOVA -
pairwise.t.test(rain, region, p.adjust.method = "none")

## - Multiple comparisons -
# Tukey studentized range test 
diffs <- TukeyHSD(poll.aov1, whih = "region", conf.level = 0.95)
diffs
plot(diffs)

library(agricolae)
# Another HSD test
HSD.test(poll.aov1, "regio", group = FALSE)$comparison

# Scheffé multiple contrasts test 
scheffe.test(poll.aov1, "regio", group = FALSE)$comparison

# Bonferroni method
pairwise.t.test(rain, region, p.adj="bonferroni")

# Scheffé, homogeneous groups
Hgroups.scheffe <- scheffe.test(poll.aov1, "regio", group = TRUE)
Hgroups.scheffe$groups

# Pairwise comparisons between group levels calculated with non-adjusted p-values
pairwise.t.test(rain, region, p.adj = "none")

# Holm's approach 
pairwise.t.test(rain, region, p.adj = "holm")


# - Extensions -
## - The Kruskal-Wallis test -
# Sort data 
subpol <- pollution[,c("Rain", "regio")]
library("doBy")
sortsubpol <- orderBy( ~ Rain, data = subpol)
sortsubpol

# Kruskal-Wallis statistic 
kruskal.test(rain ~ region)


# multi comparisons 
library(pgirmess)
kruskalmc(rain, region)




# - Two-way ANOVA -
## - Introduction - 
# Import diet.txt 
diet_df <- read.table(file=file.choose(), header=TRUE)
head(diet_df)

# Descriptive statistics 
names(diet_df)
by_diet_jogging <- group_by(diet_df, DIET, JOGGING)
summarise(by_diet_jogging, Avgloss = mean(LOSS),SDloss=sd(LOSS),number=n())

# Descriptive statistics by diet
by_diet <- group_by(diet_df, DIET)
summarise(by_diet, Avgloss = mean(LOSS),SDloss=sd(LOSS),number=n())

# Descriptive statitics by jogging
by_jogging <- group_by(diet_df,  JOGGING)
summarise(by_jogging, Avgloss = mean(LOSS),SDloss=sd(LOSS),number=n())

# Box plots
boxplot(LOSS ~ JOGGING, col=2, data=diet_df)
boxplot(LOSS ~ DIET, col=3, data=diet_df)

?interaction.plot
# Visualization of the mean values for every combination of the factors diet and jogging
diet.f <- as.factor(diet_df$DIET)
jogging.f <- as.factor(diet_df$JOGGING)
loss <- diet_df$LOSS
interaction.plot(diet.f, jogging.f, loss, type = "b", pch = c(18, 24, 22), col = c(1, 2, 3))



## - Strategy for the analysis of two-way ANOVA studies -
### - Example in R -
# Use sum coding 
diet.aov1 <- aov(LOSS ~ JOGGING + DIET + JOGGING*DIET, data = diet_df, contrasts = list(JOGGING = "contr.sum", DIET = "contr.sum"))
summary(diet.aov1)

# Drop interaction term and refit
diet.aov2 <- aov(LOSS ~ JOGGING + DIET, data = diet_df, contrasts = list(JOGGING = "contr.sum", DIET = "contr.sum"))
summary(diet.aov2)

# Use lm
diet.lm <- lm(LOSS ~ JOGGING + DIET + JOGGING * DIET, data = diet_df, contrasts = list(JOGGING ="contr.sum", DIET = "contr.sum"))
# To see the ANOVA table
Anova(diet.lm, type = "III") # function from package 'car'


## - Diagnostics - 

### - Checking homogeneity of variances -
library(car)
# Checking homogeneity of variances for jogging
leveneTest(LOSS ~ JOGGING, data = diet_df) # This is a function from the package 'car'
# Checking homogeneity of variances for diet
leveneTest(LOSS ~ DIET, data = diet_df)

# Robust analysis (if there is an indication of unequal variances)
Anova(diet.aov2, type = "III", white.adjust = "hc3")


### - Checking normality of residuals - 
shapiro.test(diet.aov2$residuals)
hist(diet.aov2$residuals)

### - Influential observations -
plot(cooks.distance(diet.aov2))
diet_df[cooks.distance(diet.aov2)>0.3,]
# Delete observation 22
diet_df_small <- diet_df[-22,] 

loss_small <- diet_df_small$LOSS
jogging_small <- diet_df_small$JOGGING
diet_small <- diet_df_small$DIET

# Two-way ANOVA
diet.aov1_small <- aov(loss_small ~ jogging_small + diet_small + jogging_small * diet_small, contrasts = list(jogging_small = "contr.sum", diet_small = "contr.sum"))
summary(diet.aov1_small)

# Two-way ANOVA without interaction
diet.aov2_small <- aov(loss_small ~ jogging_small + diet_small, contrasts = list(jogging_small = "contr.sum", diet_small = "contr.sum"))
summary(diet.aov2_small)



## - Multiple comparisons for the main effects - 
library(agricolae)
library(sp)
diet.aov2 <- aov(LOSS ~ JOGGING + DIET, data = diet_df, contrasts = list(JOGGING = "contr.sum", DIET = "contr.sum"))

# Tukey HSD test for Jogging
out <-  HSD.test(diet.aov2, "JOGGING", group = FALSE)
out$means
out$comparison

# Tukey HSD test for diet
out1 <- HSD.test(diet.aov2, "DIET", group = FALSE)
out1$means
out1$comparison



## - Two-way ANOVA when cells have unequal sample size -
### - Illustrative example -
# Import training.txt
training <- read.table(file=file.choose(), header=TRUE)
head(training)

# Descriptive statistics 
by_Method_SepPeriod <- group_by(training, Method, Sep_Period)
summarise(by_Method_SepPeriod, Avg = mean(score), SD = sd(score), number = n())

# Visualization 
library(stats)
interaction.plot(training$Sep_Period, training$Method, training$score, type="b", pch = c(18, 24, 22), col = c(1, 2, 3))


### - ANOVA table -
# ANOVA table in case of unbalanced design: use of lm function
training.lm <- lm(score ~ Method + Sep_Period + Method*Sep_Period, data = training, contrasts = list(Method = "contr.sum", Sep_Period = "contr.sum"))
Anova(training.lm, type = "III")

### - Diagnostics -
# Check assumption of normality 
shapiro.test(training.lm$residuals)
hist(training.lm$residuals)
# Check assumption of homogeneity of variances 
leveneTest(score ~ Method*Sep_Period, data = training)
# Check influential observations 
plot(cooks.distance(training.lm))


### - Pairwise comparisons of treatment effects -
## Create a new variable with the interaction term
# Initialize the new variable
method_sep_period <- character(length(training$score))
# Store values in new variable
for (i in 1: length(training$score))
{method_sep_period[i] <- paste(substring(training$Method[i], 1, 4), training$Sep_Period[i], sep="")}
# Create a new data frame
new_df <- data.frame(score = training$score, method_sep_period)
head(new_df, n = 10)

# Descriptive statistics 
describe <- describeBy(new_df$score, new_df$method_sep_period, mat = TRUE)
describe.st <- subset(describe, select=c("group1", "n", "mean", "sd", "median", "min", "max"))
describe.st

# Apply one-way ANOVA on this new data frame
new_df.aov <- aov(score ~ method_sep_period, new_df, contrasts = list(method_sep_period = "contr.sum"))
summary(new_df.aov)

# Ask for Tukey HSD test
out2 <- HSD.test(new_df.aov, "method_sep_period", group = FALSE)
round(out2$means, 2)
out2$comparison




