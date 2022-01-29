
# --- Chapter 10: Logistic regression ---

# - Introduction -
# Import political_party.xlsx as political_party
names(political_party)

# - Simple logistic regression -
## - How to obtain parameter estimates in R - 
PP <- political_party
names(PP) 
head(PP)

# Model 1
glm.log1 <- glm(Republican ~ pro_capital_punishment, family = binomial(link = logit), data = PP)
summary(glm.log1)
# Look at predicted values
combine <- data.frame(cbind(PP$pro_capital_punishment, PP$Republican, fitted(glm.log1)))
colnames(combine) <- c("pro_capital_punishment", "Republican", "Fitted value")
head(combine,5)
# Visualization of predicted and observed values
plot(PP$pro_capital_punishment, PP$Republican, type="p", col="red")
points(PP$pro_capital_punishment, fitted(glm.log1), col="black")


## - Interpretation of b1 -
### - Example interpreting odds ratio for continuous explanatory variable - 
glm.log1$coefficients
exp(glm.log1$coefficients)

# Consider subject 1 and subject 4
head(combine, 5)
b0 <- glm.log1$coefficients[1]
b1 <- glm.log1$coefficients[2]
P1_sub1 <- inv.logit(b0+b1*combine[1,"pro_capital_punishment"])
P0_sub1 <- 1- P1_sub1
odds1 <- P1_sub1/P0_sub1
P1_sub4 <- inv.logit(b0+b1*combine[4,"pro_capital_punishment"])
P0_sub4 <- 1- P1_sub4
odds4 <- P1_sub4/P0_sub4
OR <- odds4/odds1
list("Subject1, P(Republican=1)" = P1_sub1, 
     "Subject1, P(Republican=0)" = P0_sub1,
     "Subject1, Odds" = odds1,
     "Subject4, P(Republican=1)" = P1_sub4,
     "Subject4, P(Republican=0)" = P0_sub4,
     "Subject4, Odds" = odds4,
     "Odds ratio" = OR)


## - Simple logistic regression model with categorical explanatory variable -
### - Use of binary predictor variables -
# Logistic regression with binary explanatory variable
glm.log2 <- glm(Republican ~ gender, family = binomial(link = "logit"), data = PP)
summary(glm.log2)
# To obtain odds ratio
exp(glm.log2$coefficients)
# Estimated probabilities
combine2 <- data.frame(cbind(PP$gender, PP$Republican, fitted(glm.log2)))
colnames(combine2) <- c("gender", "Republican", "Fitted value")
combine2[c(1,18:22),]



### - Use of categorical predictor variable (not binary)
# Import titanic.xlsx as titanic
names(titanic)

titanic$class.f <- as.factor(titanic$Class_New)
glm.log1 <- glm(survived ~ class.f, family = binomial(link = logit), data = titanic)
summary(glm.log1)

# We want to change the reference group.
# We want Class_New = 4 to be the reference category. 
titanic$Class_Ref <- relevel(titanic$class.f, ref = "4")
glm.log2 <- glm(survived ~ Class_Ref, family = binomial(link = logit), data = titanic)
summary(glm.log2)

# To obtain the odds ratio
exp(glm.log2$coefficients)



## - Goodness of fit -
glm.log1 <- glm(Republican ~ pro_capital_punishment, family = binomial(link = logit), data = PP)
summary(glm.log1)

### - Hosmer-Lemeshow goodness of fit test -
install.packages("ResourceSelection")
library(ResourceSelection)
Republican <- PP$Republican
hoslem <- hoslem.test(Republican, fitted(glm.log1))
hoslem
combine <- cbind (hoslem$observed, hoslem$expected)
combine


### - Wald test to test significance of regression coefficients -
summary(glm(formula = Republican ~ pro_capital_punishment, family = binomial(link = logit), data = PP))$coefficients


### - Deviance -
summary(glm(formula = Republican ~ pro_capital_punishment, family = binomial(link = logit), data = PP))$deviance

### - Pseudo R² -
library(pscl)
pR2(glm.log1)



## - Classification of observations
glm.log1 <- glm(Republican ~ pro_capital_punishment, family = binomial(link = logit), data = PP)
combine <- data.frame(cbind(PP$pro_capital_punishment, PP$Republican, fitted(glm.log1)))
colnames(combine) <- c("pro_capital_punishment", "Republican", "Fitted value")
head(combine,5)
# Classification table
table(Republican, fitted(glm.log1) > 0.5)


## - ROC curve -
# Classification table for a cut-off value of 0.5
table(Republican, fitted(glm.log1) > 0.5)
# Classification table for a cut-off value of 0.9
table(Republican, fitted(glm.log1) > 0.9)
# Classification table for a cut-off value of 0.1
table(Republican, fitted(glm.log1) > 0.1)

# To obtain a ROC curve in R
install.packages("ROCR")
library(ROCR)
glm.log1 <- glm(Republican ~ pro_capital_punishment, family = binomial(link = logit), data = PP)
predict <- fitted(glm.log1)
pred <- prediction(predict, Republican)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# Colorize argument in following plot function: 
# This logical determines whether the curve(s) 
# should be colorized according to cutoff
plot(perf, main = "sensitivity vs false positive rate", colorize = TRUE, colorkey.relwidth = 0.5, lwd = 4.5)

# Area under the ROC curve
perf_auc <- performance(pred, measure = "auc")
perf_auc@y.values


# Example political party 
glm.log2 <- glm(Republican ~ gender, family = binomial(link = "logit"), data = PP)
pred2 <- prediction(fitted(glm.log2), Republican)
perf2 <- performance(pred2, measure = "tpr", x.measure = "fpr")
plot(perf2, main = "sensitivity vs false positive rate", colorize = TRUE, colorkey.relwidth = 0.5, lwd = 4.5)
# AUC
performance(pred2, measure = "auc")@y.values

# To show two ROC curves on the same plot
plot(perf, colorize = TRUE, lwd = 3)
plot(perf2, add = TRUE, colorize = TRUE, lwd = 3)
abline(0, 1, lty = 2)


# - Multiple logistic regression - 

## - Example -
### - Hierarchical step by step (manually)
# Model A: Model with 4 explanatory variables 
glm.log.A <- glm(Republican ~ pro_capital_punishment + pro_welfare_reform + pro_fed_support_ed + gender, family = binomial(link = logit), data = PP)
summary(glm.log.A)
# Model B: Model with 3 explanatory variables
glm.log.B <- glm(Republican ~ pro_capital_punishment + pro_welfare_reform + gender, family = binomial(link = logit), data = PP)
summary(glm.log.B)
# Model C: Model with 2 explanatory variables
glm.log.C <- glm(Republican ~ pro_capital_punishment + gender, family = binomial(link = logit), data = PP)
summary(glm.log.C)


### - Hierarchical step by step by R
glm.log.forward <- step(glm(Republican ~ 1, family = binomial(link = logit), data = PP), scope = ~ pro_capital_punishment + pro_welfare_reform + pro_fed_support_ed + gender, direction = "forward")


## - Partial deviance - 
### - Example - 
glm.log.M1 <- glm(Republican ~ gender, family = binomial(link = logit), data = PP)
glm.log.M2 <- glm(Republican ~ gender + pro_capital_punishment, family = binomial(link = logit), data = PP)
glm.log.M3 <- glm(Republican ~ gender + pro_capital_punishment + pro_welfare_reform, family = binomial(link = logit), data = PP)
glm.log.M4 <- glm(Republican ~ gender + pro_capital_punishment + pro_welfare_reform + pro_fed_support_ed, family = binomial(link = logit), data = PP)

# a) Compare model 2 to model 1
anova(glm.log.M1, glm.log.M2, test = "Chisq") # Note the argument 'test = "Chisq"'!
# b) Compare model 3 to model 2
anova(glm.log.M2, glm.log.M3, test = "Chisq")
# c) Compare model 4 to model 2
anova(glm.log.M2, glm.log.M4, test = "Chisq")


## - Interpreting the output -
### - Interpreting the parameter estimates - 
summary(glm.log.M2)$coefficients
exp(glm.log.M2$coefficients)

### - Classification table - 
table(Republican, fitted(glm.log.M2)>0.5)

### - Generalized R² value - 
library(pscl)
pR2(glm.log.M2)

### - Create the ROC curve and area under the curve - 
pred.M2 <- prediction(fitted(glm.log.M2), Republican)
perf.M2 <- performance(pred.M2, measure = "tpr", x.measure = "fpr")
plot(perf.M2, main = "sensitivity vs false positive rate", colorize = TRUE, colorkey.relwidth = 0.5, lwd = 4.5)
performance(pred.M2, measure = "auc")@y.values







