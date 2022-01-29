###########################
#### EXERCISE SESSION 7 ###
###########################

lab.df <- read.table(file=file.choose(), header=TRUE, sep="\t")

summary(lab.df)

x <- c(1,3,4)
for (i in 1:length(x)){
  lab.df[,x[i]] <- as.factor(lab.df[,x[i]])
}

summary(lab.df)

## b
Mean <- rowMeans(lab.df[,6:9])
lab.df <- data.frame(lab.df, Mean)

## c
Diff <- lab.df[,5] - lab.df[,10]
lab.df <- data.frame(lab.df, Diff)

## d
# d.1
#install.packages("psych")
library(psych)

describe <- describeBy(lab.df$Diff, list(lab.df$Lab, lab.df$Rep), mat=TRUE)
describe.st <- subset(describe, select=c("group1", "group2", "mean", "sd", "n"))
describe.st

# d.2
interaction.plot(lab.df$Lab, lab.df$Rep, lab.df$Diff, type="b", pch=c(18,24), col=c(1,2))

# d.3
# Full model
lab.aov <- aov(Diff~Lab*Rep, 
               contrasts=list(Lab="contr.sum", Rep="contr.sum"),
               data = lab.df)
summary(lab.aov)

# Reduced model without interaction
lab.aov2 <- aov(Diff~Lab+Rep, 
                contrasts=list(Lab="contr.sum", Rep="contr.sum"),
                data = lab.df)
summary(lab.aov2)

# d.4
# Checking homogeneity of variance for each factor seperately
#install.packages("car")
library(car)
leveneTest(Diff~Lab, data = lab.df)
leveneTest(Diff~Rep, data = lab.df)

# Using more robust ANOVA
Anova(lab.aov2, type='III',white.adjust='hc3')

# Checking for outlying values
number <- 1:nrow(lab.df)
cook <- cooks.distance(lab.aov2)
par(mfrow=c(1,1))
plot(cook~ number)
#identify(cook, labels=number)

# Checking for normality of residuals
shapiro.test(lab.aov2$residuals)
hist(lab.aov2$residuals)

# Using non-parametric test for differences between labs (One-Way ANOVA)
kruskal.test(Diff~Lab, data = lab.df)

# d.5
# Using non-parametric method for comparing means
# install.packages("pgirmess")
library(pgirmess)
kruskalmc(Diff~Lab, data = lab.df)

# Plotting significantly different means
# in easy to interpret dotchart
lab.kru <- data.frame(kruskalmc(Diff~Lab, data = lab.df))
lab.kru <- lab.kru[,3:5]
kru.sig <- as.data.frame(matrix(data = NA, nrow = nrow(lab.kru), ncol = ncol(lab.kru), dimnames = dimnames(lab.kru)))
for(i in 1:nrow(lab.kru)){
  if(lab.kru[i,3] == "TRUE"){
    kru.sig[i,] <- lab.kru[i,]
  }
}

kru.sig <- na.omit(kru.sig)

dotchart(kru.sig[,1], labels = rownames(kru.sig), col = "red", xlim = c(52, 90))
abline(v = 52.39141, lty = 2, col = "grey")

###########################
####       EXTRA        ###
###########################

# we can introduce the "Solution" factor in the model, but this is nested in the lab factor

lab.aov3 <- aov(Diff~Lab+Rep+Sol%in%Lab+Lab:Rep, 
               contrasts=list(Lab="contr.sum", Rep="contr.sum", Sol="contr.sum"),
               data = lab.df)
summary(lab.aov3)
interaction.plot(lab.df$Lab, lab.df$Sol, lab.df$Diff, type="b", pch=c(18,24), col=c(1,2))

anova(lab.aov, lab.aov3)
