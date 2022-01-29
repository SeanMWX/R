###########################
#### EXERCISE SESSION 3 ###
###########################

# EXERCISE 1

Iran <- c(128,125,133,104,146,132,125,118,129,124)
Belgium <- c(160,128,169,105,151,164,162,177,185,150,182,158,156,123,141,176,162,172)

# a
# check normality:
shapiro.test(Iran)
shapiro.test(Belgium)
# check variances equal or not:
var.test(Iran, Belgium)

# c & d
t.test(Iran, Belgium, var.equal=TRUE, conf.level=0.90)

# e
t.test(Iran, Belgium, var.equal=FALSE, alternative = c("less"), conf.level=0.90)

# EXERCISE 2

blood.df <- read.table(file=file.choose(), header=TRUE, sep=",")

# a
blood.Y <- subset(blood.df, age < 50)
blood.O <- subset(blood.df, age > 68)

# c
nrow(blood.Y)
nrow(blood.O)
# Small subsets, so check normality:
shapiro.test(blood.Y$testost)
shapiro.test(blood.O$testost)
# at least one is non-normal, use non-parametric test for 2 samples: "Wilcoxon rank sum test"

# d
t.test(blood.Y$testost, blood.O$testost)
wilcox.test(blood.Y$testost, blood.O$testost, exact = FALSE)
# Put exact = FALSE since there are ties in the data


# EXERCISE 3 https://en.wikipedia.org/wiki/Correlation_does_not_imply_causation

install.packages("faraway")
library(faraway)
tvdoc.df <- tvdoctor

# a & b & c
apply(tvdoc.df, 2, FUN = shapiro.test)
# cor(tvdoc.df, method = "pearson")
cor(tvdoc.df, method = "spearman")

# d
pairs(tvdoc.df)

# exercise 4

senic.df <- read.table(file=file.choose(), header=TRUE, sep="\t")

# a
senic.sub <- data.frame(senic.df$length, senic.df$risk, senic.df$fac, senic.df$xray)

senic.var <- diag(var(senic.sub))
senic.sd <- diag(var(senic.sub)^0.5)
summary(senic.sub)
senic.var
senic.sd

par(mfrow=c(2,2))
boxplot(senic.df$length, main = "length")
boxplot(senic.df$risk, main = "risk")
boxplot(senic.df$fac, main = "facilities")
boxplot(senic.df$xray, main = "X-ray")

pairs(senic.sub)

# some interesting additional plots
# density plots
par(mfrow=c(2,2))
plot(density(senic.df$length), main = "length")
plot(density(senic.df$risk), main = "risk")
plot(density(senic.df$fac), main = "facilities")
plot(density(senic.df$xray), main = "X-ray")

# # index plot of sorted values
# par(mfrow=c(2,2))
# plot(sort(senic.df$length), main = "length", cex = 1.2)
# plot(sort(senic.df$risk), main = "risk", cex = 1.2)
# plot(sort(senic.df$fac), main = "facilities", cex = 1.2)
# plot(sort(senic.df$xray), main = "X-ray", cex = 1.2)

# b
# Test for normality
apply(senic.sub, 2, FUN = shapiro.test)
# Normality is rejected for length of stay and comes close to being rejected for infection risk
# Use Spearman correlation
senic.cor <- cor(senic.sub, method = "spearman")
senic.cor[c(1,3,4),2]

# c
senic.lmL <- lm(risk~length, data = senic.df)
summary(senic.lmL)

par(mfrow=c(1,1))
plot(risk~length, data = senic.df, main = "risk~length", xlab = "Length", ylab = "Risk")
abline(senic.lmL)
# Two outliers in length of stay in the far right

# (Optional!)
# Same as above, but without outliers, observations 47 and 112
# senic.wo <- senic.df[c(1:46,48:111,113),]

# c.2
senic.lmL2 <- lm(risk~length, data = senic.df)
summary(senic.lmL2)

par(mfrow=c(1,1))
plot(risk~length, data = senic.df, xlim = c(6.5,20), main = "risk~length", xlab = "Length", ylab = "Risk")
abline(senic.lmL2, col = "RED")

# d
senic.lmF <- lm(risk~fac, data = senic.df)
summary(senic.lmF)

#par(mfrow=c(1,1))
plot(risk~fac, data = senic.df, main = "risk~facilities", xlab = "Facilities", ylab = "Risk")
abline(senic.lmF)

# e
senic.lmX <- lm(risk~xray, data = senic.df)
summary(senic.lmX)

#par(mfrow=c(1,1))
plot(risk~xray, data = senic.df, main = "risk~X-ray", xlab = "X-ray", ylab = "Risk")
abline(senic.lmX)

# (Optional!)
# Multiple linear regression 
senic.df$reg <- as.factor(senic.df$reg)
lm.f <- lm(risk~.-ID, data = senic.df)
summary(lm.f)
par(mfrow = c(2, 2))
plot(lm.f)
