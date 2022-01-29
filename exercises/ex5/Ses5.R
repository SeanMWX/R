###########################
#### EXERCISE SESSION 5 ###
###########################

fev.df <- read.table(file=file.choose(), header=TRUE, sep=" ")

## a ##

summary(fev.df)
fev.cor <- cor(fev.df[,1:3])
fev.cor

pairs(fev.df[,1:3])

# boxplots
par(mfrow=c(1,3))
boxplot(fev.df[,1], main = "Age")
boxplot(fev.df[,2], main = "FEV")
boxplot(fev.df[,3], main = "Height")


# density plots
par(mfrow=c(1,3))
plot(density(fev.df[,1]), main = "Age")
plot(density(fev.df[,2]), main = "FEV")
plot(density(fev.df[,3]), main = "Height")

# index plots
par(mfrow=c(1,3))
plot(sort(fev.df[,1]), ylab = "", main = "Age", pch = 16)
plot(sort(fev.df[,2]), ylab = "", main = "FEV", pch = 16)
plot(sort(fev.df[,3]), ylab = "", main = "Height", pch = 16)

## b ##

X1 <- as.vector(rep(0,nrow(fev.df)))
X2 <- X1
X1 <- ifelse(fev.df$Sex == "Male", 0, 1) # Females are represented by an 1!
X2 <- ifelse(fev.df$Smoke == "No", 0, 1) # Smokers are represented by an 1!
fev.df <- data.frame(fev.df[,1:4], X1, fev.df[,5], X2)
names(fev.df) <- c("Age", "Fev", "Height", "Sex", "X1", "Smoke", "X2")

## c ##

# c.1
par(mfrow = c(1,1))
boxplot(fev.df$Fev~fev.df$Smoke)
#### CONFOUNDING ####

# c.2
fev.s <- lm(Fev~X2, data = fev.df)

summary(fev.s)

plot(fev.df[,2]~fev.df[,7])
abline(fev.s, col = "red")

# c.3

fev.fit <- fitted(fev.s) # gives the fitted values used in calculating the residuals
fev.rs <- rstandard(fev.s) # gives standardized residuals

par(mfrow = c(2, 2))
plot(fev.s)

plot(fev.rs~fev.fit, ylim = c(-4.5, 4.5))
abline(h = c(-3,3))
abline(h = 0, col = "red", lty = 2)

# We need to control for the other variables too!

## d ##

# d.2
# Full model
fev.F <- lm(Fev~Height+Age+X1+X2+
              I(X1*Height)+I(X1*Age)+
              I(X2*Height)+I(X2*Age), 
            data = fev.df)

summary(fev.F)
# Results are now in line with intuition

fevF.fit <- fitted(fev.F) # gives the fitted values used in calculating the residuals
fevF.rs <- rstandard(fev.F) # gives standardized residuals

# Looking for polynomial terms
par(mfrow = c(1, 1))
plot(fevF.rs~fevF.fit, ylim = c(-4.5, 4.5))
abline(h = c(-3,3))
abline(h = 0, col = "red", lty = 2)

scatter.smooth(fev.df$Age, fev.df$Fev, lpars = list(col="Red"))
scatter.smooth(fev.df$Height, fev.df$Fev, lpars = list(col="Red"))

# Include polynomial term in full model
fev.P <- lm(Fev~Height+Age+X1+X2+
              I(Height^2)+
              I(X1*Height)+I(X1*Age)+
              I(X2*Height)+I(X2*Age), 
            data = fev.df)

summary(fev.P)
par(mfrow = c(2, 2))
plot(fev.P)

# Checking polynomial fit
fevP.fit <- fitted(fev.P) # gives the fitted values used in calculating the residuals
fevP.rs <- rstandard(fev.P) # gives standardized residuals

par(mfrow=c(1,1))
plot(fevF.rs~fevF.fit, ylim = c(-4.5, 4.5), main = "First-order")
abline(h = c(-3,3))
abline(h = 0, col = "red", lty = 2)

# Significant improvement
plot(fevP.rs~fevP.fit, ylim = c(-4.5, 4.5), main = "Polynomial")
abline(h = c(-3,3))
abline(h = 0, col = "red", lty = 2)
# Mild heteroscedasticity is present but otherwise OK! 

# Reduce the model via AIC (backward selection)
fev.R <- step(fev.P, direction = "backward")
summary(fev.R)
# What is the effect of smoking on FEV, according to this model?

