###########################
#### EXERCISE SESSION 4 ###
###########################

body.df <- read.table(file=file.choose(), row.names = 1, header=TRUE)

## a ##

summary(body.df)

# preparing for plotting, we need columns 1 to 15
body.names <- names(body.df[,1:15])

# Check your working directory for the output of the following!!
par(mfrow = c(3, 5))
# boxplot by for-loop
for (i in 1:length(body.names)){
  png(paste(paste("boxplot",i, sep = "_"),"png", sep = "."))
  boxplot(body.df[,i], main = paste(body.names[i])) 
 dev.off()
}


# density plots by for-loop
for (i in 1:length(body.names)){
  png(paste(paste("density",i, sep = "_"),"png", sep = "."))
  plot(density(body.df[,i]), main = paste(body.names[i]))
  dev.off()
}

# index plot by for-loop
for (i in 1:length(body.names)){
  png(paste(paste("index",i, sep = "_"),"png", sep = "."))
  plot(sort(body.df[,i]), ylab = "", main = paste(body.names[i]), pch = 16)
  dev.off()
}
par(mfrow = c(1, 1))
plot(sort(body.df[,3]), ylab = "", main = paste(body.names[3]), pch = 16)
identify(sort(body.df[,3]), labels=row.names(body.df), n = 2)

body.cor <- cor(body.df[,1:15])
body.cor[1,]

## b ##

X1 <- as.vector(rep(0,nrow(body.df)))
X1 <- ifelse(body.df$Sex == "Male", 0, 1)
body.df <- data.frame(body.df, X1)

## c ##

body.s <- sample(1:nrow(body.df), 50, replace=FALSE)
body.mod <- body.df[-body.s,]
body.new <- body.df[body.s,]

## d ##

d.2
body.names <- names(body.df[1:15])
# par(mfrow = c(2, 7))
for (k in 2:length(body.names)){
  png(paste(paste("scatter",k, sep = "_"),"png", sep = "."))
  plot(body.mod[,1]~body.mod[,k], xlab = paste(body.names[k]), ylab = "Weight", main = "")
  lines(stats::lowess(body.mod[,1]~body.mod[,k]), col = "RED")
  dev.off()
  }

# d.3
# full model - no interaction
body.lm.F <- lm(Weight~Height+Shoulder+Chest+Waist+Abdo+Hip+Thigh+Bicep+Forearm+Knee+Calf+Ankle+Wrist+Age,
                data = body.mod)
                
summary(body.lm.F)

# sorting P-values
lab <- labels(summary(body.lm.F)$coefficients)
body.p <- data.frame(lab[[1]],summary(body.lm.F)$coefficients[,4])
names(body.p) <- c("lab","P")

body.p <- body.p[order(-body.p$P),]
body.p

# reduced model based on P-value
body.lm.R <- lm(Weight~Height+Shoulder+Chest+Waist+Hip+Thigh+Forearm+Knee+Calf+Age,
                data = body.mod)

summary(body.lm.R)

# reduced model based on AIC
body.AIC <- step(lm(Weight~1, data = body.mod),
                 scope=~Height+Shoulder+Chest+Waist+
                   Abdo+Hip+Thigh+Bicep+
                   Forearm+Knee+Calf+Ankle+
                   Wrist+Age, 
                 direction="forward")

summary(body.AIC)

# d.4
# Model misspecification
body.fit <- fitted(body.lm.R) # gives the fitted values used in calculating the residuals
body.rs <- rstandard(body.lm.R) # gives standardized residuals, that is, residuals divided by the variance

png("residuals.png")
plot(body.rs~body.fit, main = "Standardized residuals vs. Fitted values")
lines(lowess(body.rs~body.fit))
abline(h = c(-3,3))
abline(h = 0, col = "red", lty = 2)
dev.off()

names.R <- c("Height","Shoulder","Chest","Waist","Hip","Thigh","Forearm","Knee","Calf","Age")
data.R <- body.mod[names.R]
for (l in 1:length(names.R)){
  png(paste(paste("residuals",l, sep = "_"),"png", sep = "."))
  plot(body.rs~data.R[,l], xlab = paste(names.R[l]), ylab = "Standardized residuals", main = "")
  abline(h = c(-3,3))
  abline(h = 0, col = "red", lty = 2)
  dev.off()
  }

# Normality of errors
shapiro.test(body.rs)

png("NormRes.png")
hist(body.rs, prob=TRUE)
dev.off()

png("QQplot.png")
plot(body.lm.R, which = 2)
dev.off()

# Influential points
png("CooksD.png")
plot(body.lm.R, which = 5)
dev.off()

# d.5 
r2.R <- data.frame(summary(body.lm.R)$r.squared, summary(body.lm.R)$adj.r.squared)
names(r2.R) <- c("R squared", "Adj.R squared")
round(r2.R, digits = 3)

# d.6
# install.packages("leaps")
library(leaps)

leap.Cp <- leaps(body.mod[,2:15], body.mod[,1], method="Cp")
leap.Ar2 <- leaps(body.mod[,2:15], body.mod[,1], method="adjr2")
leap.r2 <- leaps(body.mod[,2:15], body.mod[,1], method="r2")

# Example of model selection with Cp, adj R^2 and R^2
# Look at the variables selected for each model size
colnames( body.mod[ , leap.Cp$which[which.min(leap.Cp$Cp), ], ]) #Min Cp
colnames( body.mod[ , leap.Ar2$which[which.max(leap.Ar2$adjr2), ] ]) # Max adj R^2
colnames( body.mod[ , leap.r2$which[which.max(leap.r2$r2), ] ]) #Max R^2, NOTE all variables have been selected
co
combine.F <- cbind(leap.Cp$which,leap.Cp$size, leap.Cp$Cp, leap.Ar2$adjr2, leap.r2$r2)
dimnames(combine.F) <- list(1:131,c("Height","Shoulder","Chest","Waist","Abdo","Hip",
                                    "Thigh","Bicep","Forearm","Knee","Calf","Ankle","Wrist","Age", "size", "Cp", "Adj. R2", "R2"))
round(combine.F, digits=3)

png("Cp.png")
plot(leap.Cp$Cp~leap.Cp$size, xlim = c(0,15), ylim=c(0,15))
abline(a=0, b=1)
identify(leap.Cp$Cp~leap.Cp$size, labels=row.names(combine.F))
dev.off()

#round(combine.F[c(91,103,104,115,116),], digits = 3)

# d.7
install.packages("scatterplot3d")
library(scatterplot3d)
fit <- lm(Weight~Height+Waist, data = body.mod)
png("Scatter3D.png")
s3d <- scatterplot3d(body.mod$Height,body.mod$Waist,body.mod$Weight, main="3D Scatterplot")
s3d$plane3d(fit, lty.box = "solid")
dev.off()

## e ##

body.pred <- predict(body.lm.R, body.new, interval = "prediction", se.fit = TRUE)
round(body.pred$fit, digits = 2)
round(as.vector(body.pred$se.fit), digits = 2)

## f ##

# f.1
# Full model - all interactions
body.lm.F <- lm(Weight~Height+Shoulder+Chest+Waist+Abdo+Hip+Thigh+Bicep+Forearm+Knee+Calf+Ankle+Wrist+Age+X1+
                  I(X1*Height)+I(X1*Shoulder)+I(X1*Chest)+I(X1*Waist)+I(X1*Abdo)+I(X1*Hip)+I(X1*Thigh)+I(X1*Bicep)+
                  I(X1*Forearm)+I(X1*Knee)+I(X1*Calf)+I(X1*Ankle)+I(X1*Wrist)+I(X1*Age),data = body.mod)

summary(body.lm.F)

# sorting P-values
# Alternatively, use AIC
lab <- labels(summary(body.lm.F)$coefficients)
body.p <- data.frame(lab[[1]],summary(body.lm.F)$coefficients[,4])
names(body.p) <- c("lab","P")

body.p <- body.p[order(-body.p$P),]
body.p

# Reduced model based on p-value
body.lm.R2 <- lm(Weight~Height+Shoulder+Chest+Waist+Hip+Thigh+Forearm+Knee+Calf+Age+X1+
                   I(X1*Height)+I(X1*Shoulder), data = body.mod)

summary(body.lm.R2)

# f.2
r2.R2 <- data.frame(summary(body.lm.R2)$r.squared, summary(body.lm.R2)$adj.r.squared)
names(r2.R2) <- c("R squared", "Adj.R squared")
r2.com <- rbind(r2.R, r2.R2)
round(r2.com, digits = 3)

# f.3
lm.temp <- lm(Weight~Height+X1+I(Height*X1), data = body.mod)
reg.RM <- as.vector(c(summary(lm.temp)$coefficients[1,1], summary(lm.temp)$coefficients[2,1]))
reg.RF <- as.vector(c(summary(lm.temp)$coefficients[1,1] + summary(lm.temp)$coefficients[3,1],
                      summary(lm.temp)$coefficients[2,1] + summary(lm.temp)$coefficients[4,1]))

png("ScatterHeight.png")
plot(body.mod$Weight~body.mod$Height)
abline(coef = reg.RF, col = "red")
abline(coef = reg.RM, col = "blue")
dev.off()

## g ##

body.pred2 <- predict(body.lm.R2, body.new, interval = "prediction", se.fit = TRUE)

# Compute the prediction errors
body.pred.error1 <- as.vector(body.new[, 1] - body.pred$fit[, 1])
body.pred.error2 <- as.vector(body.new[, 1] - body.pred2$fit[, 1])
# Compute the standard deviations
sd(body.pred.error1)
sd(body.pred.error2)
# Model 2 performs better, although not by much
plot(body.pred.error1, body.pred.error2)
curve(1*x, add = TRUE, lwd = 3)
