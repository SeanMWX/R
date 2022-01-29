
# --- Chapter 7: Linear regression ---

# - Simple linear regression -
## - Fitting a line -
### - Illustrative example -
# Example temperature
# Import temp_warm.txt
temperature <- read.table(file=file.choose(), header=TRUE)
names(temperature)

annual <- temperature$annual
lat <- temperature$Latitude
long <- temperature$Longitude

## - Simple linear regression in R -
res.lm1 <- lm(annual ~ lat)
summary(res.lm1)


# - Hypothesis test for alpha and beta -
## - General -
## - In R -
# Estimated regression model
res.lm1 <- lm(annual ~ lat)
summary(res.lm1)
# Visualization
plot(annual ~ lat)
abline(res.lm1)

## - The ANOVA table - 
anova(res.lm1)



# - Model diagnostics -

## - Checking for linearity - 

### - Examples -
# Example temperature
res.lm1 <- lm(annual ~ lat)
fit <- fitted(res.lm1) 
rs <- rstandard(res.lm1)
# Plot standard residuals
par(mfrow = c(1,2))
plot(rs ~ lat)
plot(rs ~ fit)
par(mfrow = c(1,1))


# Example Vapor pressure of water
temp <- c(10, 20, 30, 40, 50, 60)
vapor <- c(9.2, 17.5, 31.8, 55.3, 92.5, 149.4)
plot(vapor ~ temp, col = 6, pch = 17)
abline(lm(vapor ~ temp), col=3, lwd=3)
# Regression fit
res.lm <- lm(vapor ~ temp)
summary(res.lm)
# Residual plots
fit <- fitted(res.lm)
rs <- rstandard(res.lm)
par(mfrow = c(1,2))
plot(rs ~ temp)
plot(rs ~ fit)


# Quadratic term added
res.lm.q <- lm(vapor ~ temp + I(temp^2))
summary(res.lm.q)
# Plot standard residuals
fit <- fitted(res.lm.q)
rs <- rstandard(res.lm.q)
par(mfrow = c(1,2))
plot(rs ~ temp)
plot(rs ~ fit)
par(mfrow = c(1,1))



## - Checking normality of the residuals - 
### - Introduction - 
### - Examples - 
# Example Temperature 
res.lm1 <- lm(annual ~ lat)
rs <- rstandard(res.lm1)
shapiro.test(rs)
hist(rs, prob = TRUE)


# Example bacteria deaths
time <- 1:15
bact <- c(335, 211, 197, 166, 142, 106, 104, 60, 56, 38, 36, 32, 21, 19, 15)
# Regression model
res.lm <- lm(bact ~ time)
summary(res.lm)

plot(bact ~ time)
abline(res.lm)
rstandard(res.lm)
# Check normality of the residuals
shapiro.test(rstandard(res.lm))
hist(rstandard(res.lm))

# When normality of the residuals does not hold
# Transformation of the response 
ln_bact <- log(bact)
reslog.lm <- lm(ln_bact ~ time)
summary(reslog.lm)
shapiro.test(rstandard(reslog.lm))



## - Influential points - 

### - Examples -
# Example temperature
res.lm1 <- lm(annual ~ lat)
plot(cooks.distance(res.lm1))


# Example Surface tension of water-based coatings
amount <- c(0.05,0.1,0.15,0.2,0.25,0.5)
surface <-c(70,60,49,41,30,46)
ex2 <- data.frame(amount, surface)
res.lm3 <- lm(surface ~ amount)
summary(res.lm3)
# Plotting data and estimated regression line
plot(surface ~ amount)
abline(res.lm3)
# Plotting Cook's distance
plot(cooks.distance(res.lm3))
# Selecting the data points of which the Cook's distance stand out from the rest
ex2[cooks.distance(res.lm3)>8,]
# Delete last point and redo the analysis
amount_new <- amount[-6]
surface_new <- surface[-6]
res.lm3_new <- lm(surface_new ~ amount_new)
summary(res.lm3_new)
plot(surface_new ~ amount_new)
abline(res.lm3_new)
plot(cooks.distance(res.lm3_new))



# - Prediction -

## - In R -
res.lm1 <- lm(annual ~ lat)
city <- c("Belgium", "Athens", "Ankara", "Bangkok")
# Confidence interval
res.pred_CI <- predict(res.lm1, list(lat= c(50, 37, 39, 13)), interval = "confidence")
pred_CI <- data.frame(city, res.pred_CI)
pred_CI
# Prediction interval
res.pred_PI <- predict(res.lm1, list(lat= c(50, 37, 39, 13)), interval = "prediction")
pred_PI <- data.frame(city, res.pred_PI)
pred_PI


# - Overview: Global structure for regression analysis - 

# - Multiple linear regression - 
## - Multiple regression model in R - 
res.lm5 <- lm(annual ~ lat + long)
summary(res.lm5)

## - Checking assumptions in R -
### - Checking for linearity -
res.lm5 <- lm(annual ~ lat + long)
fit <- fitted(res.lm5) 
rs <- rstandard(res.lm5) 
par(mfrow = c(2, 2))
plot(rs ~ fit)
plot(rs ~ lat)
plot(rs ~ long)
par(mfrow = c(1, 1))


### - Check normality of (standardised) residuals -
shapiro.test(rs)


### - Check for influential poitns - 
plot(cooks.distance(res.lm5))
temperature[which(cooks.distance(res.lm5) > 0.3),]
# Delete point number 2 and redo the analysis
annual_new <- annual[-2]
lat_new <- lat[-2]
long_new <- long[-2]
res.lm6 <- lm(annual_new ~ lat_new + long_new)
summary(res.lm6)


## - Predictions -
new <- data.frame(list(lat = c(52, 52 ,57, 41), long = c(13, 1, 2, 2)))
res.pred5 <- predict(res.lm5, new, interval = "confidence")
city1 <- c("Berlin","Birmingham", "Aberdeen", "Barcelona")
pred5 <- data.frame(city1, res.pred5)
pred5



# - Polynomial model -
## - A polynomial model in R -
res.lm7 <- lm(annual ~ lat + I(lat^2))
summary(res.lm7)


# - Interaction models -
## - Interaction models in R -
res.lm8 <- lm(annual ~ lat + long + I(lat*long))
summary(res.lm8)


# - Qualitative independent variable - 
## - Introduction -
# Example Wine
# Import wine.txt
wine <- read.table(file=file.choose(), header=TRUE)
head(wine)
quality <- wine$quality
flavor <- wine$flavor
# Estimate regression model
res.lm <- lm(quality ~ flavor)
summary(res.lm)
# Indicator variable
indic <- wine$indicator
# Visualize the data 
library(ggplot2)
pl1 <- qplot(flavor, quality, colour=factor(indic), data= wine, size=3)
pl1


## - Regression model with indicator variable (but without interaction term) -
res.lm1 <- lm(quality ~ flavor + indic)
summary(res.lm1)

fit <- fitted(res.lm1)
plot(flavor, fit, lty = 2, col = 4)

## - Regression model with indicator variable and interaction term -
res.lm2 <- lm(quality ~ flavor + indic + I(flavor * indic))
summary(res.lm2)


