
# --- Chapter 11: Introduction to Poisson regression ---

# - Introduction -
lam_1.5 <-rpois(1000, 1.5)
lam_5 <- rpois(1000, 5)
par(mfrow = c(1,2))
hist(lam_1.5, breaks = 15, main = "Histogram of Poisson lambda = 1.5", xlab="")
hist(lam_5, breaks = 15, main = "Histogram of Poisson lambda = 5", xlab="")

# Example horseshoe crabs
# Import crab.txt
crab <- read.table(file = file.choose(), header = TRUE)
names(crab)
head(crab, n = 6)

hist(crab$Sa, breaks = 15)

# Descriptive statistics
list(mean = mean(crab$Sa), var = var(crab$Sa))


# - The Poisson regression model -
## - Illustration in R -
# Model 0: only intercept
model.0 <- glm(Sa ~ 1, family = poisson(link = log), data = crab)
summary(model.0)

# Model 1: Single explanatory variable
model.1 <- glm(Sa ~ W, family = poisson(link = log), data = crab)
summary(model.1)

# Visualize the Poisson regression model
pred <- fitted(model.1)
plot(crab$W, crab$Sa, col = 4) # Scatter plot of Sa vs W
points(crab$W, pred, col = 3)

# Is there a good fit? 
anova(model.1)

# Compare model 1 with model 0
anova(model.0, model.1, test = "Chisq")

