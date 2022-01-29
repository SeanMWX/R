
# --- Chapter 8: Selection of variables ---

# - All possible regressions - 

## - Example -
# Import insurance.txt
insurance <- read.table(file=file.choose(), header=TRUE)
names(insurance)
insurance[1:10, c(2, 4, 5, 8, 9)]

loss <- insurance$LOSS
age <- insurance$AGE
exper <- insurance$EXPER
miles <- insurance$MILES
towork <- insurance$TOWORK

install.packages("leaps")
library(leaps)
?leaps

# Search for best subsets of explanatory variables
leap <- leaps(x = cbind(age, exper, miles, towork), y = loss, method = c("r2"))
combine <- cbind(leap$which,leap$size, leap$r2)
dimnames(combine) <- list(1:15,c("age","exper","miles","towork","size","r2"))
round(combine, digits=3)


## - Mallows' Cp - 

### - Theory of Mallows' Cp - 
leap.cp <- leaps(x = cbind(age, exper, miles, towork), y=loss, method="Cp") # Note that method is set to "Cp"
combine.cp <- cbind(leap.cp$which,leap.cp$size, leap.cp$Cp)
dimnames(combine.cp) <- list(1:15,c("age","exper","miles","towork","size","cp"))
round(combine.cp, digits=3)

plot(leap.cp$size, leap.cp$Cp, ylim=c(1,5))
abline(a=0, b=1)



# - Model selection criteria - 
## - Partial F-test - 
# Consider the following two nested models:
reg.lm1 <- lm(loss ~ age + towork)
reg.lm2 <- lm(loss ~ age + towork + miles + exper)
# Compare these models
anova(reg.lm1, reg.lm2)

## - Akaike information criterion - 
list(AIC_model1 = AIC(reg.lm1), AIC_model2 = AIC(reg.lm2))



# - Selection methods -
## - In R -
# Forward selection
slm.foward <- step(lm(LOSS ~ 1, data = insurance), scope = ~ AGE + EXPER + MILES + TOWORK, direction = "forward", data = insurance)

# Backward selection 
slm.backward <- step(lm(LOSS ~ AGE + EXPER + MILES + TOWORK, data = insurance), direction = "backward")

# Bidirectional selection 
slm.both <- step(lm(LOSS ~ AGE + EXPER + MILES + TOWORK, data = insurance), direction = "both")



# - Solution to cars exercise -
# Import cars1.txt 
cars <- read.table(file=file.choose(), header=TRUE)
names(cars)

## - Question (a): Use of all-possible-subset selection - 
attach(cars)
# Using R²
leap <- leaps(x = cbind(Horsepower, Length, Luggage, Uturn, Wheelbase, Width), y=MidrangePrice, method=c("r2"), nbest=3)
combine <- cbind(leap$which, leap$size, leap$r2)
n <- length(leap$size)
dimnames(combine) <- list(1:n, c("horsep", "length", "Lug", "Uturn", "WB", "Width", "size", "r2"))
round(combine, digits=3)

# Using Mallows' Cp
leap.cp <- leaps(x = cbind(Horsepower, Length, Luggage, Uturn, Wheelbase, Width), y=MidrangePrice, nbest=3)
combine.cp <- cbind(leap.cp$which, leap.cp$size, leap.cp$Cp)
n <- length(leap.cp$size)
dimnames(combine.cp) <- list(1:n, c("horsep", "length", "Lug", "Uturn", "WB", "Width", "size", "Cp"))
round(combine.cp, digits=3)
plot(leap.cp$size, leap.cp$Cp, ylim=c(1,7))
abline(a=0, b=1)
detach(cars)


## - Question (b): Use one of the automatic-selection techniques - 
# Forward selection 
slm.foward <- step(lm(MidrangePrice ~ 1, data=cars), scope = ~ Horsepower + Length + Luggage + Uturn + Wheelbase + Width, direction = "forward", data = cars)
# Backward selection 
reg.lm1 <- lm(MidrangePrice ~ Horsepower + Length + Luggage + Uturn + Wheelbase + Width, data = cars)
slm.backward <- step(reg.lm1, direction="backward")
# Bidirectional selection 
reg.lm1 <- lm(MidrangePrice ~ Horsepower + Length + Luggage + Uturn + Wheelbase + Width, data = cars)
slm.both <- step(reg.lm1, direction="both")





