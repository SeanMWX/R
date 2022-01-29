
# --- Chapter 3: Important distributions ---

# - Discrete distributions -
## - Introduction - 
## - Binomial distribution -
### - Examples -
# Example Albino
k <- c(0:3)
densbin <- dbinom(k, 3, 0.25)
names(densbin) <- k
densbin

# Compute cumulative density
cumdens <- pbinom(k, 3, 0.25)
names(cumdens) <- k
cumdens

# Example Golub
dbinom(47, 72, 0.68)


### - Use of the Binomial distribution in R -
y <- c(0:10)
dens <- dbinom(y, size = 10, prob = 0.2)
barplot(dens, xlab = "y", ylab = "probability")


## - The Poisson distribution -
### - Example - 
# Example cars per minute 
dpois(17, lambda=12)

cumprob <- ppois(16, lambda = 12)
1-cumprob


# - Continuous distributions - 
## - The normal distribution -
### - Examples -
#### - Example gene expression data -
pnorm(1.4, 1.9, 0.5)
1-pnorm(2.4, 1.9, 0.5)
pnorm(2.4, 1.9, 0.5) - pnorm(1.4, 1.9, 0.5)

#### - Example standard normal distribution N(0,1)
diff(pnorm(c(-3, 3), mean = 0, sd = 1))


## - The T distribution - 
pt(2, 10) - pt(-2, 10)


## - The F distribution -
1-pf(2, 32, 9)


## - The chi-square distribution
pchisq(8,5)


