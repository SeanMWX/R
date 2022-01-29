###########################
#### EXERCISE SESSION 2 ###
###########################

# EXERCISE 1
# a define the random variable: X = "result of a dice throw", give possible outcomes: 1 ... 6
# c
dice <- seq(1:6)
dice.p <- as.vector(rep(1/6,6))
dice.c <- as.vector(rep(1/6,6))
for (i in 2:length(dice.p)){
  dice.c[i] <- dice.c[i-1] + dice.p[i]
  }

barplot(dice.p, space = 0.2, main="Probability Density Function",
        ylim = c(0,0.2), names.arg=c("1","2","3","4","5","6"), 
        axes = FALSE, col = "red")
axis(2, at = c(0,1/6), labels = c("0", "1/6"), lty = 1)

plot(dice, dice.c, type="n", 
     main="Cumulative Distribution Function", xlab = "", ylab = "Fx", axes = FALSE,)
lines(dice, dice.c, type="s")
axis(2, at = c(0,dice.c), 
     labels = c("0", "1/6", "1/3", "1/2", "2/3", "5/6", "1"), lty = 1)
axis(1, at = dice, lty = 1)

# EXERCISE 2

# d
round(pbinom(6,10,0.5), 2)

# e
1-pbinom(6,10,0.5) # or equivalent:
pbinom(6,10,0.5, lower.tail=FALSE)

# f
qbinom(0.5,10,0.5)

# g
qbinom(0.75,10,0.5)

# EXERCISE 3

# c
ppois(0,2)

# d
1-ppois(0,2) # or equivalent
ppois(0,2,lower.tail=FALSE)

# e
(ppois(0,2))^5 # or equivalent
ppois(0,10)

# f
range <- seq(0,16,1)
density <- dpois(range,4)
barplot(density, ylim=c(0,0.2))

# EXERCISE 4

s.d <- sqrt(500)

# d
pnorm(100,85, s.d) # or equivalent by standardizing
z <- (100-85)/s.d
pnorm(z, 0, 1)

# e
pnorm(80, 85, s.d) # or equivalent by standardizing
z <- (80-85)/s.d
pnorm(z, 0, 1)

# f
1-pnorm(60, 85, s.d) # or equivalent
pnorm(60, 85, s.d, lower.tail=FALSE) # or equivalent by standardizing
z <- (60-85)/s.d
pnorm(z, 0, 1, lower.tail=FALSE)

# g
x.60 <- 60
lim <- qnorm(0.975, 85, s.d)
lim.u <- 170
lim.l <- 0
x <- seq(lim.l,lim.u,length=1000)
i.60 <- x >= x.60 & x <= lim.u
hx <- dnorm(x, mean = 85, sd = s.d)

plot(x, hx, type="n", xlab="", ylab="",
     main=expression(mu ~ "= 85 kg," ~ sigma ~ "= 22.36 kg"),
     axes=FALSE)
lines(x, hx)
polygon(c(x.60,x[i.60],lim.u), c(0,hx[i.60],0), col="green")
result <- "P(X > 60)"
mtext(result,3)
axis(1, at=c(lim.l, x.60, 85, lim.u), pos=0)

# h
qnorm(0.975, 85, s.d)

# EXERCISE 5

# a
1-pt(1, 15) # or equivalent
pt(1, 15, lower.tail=FALSE)

# b
qt(0.95, 15) # or equivalent
qt(0.05, 15, lower.tail = FALSE)

# c
x.95 <- round(qt(0.95, 15), 2)
lim.u <- 4 #round(qt(0.999, 15), 2)
lim.l <- -lim.u
x <- seq(lim.l,lim.u,length=1000)
i.95 <- x >= x.95 & x <= lim.u
hx <- dt(x, 15)

plot(x, hx, type="n", xlab="", ylab="",
     main="Student t with 15 DF",
     axes=FALSE)
lines(x, hx)
polygon(c(x.95,x[i.95],lim.u), c(0,hx[i.95],0), col="green")
result <- "P(T > t) = 0.05"
mtext(result,3)
axis(1, at=c(lim.l, 0, x.95, lim.u), pos=0)

# EXERCISE 6

# a
qchisq(0.95, 10) # or equivalent
qchisq(0.05, 10, lower.tail=FALSE)

# EXERCISE 7

# a
P10 <- pf(10, 4, 9)
P5 <- pf(5, 4, 9)
P10 - P5

# b
qf(0.95, 4, 9) # or equivalent
qf(0.05, 4, 9, lower.tail=FALSE)

# INSTALLING "BSDA" and "PropCIs" PACKAGES
install.packages("BSDA")
install.packages("PropCIs")
library(BSDA)
library(PropCIs)

# EXERCISE 8

# a
sigma <- 0.3
n <- 10
xmean <- 41.924
conf <- 0.95
alpha <- 1-conf

lcl <- xmean-qnorm(1-alpha/2)*sigma/sqrt(n)
ucl <- xmean+qnorm(1-alpha/2)*sigma/sqrt(n)
CI <- list(lcl=lcl, ucl=ucl)
CI

# b
zsum.test(41.924, 0.3, 10, conf.level=0.95)

# EXERSISE 9

# a
zsum.test(750, 30, 20, conf.level=0.95)

# EXERCISE 10
blood.df <- read.table(file=file.choose(), header=TRUE, sep=",")

# a
z.test(blood.df$age, sigma.x=5, conf.level=0.90)

# b
t.test(blood.df$prolactn, conf.level=0.95)

# c
tot.obs <- nrow(blood.df)
subset.df <- subset(blood.df, age > 50 & age < 60)
sub.obs <- nrow(subset.df)
prop <- sub.obs/tot.obs
prop

# d
scoreci(sub.obs, tot.obs, conf.level=0.95) 
prop.test(sub.obs, tot.obs, conf.level=0.95) # or equivalent from "stats" package

# e
subset.df <- subset(blood.df, age < 50)
nrow(subset.df)

# f
# first check normality because sample is small
shapiro.test(subset.df$testost)
# now calculate confidence limits
t.test(subset.df$testost, conf.level=0.99)
