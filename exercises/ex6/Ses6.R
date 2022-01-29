###########################
#### EXERCISE SESSION 6 ###
###########################

senic.df <- read.table(file=file.choose(), header=TRUE, sep="\t")
senic.df$reg <- as.factor(senic.df$reg)

## a
X1 <- as.vector(rep(0,nrow(senic.df)))
X2 <- X1
X3 <- X1

X1[which(senic.df$reg == 1)] <- 1
X2[which(senic.df$reg == 2)] <- 1
X3[which(senic.df$reg == 3)] <- 1

senic.df <- data.frame(senic.df, X1, X2, X3)

senic.df[,13:15]

## b
senic.lm <- lm(risk~X1+X2+X3, data = senic.df)
summary(senic.lm)

## c
library(BSDA)
senic.lm$coefficients
s.mean <- c(0,0,0,0)
s.mean[1] <- senic.lm$coefficients[1]+senic.lm$coefficients[2]
s.mean[2] <- senic.lm$coefficients[1]+senic.lm$coefficients[3]
s.mean[3] <- senic.lm$coefficients[1]+senic.lm$coefficients[4]
s.mean[4] <- senic.lm$coefficients[1]

s.sd <- as.matrix(by(senic.df$risk, senic.df$reg, sd))

s.n <- c(28, 32, 37, 16)

senic.conf <- matrix(data = 0, nrow = 4, ncol = 4)
senic.conf[,1] <- c(1, 2, 3, 4)
senic.conf[,2] <- s.mean
for(j in 1:4){
  y <- tsum.test(s.mean[j], s.x = s.sd[j], n.x = s.n[j], conf.level=0.95, var.equal=TRUE)
  senic.conf[j,3] <- y[[4]][1]
  senic.conf[j,4] <- y[[4]][2]
  if(j == 4){print(senic.conf)}
}

dotchart(senic.conf[,2], labels = senic.conf[,1], col = "red", xlim = c(min(senic.conf[,3:4]), max(senic.conf[,3:4])))
for(i in 1:4){
  lines(x = c(senic.conf[i,3], senic.conf[i,4]), y = c(senic.conf[i,1], senic.conf[i,1]))
}

## d
senic.aov <- aov(risk~reg, data = senic.df)
summary(senic.aov)

## e
senic.tuk <- TukeyHSD(senic.aov, conf.level = 0.95)
plot(senic.tuk)

## f
# Constant within-group variance
#install.packages("car")
library(car)
leveneTest(risk~reg, data = senic.df)

# Normally distributed residuals
# shapiro.test(senic.lm$residuals)
# hist(senic.lm$residuals)
by(senic.df$risk, INDICES = senic.df$reg, FUN = shapiro.test)
# Normality seems tenable

# Influential observations
plot(senic.lm, which = 5)

