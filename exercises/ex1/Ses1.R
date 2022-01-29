###########################
#### EXERCISE SESSION 1 ###
###########################

# EXERCISE 0

# importing BLOOD.DAT dataset
blood.df <- read.table(file=file.choose(), header=TRUE, sep=",")

# EXERCISE 1

# a
beta.df <- read.table(file=file.choose(), header=TRUE, sep="\t")

# EXERCISE 2

# a
names(beta.df)

# EXERCISE 3

# a
chick.df <- read.table(file=file.choose(), sep="\t")

# b
names(chick.df)

# c
summary(chick.df$weight)
var(chick.df$weight)
sd(chick.df$weight)

# d
View(chick.df)
summary(chick.df$chicken)
# This is nonsense, as these numbers are simply labels

# e
names(chick.df)[1] <- "No."
names(chick.df)

# f
by(chick.df$weight, chick.df$feed, summary)
by(chick.df$weight, chick.df$feed, var)
by(chick.df$weight, chick.df$feed, sd)

# g
table(chick.df$feed)



# EXERCISE 4

# a
monica.df <- read.table(file=file.choose(), header=TRUE, sep=";")

# b
by(monica.df$age, monica.df$sex, summary)
by(monica.df$age, monica.df$sex, var)
by(monica.df$age, monica.df$sex, sd)

# c
par(mfrow = c(1,1 ))
boxplot(monica.df$age)

# d
boxplot(age~sex, data = monica.df)

# f
monica.den <- by(monica.df$age, monica.df$sex, density) # calculate densities
par(mfrow=c(1,2)) # put two graphs next to each other in one row, two columns
plot(monica.den[[1]], main="Females")
plot(monica.den[[2]], main="Males")
# NOTE: Females is the first density:

# alternative with histogram and lines
par(mfrow=c(1,2))
hist(monica.df$age[which(monica.df$sex == 'm')], probability = TRUE, main = "Males", breaks = 36, ylim = c(0,0.08), xlab = "Age")
lines(density(monica.df$age[which(monica.df$sex == 'm')]))
hist(monica.df$age[which(monica.df$sex == 'f')], probability = TRUE, main = "Females", breaks = 36, ylim = c(0,0.08), xlab = "Age")
lines(density(monica.df$age[which(monica.df$sex == 'f')]))
