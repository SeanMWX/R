
# --- Chapter 6: Correlation ---

# - Starting example: Temperature data -
# Example temperature
temperature <- read.table(file=file.choose(), header=TRUE)
names(temperature)
head(temperature[15:19])

annual <- temperature$annual
lat <- temperature$Latitude
long <- temperature$Longitude
combine <- data.frame(annual, lat, long)
pairs(combine)



# - Pearson Correlation coefficient -
## - Correlation coefficients in R -
cor(combine)
cor.test(annual, lat)
cor.test(annual, long)



# - Spearman correlation coefficient -
# Check the normality
shapiro.test(annual)
# Compute Spearman correlations
cor(combine, method = "spearman")
cor.test(annual, lat, method = "spearman")
cor.test(annual, long, method = "spearman")


