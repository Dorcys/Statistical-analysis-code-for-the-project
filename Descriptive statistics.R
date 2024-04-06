#################################################################################################
###### DESCRIPTIVE STATISTICS

# Measures of Central tendency of Volume
mean(Trees$Volume)
var(Trees$Volume)
sd(Trees$Volume)
median(Trees$Volume)
quantile(Trees$Volume)
summary(Trees$Volume)

# Subsets of North and South locations 
north <- subset(Trees, Site %in% c("N1", "N2", "N3"))
south <- subset(Trees, Site %in% c("S1", "S2", "S3"))

View(north)
View(south)

summary(north$Volume)
summary(south$Volume)

# Measures of Central tendency of Volume 
# North locations
mean_north <- mean(north$Volume)
median_north <- median(north$Volume)
variance_north <- var(north$Volume)
stdev_north <- sd(north$Volume)

# South locations
mean_south <- mean(south$Volume)
median_south <- median(south$Volume)
variance_south <- var(south$Volume)
stdev_south <- sd(south$Volume)

# Filtering species in north and south 
table(south$Specie)
table(north$Specie)

#  Acer platanoides
AP_N <- subset(north, Specie == "Acer platanoides")
AP_S <- subset(south, Specie == "Acer platanoides")

# Fraxinus excelsior
FE_N <- subset(north, Specie == "Fraxinus excelsior")
FE_S <- subset(south, Specie == "Fraxinus excelsior")

# Quercus robur
QR_N <- subset(north, Specie == "Quercus robur")
QR_S <- subset(south, Specie == "Quercus robur")

## Measures of Central tendency
# Acer platanoides
# North
AP_mean_north <- mean(AP_N$Volume)
AP_median_north <- median(AP_N$Volume)
AP_variance_north <- var(AP_N$Volume)
AP_stdev_north <- sd(AP_N$Volume)

# South
AP_mean_south <- mean(AP_S$Volume)
AP_median_south <- median(AP_S$Volume)
AP_variance_south <- var(AP_S$Volume)
AP_stdev_south <- sd(AP_S$Volume)

## Fraxinus excelsior
# North 
FE_mean_north <- mean(FE_N$Volume)
FE_median_north <- median(FE_N$Volume)
FE_variance_north <- var(FE_N$Volume)
FE_stdev_north <- sd(FE_N$Volume)

# South
FE_mean_south <- mean(FE_S$Volume)
FE_median_south <- median(FE_S$Volume)
FE_variance_south <- var(FE_S$Volume)
FE_stdev_south <- sd(FE_S$Volume)

## Quercus robur
# North 
QR_mean_north <- mean(QR_N$Volume)
QR_median_north <- median(QR_N$Volume)
QR_variance_north <- var(QR_N$Volume)
QR_stdev_north <- sd(QR_N$Volume)

# South
QR_mean_south <- mean(QR_S$Volume)
QR_median_south <- median(QR_S$Volume)
QR_variance_south <- var(QR_S$Volume)
QR_stdev_south <- sd(QR_S$Volume)
