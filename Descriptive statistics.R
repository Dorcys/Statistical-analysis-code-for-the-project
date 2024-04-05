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

# Comparison between north and south species 
table(south$Specie)
table(north$Specie)

###  Acer platanoides
# Filtering north and south subset 
AP_N <- subset(north, Specie == "Acer platanoides")
AP_S <- subset(south, Specie == "Acer platanoides")
View(AP_N)
View(AP_S)

# Calculate the mean volume 
Volume_AP_N <- mean(AP_N$Volume)
Volume_AP_S <- mean(AP_S$Volume)
print(Volume_AP_N)
print(Volume_AP_S)

### Fraxinus excelsior
# Filtering north and south subset 
FE_N <- subset(north, Specie == "Fraxinus excelsior")
FE_S <- subset(south, Specie == "Fraxinus excelsior")

# Calculating the mean volume
Volume_FE_N <- mean(FE_N$Volume)
Volume_FE_S <- mean(FE_S$Volume)
print(Volume_FE_N)
print(Volume_FE_S)

### Quercus robur
# Filtering north and south subset 
QR_N <- subset(north, Specie == "Quercus robur")
QR_S <- subset(south, Specie == "Quercus robur")

# Calculating the mean volume
Volume_QR_N <- mean(QR_N$Volume)
Volume_QR_S <- mean(QR_S$Volume)
print(Volume_QR_N)
print(Volume_QR_S)


