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

# Filter north and south subset for Acer platanoides
AP_N <- subset(north, Specie == "Acer platanoides")
AP_S <- subset(south, Specie == "Acer platanoides")
View(AP_N)
View(AP_S)

# Calculate the mean volume for Acer platanoides
Volume_AP_N <- mean(AP_N$Volume)
Volume_AP_S <- mean(AP_S$Volume)
print(Volume_AP_N)
print(Volume_AP_S)

