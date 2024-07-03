###This is final code for this project###
install.packages("dplyr")
install.packages("readr")
library(readr)
library(dplyr)
#Fix problem  with "," insted of "."
locale_dec_comma <- locale(decimal_mark = ",")
Trees <- read_csv("Trees.csv", locale = locale_dec_comma)
View(Trees)

#Translating of species names from Russian code to Latin names
Trees$Specie[Trees$Specie == "ЛПМ"] <- "Tilia cordata"
Trees$Specie[Trees$Specie == "Аб"] <- "Prunus armeniaca"
Trees$Specie[Trees$Specie == "Д"] <- "Quercus robur"
Trees$Specie[Trees$Specie == "Яс"] <- "Fraxinus excelsior"
Trees$Specie[Trees$Specie == "ЯС"] <- "Fraxinus excelsior"
Trees$Specie[Trees$Specie == "ЯСО"] <- "Fraxinus excelsior"
Trees$Specie[Trees$Specie == "ЯБЛ"] <- "Malus sylvestris"
Trees$Specie[Trees$Specie == "Рб"] <- "Robinia pseudoacacia"
Trees$Specie[Trees$Specie == "Р"] <- "Robinia pseudoacacia"
Trees$Specie[Trees$Specie == "КЛО"] <- "Acer platanoides"
Trees$Specie[Trees$Specie == "КлО"] <- "Acer platanoides"
Trees$Specie[Trees$Specie == "ВЯЗ"] <- "Ulmus glabra"
Trees$Specie[Trees$Specie == "ВЗ"] <- "Ulmus glabra"
#Summary of species 
table(Trees$Specie)

#Create new column for calculations 
Trees$Species_index <- NA
Trees$Species_index[Trees$Specie == "Ulmus glabra"] <- 0.569
Trees$Species_index[Trees$Specie == "Tilia cordata"] <- 0.472
Trees$Species_index[Trees$Specie == "Prunus armeniaca"] <- 0.550
Trees$Species_index[Trees$Specie == "Quercus robur"] <- 0.494
Trees$Species_index[Trees$Specie == "Fraxinus excelsior"] <- 0.468
Trees$Species_index[Trees$Specie == "Malus sylvestris"] <- 0.515
Trees$Species_index[Trees$Specie == "Robinia pseudoacacia"] <- 0.508
Trees$Species_index[Trees$Specie == "Acer platanoides"] <- 0.470
#Using filters for adding indexes for dead trees
Trees$Species_index[Trees$`Health Status`== 0 & Trees$Specie =="Tilia cordata"] <- 0.463
Trees$Species_index[Trees$`Health Status`== 0 & Trees$Specie =="Prunus armeniaca"] <- 0.569
Trees$Species_index[Trees$`Health Status`== 0 & Trees$Specie =="Quercus robur"] <- 0.505
Trees$Species_index[Trees$`Health Status`== 0 & Trees$Specie =="Fraxinus excelsior"] <- 0.468   
Trees$Species_index[Trees$`Health Status`== 0 & Trees$Specie =="Malus sylvestris"] <- 0.515
Trees$Species_index[Trees$`Health Status`== 0 & Trees$Specie =="Robinia pseudoacacia"] <- 0.537 
Trees$Species_index[Trees$`Health Status`== 0 & Trees$Specie =="Acer platanoides"] <- 0.491
table(Trees$Species_index)

#Make filter for more nice code (D = dead)
UG <- Trees$`Health Status` == 1 & Trees$Specie =="Ulmus glabra"

TC <- Trees$`Health Status` == 1 & Trees$Specie =="Tilia cordata"
TC_D <- Trees$`Health Status` == 0 & Trees$Specie =="Tilia cordata"

AP <- Trees$`Health Status` == 1 & Trees$Specie =="Acer platanoides"
AP_D <- Trees$`Health Status` == 0 & Trees$Specie =="Acer platanoides"

FE <- Trees$`Health Status` == 1 & Trees$Specie =="Fraxinus excelsior"
FE_D <- Trees$`Health Status` == 0 & Trees$Specie =="Fraxinus excelsior"

QR <- Trees$`Health Status` == 1 & Trees$Specie =="Quercus robur"
QR_D <- Trees$`Health Status` == 0 & Trees$Specie =="Quercus robur"

MS <- Trees$`Health Status` == 1 & Trees$Specie =="Malus sylvestris"
MS_D <- Trees$`Health Status` == 0 & Trees$Specie =="Malus sylvestris"

RP <- Trees$`Health Status` == 1 & Trees$Specie =="Robinia pseudoacacia"
RP_D <- Trees$`Health Status` == 0 & Trees$Specie =="Robinia pseudoacacia"

PA <- Trees$`Health Status` == 1 & Trees$Specie =="Prunus armeniaca"
PA_D <- Trees$`Health Status` == 0 & Trees$Specie =="Prunus armeniaca"

#The volume of tress, calculatiuon depends on specie and their health status
Trees$Volume <- NA

Trees$Volume[UG] <- Trees$Height[UG] * Trees$`S of tree`[UG] * Trees$Species_index[UG]

Trees$Volume[UG] <- Trees$Height[UG] * Trees$`S of tree`[UG] * Trees$Species_index[UG]

Trees$Volume[TC] <- Trees$Height[TC] * Trees$`S of tree`[TC] * Trees$Species_index[TC]
Trees$Volume[TC_D] <- Trees$Height[TC_D] * Trees$`S of tree`[TC_D] * Trees$Species_index[TC_D]

Trees$Volume[AP] <- Trees$Height[AP] * Trees$`S of tree`[AP] * Trees$Species_index[AP]
Trees$Volume[AP_D] <- Trees$Height[AP_D] * Trees$`S of tree`[AP_D] * Trees$Species_index[AP_D]

Trees$Volume[FE] <- Trees$Height[FE] * Trees$`S of tree`[FE] * Trees$Species_index[FE]
Trees$Volume[FE_D] <- Trees$Height[FE_D] * Trees$`S of tree`[FE_D] * Trees$Species_index[FE_D]

Trees$Volume[QR] <- Trees$Height[QR] * Trees$`S of tree`[QR] * Trees$Species_index[QR]
Trees$Volume[QR_D] <- Trees$Height[QR_D] * Trees$`S of tree`[QR_D] * Trees$Species_index[QR_D]

Trees$Volume[MS] <- Trees$Height[MS] * Trees$`S of tree`[MS] * Trees$Species_index[MS]
Trees$Volume[MS_D] <- Trees$Height[MS_D] * Trees$`S of tree`[MS_D] * Trees$Species_index[MS_D]

Trees$Volume[RP] <- Trees$Height[RP] * Trees$`S of tree`[RP] * Trees$Species_index[RP]
Trees$Volume[RP_D] <- Trees$Height[RP_D] * Trees$`S of tree`[RP_D] * Trees$Species_index[RP_D]

Trees$Volume[PA] <- Trees$Height[PA] * Trees$`S of tree`[PA] * Trees$Species_index[PA]
Trees$Volume[PA_D] <- Trees$Height[PA_D] * Trees$`S of tree`[PA_D] * Trees$Species_index[PA_D]

# Changing names of the columns 
# Renaming the "Quantity" column to "N"
Trees <- Trees %>% rename(N = Quantity)

# Renaming the "Sample" column to "Site"
Trees <- Trees %>% rename(Site = Sample)

# Renaming the "S of tree" column to "Area"
Trees <- Trees %>% rename(Area = "S of tree")


###### !!!!DESCRIPTIVE STATISTICS!!!! ###### 

# Measures of Central tendency of Volume (Volume used as main indicator of trees life state)
mean(Trees$Volume)
var(Trees$Volume)
sd(Trees$Volume)
median(Trees$Volume)
quantile(Trees$Volume)
summary(Trees$Volume)

#Next, we will split our data into North and South regions 
#to further examine the influence of cymatic conditions on tree development.
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
#Next we will compare the same tree species in the north and south.
# Filtering species in north and south to find common species
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
# Fraxinus excelsior
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
# Quercus robur
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

#Calculation of covariance correlation coefficient
cov(Trees$`Mean D.`, Trees$Height)
cor(Trees$`Mean D.`, Trees$Height)
cor(north$`Mean D.`, north$Height)
cor(south$`Mean D.`, south$Height)

#Plots with regression model 
x <- Trees$`Mean D.`
y <- Trees$Height
# fit a linear regression model
fit <- lm(y ~ x)
# create a scatter plot of the data
plot(x, y)
# add the regression line to the plot
abline(fit, col = "red")
