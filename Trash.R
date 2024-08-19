####This is final code for this project####
install.packages("dplyr")
install.packages("readr")
library(readr)
library(dplyr)
#Fix problem  with "," insted of "."
locale_dec_comma <- locale(decimal_mark = ",")
Trees <- read_csv("Trees.csv", locale = locale_dec_comma)
View(Trees)
####Code cleaning####
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
#We compared volume within regions and species in addition to that we gonna compere more variable

#The results might be the same as we had in volume calculations, 
#because the diameter and height are directly used in volume calculations
#Comparison of hightn mean diameter in whole forest
mean(Trees$`Mean D.`)
mean(Trees$Height)
median(Trees$`Mean D.`)
median(Trees$Height)
var(Trees$`Mean D.`)
var(Trees$Height)
sd(Trees$`Mean D.`)
sd(Trees$Height)
#Comparison between North & South 
#North
mean(north$`Mean D.`)
mean(north$Height)
median(north$`Mean D.`)
median(north$Height)
var(north$`Mean D.`)
var(north$Height)
sd(north$`Mean D.`)
sd(north$Height)
#South
mean(south$`Mean D.`)
mean(south$Height)
median(south$`Mean D.`)
median(south$Height)
var(south$`Mean D.`)
var(south$Height)
sd(south$`Mean D.`)
sd(south$Height)

#Now same but within species in different sites 
#AP_N QR_N FE_N
#AP_S QR_S FE_S
mean(AP_N$`Mean D.`)
mean(AP_N$Height)
mean(AP_S$`Mean D.`)
mean(AP_S$Height) 

mean(QR_N$`Mean D.`)
mean(QR_N$Height)
mean(QR_S$`Mean D.`)
mean(QR_S$Height)

mean(FE_N$`Mean D.`)
mean(FE_N$Height)
mean(FE_S$`Mean D.`)
mean(FE_S$Height)


#####Some underdeveloped stuff#####
#Calculation of covariance correlation coefficient
cov(Trees$`Mean D.`, Trees$Height)
cor(Trees$`Mean D.`, Trees$Height)
cor(north$`Mean D.`, north$Height)
cor(south$`Mean D.`, south$Height)

  
### Graphs ###
#Plots with regression model 
x <- Trees$`Mean D.`
y <- Trees$Height
# fit a linear regression model
lm(y ~ x)
fit <- lm(y ~ x)
summary(fit)$r.squared #Coefficient of Determination
# create a scatter plot of the data
plot(x, y)
# add the regression line to the plot
abline(fit, col = "red")

plot(north$`Mean D.`, north$Volume)
points(south$`Mean D.`, south$Volume, col="red")

Volume <-  data.frame(Site = c(Trees$Site),
                       Volume = c(Trees$Volume))
View(Volume)
Volume$Site[Volume$Site == "N1"] <- "North"
Volume$Site[Volume$Site == "N2"] <- "North"
Volume$Site[Volume$Site == "N3"] <- "North"
Volume$Site[Volume$Site == "S1"] <- "South"
Volume$Site[Volume$Site == "S2"] <- "South"
Volume$Site[Volume$Site == "S3"] <- "South"
barplot(mean(Volume$Volume[Volume$Site == "North"]), mean(Volume$Volume[Volume$Site == "South"]))
mean(Volume$Volume[Volume$Site == "North"])
mean(Volume$Volume[Volume$Site == "South"])

boxplot(Volume$Volume[Volume$Site == "North"])

par(mfrow = c(1,2))
boxplot(Volume$Volume[Volume$Site == "North"], ylim = c(0.0, 1.0))
boxplot(Volume$Volume[Volume$Site == "South"], ylim = c(0.0, 1.0))

###Chi### This whole thing is wrong !!!!!!
CST <-data.frame(
                      QR = c(QR_mean_north, QR_mean_south),
                      FE = c(FE_mean_north, FE_mean_south),
                      AP = c(AP_mean_north, AP_mean_south))
CST

chisq.test(CST)

table(south$Specie)
table(north$Specie)
CST_N <-  data.frame(
                    QR = c(33, 44),
                    FE = c(33, 48),
                    AP = c(29,21))

chisq.test(CST_N)

dev.off()

##08##
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
#We compared volume within regions and species in addition to that we gonna compere more variable

#The results might be the same as we had in volume calculations, 
#because the diameter and height are directly used in volume calculations
#Comparison of hightn mean diameter in whole forest
mean(Trees$`Mean D.`)
mean(Trees$Height)
median(Trees$`Mean D.`)
median(Trees$Height)
var(Trees$`Mean D.`)
var(Trees$Height)
sd(Trees$`Mean D.`)
sd(Trees$Height)
#Comparison between North & South 
#North
mean(north$`Mean D.`)
mean(north$Height)
median(north$`Mean D.`)
median(north$Height)
var(north$`Mean D.`)
var(north$Height)
sd(north$`Mean D.`)
sd(north$Height)
#South
mean(south$`Mean D.`)
mean(south$Height)
median(south$`Mean D.`)
median(south$Height)
var(south$`Mean D.`)
var(south$Height)
sd(south$`Mean D.`)
sd(south$Height)

#Now same but within species in different sites 
#AP_N QR_N FE_N
#AP_S QR_S FE_S
mean(AP_N$`Mean D.`)
mean(AP_N$Height)
mean(AP_S$`Mean D.`)
mean(AP_S$Height) 

mean(QR_N$`Mean D.`)
mean(QR_N$Height)
mean(QR_S$`Mean D.`)
mean(QR_S$Height)

mean(FE_N$`Mean D.`)
mean(FE_N$Height)
mean(FE_S$`Mean D.`)
mean(FE_S$Height)

shapiro.test(Trees$Volume)
shapiro.test(Trees$`Mean D.`)
shapiro.test(Trees$Height)
shapiro.test(Trees$`Health Status`)
shapiro.test(north$Volume)
shapiro.test(south$Volume)
shapiro.test(north$`Mean D.`)
shapiro.test(north$Height)
shapiro.test(north$`Health Status`)
shapiro.test(south$`Mean D.`)
shapiro.test(south$Height)
shapiro.test(south$`Health Status`
shapiro.test(south$Area)
shapiro.test(QR_N$Volume)
shapiro.test(Trees$Volume[Trees$Specie == "Quercus robur"])
shapiro.test(Trees$Volume[Trees$Specie == "Acer platanoides"])
shapiro.test(Trees$Volume[Trees$Specie == "Fraxinus excelsior"])
shapiro.test(QR_S$Volume)
shapiro.test(FE_N$Volume)
shapiro.test(FE_S$Volume)
shapiro.test(AP_N$Volume)
shapiro.test(AP_S$Volume)

#####Some underdeveloped stuff#####

Welch Two Sample t-test
MVQRN <- QR_N$Volume
MVQRS <- QR_S$Volume

t.test(MVQRN,MVQRS, "two.sided", var.equal = FALSE)

#To calculate Bernuli destribution try to do  as.factor T or F 

#Calculation of covariance correlation coefficient
cov(Trees$`Mean D.`, Trees$Height)
cor(Trees$`Mean D.`, Trees$Height)
cor(north$`Mean D.`, north$Height)
cor(south$`Mean D.`, south$Height)

north$`Health Status`[north$`Health Status` == 0]/north$`Health Status`
compute(north$`Health Status`[north$`Health Status` == 0])
XYZ <-  c (1,2,3,4,5,5,5,5,5,5,5,5)
count(XYZ)
View(north)
### Graphs ###
#Plots with regression model 
x <- Trees$`Mean D.`
y <- Trees$Height
# fit a linear regression model
lm(y ~ x)
fit <- lm(y ~ x)
summary(fit)$r.squared #Coefficient of Determination
# create a scatter plot of the data
plot(x, y)
# add the regression line to the plot
abline(fit, col = "red")
View(south)

plot(north$`Mean D.`, north$Volume)
points(south$`Mean D.`, south$Volume, col="red")
points(south$`Mean D.`[south$Specie == "Fraxinus excelsior"], col = "blue")

Volume <-  data.frame(Site = c(Trees$Site),
                       Volume = c(Trees$Volume))
View(Volume)
Volume$Site[Volume$Site == "N1"] <- "North"
Volume$Site[Volume$Site == "N2"] <- "North"
Volume$Site[Volume$Site == "N3"] <- "North"
Volume$Site[Volume$Site == "S1"] <- "South"
Volume$Site[Volume$Site == "S2"] <- "South"
Volume$Site[Volume$Site == "S3"] <- "South"
barplot(mean(Volume$Volume[Volume$Site == "North"]), mean(Volume$Volume[Volume$Site == "South"]))
mean(Volume$Volume[Volume$Site == "North"])
mean(Volume$Volume[Volume$Site == "South"])

boxplot(Volume$Volume[Volume$Site == "North"])

par(mfrow = c(1,2))
boxplot(Volume$Volume[Volume$Site == "North"], ylim = c(0.0, 1.0))
boxplot(Volume$Volume[Volume$Site == "South"], ylim = c(0.0, 1.0))

###Chi### This whole thing is wrong !!!!!!
CST <-data.frame(
                      QR = c(QR_mean_north, QR_mean_south),
                      FE = c(FE_mean_north, FE_mean_south),
                      AP = c(AP_mean_north, AP_mean_south))
CST

chisq.test(CST)

table(south$Specie)
table(north$Specie)
CST_N <-  data.frame(
                    QR = c(33, 44),
                    FE = c(33, 48),
                    AP = c(29,21))

chisq.test(CST_N)

dev.off()

