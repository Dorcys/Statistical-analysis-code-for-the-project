library(readr)
#Fix problem  with "," insted of "."
locale_dec_comma <- locale(decimal_mark = ",")
Trees <- read_csv("Trees.csv", locale = locale_dec_comma)
Trees_2 <- read_csv("Trees.2.csv")
View(Trees_2)
#Translation of species names from Russian code to latin names
Trees_2$Specie[Trees_2$Specie == "ЛПМ"] <- "Tilia cordata"
Trees_2$Specie[Trees_2$Specie == "Аб"] <- "Prunus armeniaca"
Trees_2$Specie[Trees_2$Specie == "Д"] <- "Quercus robur"
Trees_2$Specie[Trees_2$Specie == "Яс"] <- "Fraxinus excelsior"
Trees_2$Specie[Trees_2$Specie == "ЯСО"] <- "Fraxinus excelsior"
Trees_2$Specie[Trees_2$Specie == "ЯБЛ"] <- "Malus sylvestris"
Trees_2$Specie[Trees_2$Specie == "Рб"] <- "Robinia pseudoacacia"
Trees_2$Specie[Trees_2$Specie == "Р"] <- "Robinia pseudoacacia"
Trees_2$Specie[Trees_2$Specie == "КЛО"] <- "Acer platanoides"
Trees_2$Specie[Trees_2$Specie == "КлО"] <- "Acer platanoides"
Trees_2$Specie[Trees_2$Specie == "ВЯЗ"] <- "Ulmus glabra"
#Summary of species 
table(Trees_2$Specie)
#Assign some indexes (15)
UG <- 0.569
TC <- 0.472
TC_D <-  0.463
AP <- 0.470
AP_D <- 0.491
FE <-  0.468
FE_D <- 0.508
QR <- 0.494
QR_D <- 0.505
MS <-  0.515
MS_D <-  0.515
RP <- 0.508
RP_D <- 0.537
PA <- 0.550
PA_D <- 0.569
#Create new colloum for calculations 
Trees_2$Species_index <- "Species index"
#Using filters for adding some inexes
Trees_2$Species_index[Trees_2$Specie == "Ulmus glabra"] <- UG
Trees_2$Species_index[Trees_2$Specie == "Tilia cordata"] <- TC 
Trees_2$Species_index[Trees_2$Specie == "Prunus armeniaca"] <- PA 
Trees_2$Species_index[Trees_2$Specie == "Quercus robur"] <- QR
Trees_2$Species_index[Trees_2$Specie == "Fraxinus excelsior"] <- FE
Trees_2$Species_index[Trees_2$Specie == "Malus sylvestris"] <- MS 
Trees_2$Species_index[Trees_2$Specie == "Robinia pseudoacacia"] <- RP 
Trees_2$Species_index[Trees_2$Specie == "Acer platanoides"] <- AP
#Using filters for adding some inexes part 2 
Trees_2$Species_index[Trees_2$`Health Status`== 0 & Trees_2$Specie =="Tilia cordata"] <- TC_D
Trees_2$Species_index[Trees_2$`Health Status`== 0 & Trees_2$Specie =="Prunus armeniaca"] <- PA_D 
Trees_2$Species_index[Trees_2$`Health Status`== 0 & Trees_2$Specie =="Quercus robur"] <- QR_D
Trees_2$Species_index[Trees_2$`Health Status`== 0 & Trees_2$Specie =="Fraxinus excelsior"] <- FE_D 
Trees_2$Species_index[Trees_2$`Health Status`== 0 & Trees_2$Specie =="Malus sylvestris"] <- MS_D
Trees_2$Species_index[Trees_2$`Health Status`== 0 & Trees_2$Specie =="Robinia pseudoacacia"] <- RP_D 
Trees_2$Species_index[Trees_2$`Health Status`== 0 & Trees_2$Specie =="Acer platanoides"] <- AP_D
#Adding volume column
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
Trees$Species_index


#make filter for more nice code (D = dead)
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

Trees$Volume[PA ] <- Trees$Height[PA ] * Trees$`S of tree`[PA ] * Trees$Species_index[PA ]
Trees$Volume[PA_D] <- Trees$Height[PA_D] * Trees$`S of tree`[PA_D] * Trees$Species_index[PA_D]

Trees$Volume

##########################################Paula####################################################################
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
Trees$Specie[Trees$Specie == "ЯСО"] <- "Fraxinus excelsior"
Trees$Specie[Trees$Specie == "ЯБЛ"] <- "Malus sylvestris"
Trees$Specie[Trees$Specie == "Рб"] <- "Robinia pseudoacacia"
Trees$Specie[Trees$Specie == "Р"] <- "Robinia pseudoacacia"
Trees$Specie[Trees$Specie == "КЛО"] <- "Acer platanoides"
Trees$Specie[Trees$Specie == "КлО"] <- "Acer platanoides"
Trees$Specie[Trees$Specie == "ВЯЗ"] <- "Ulmus glabra"

#Summary of species 
table(Trees$Specie)

#Assign the constants to calculate the volume (D = dead)
UG <- 0.569
TC <- 0.472
TC_D <-  0.463
AP <- 0.470
AP_D <- 0.491
FE <-  0.468
FE_D <- 0.508
QR <- 0.494
QR_D <- 0.505
MS <-  0.515
MS_D <-  0.515
RP <- 0.508
RP_D <- 0.537
PA <- 0.550
PA_D <- 0.569

#Create new column for calculations 
Trees$Species_index <- "Species index"

#Using filters for adding some indexes
Trees$Species_index[Trees$Specie == "Ulmus glabra"] <- UG 
Trees$Species_index[Trees$Specie == "Tilia cordata"] <- TC
Trees$Species_index[Trees$Specie == "Prunus armeniaca"] <- PA
Trees$Species_index[Trees$Specie == "Quercus robur"] <- QR
Trees$Species_index[Trees$Specie == "Fraxinus excelsior"] <- FE
Trees$Species_index[Trees$Specie == "Malus sylvestris"] <- MS
Trees$Species_index[Trees$Specie == "Robinia pseudoacacia"] <- RP
Trees$Species_index[Trees$Specie == "Acer platanoides"] <- AP

#Using filters for adding some indexes part 2 
Trees$Species_index[Trees$`Health Status`== 0 & Trees$Specie =="Tilia cordata"] <- TC_D 
Trees$Species_index[Trees$`Health Status`== 0 & Trees$Specie =="Prunus armeniaca"] <- PA_D
Trees$Species_index[Trees$`Health Status`== 0 & Trees$Specie =="Quercus robur"] < QR_D
Trees$Species_index[Trees$`Health Status`== 0 & Trees$Specie =="Fraxinus excelsior"] <- FE_D   
Trees$Species_index[Trees$`Health Status`== 0 & Trees$Specie =="Malus sylvestris"] <- MS_D
Trees$Species_index[Trees$`Health Status`== 0 & Trees$Specie =="Robinia pseudoacacia"] <- RP_D 
Trees$Species_index[Trees$`Health Status`== 0 & Trees$Specie =="Acer platanoides"] <- AP_D

#Adding volume column - calculated by forestry formula 
Trees$Volume <- Trees$`S of tree` * Trees$Height 

### we need to add s of trees * trees height * indexes for each species

# Renaming the "Quantity" column to "N"
Trees <- Trees %>% rename(N = Quantity)

# Renaming the "Sample" column to "Site"
Trees <- Trees %>% rename(Site = Sample)

# Renaming the "S of tree" column to "Area"
Trees <- Trees %>% rename(Area = "S of tree")
#Volume but still need update
Trees$Volume[Trees$`Health Status` == 1 & Trees$Specie =="Acer platanoides"] <- Trees$`S of tree`[Trees$`Health Status` == 1 & Trees$Specie =="Acer platanoides"] * 0.47
#Note problem with Trees$Specie need change to nubers not variables 





