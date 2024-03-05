
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
Trees_2$Volume <- Trees_2$`S of tree` * Trees_2$Height
### Объем не работает из - за запятой в формуле 
