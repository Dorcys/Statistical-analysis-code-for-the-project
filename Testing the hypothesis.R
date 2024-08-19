####Testing the hypothesis####

##First we need test if our data have normal distribution 
#Shapiro test
#General test 
shapiro.test(Trees$Volume)
shapiro.test(Trees$`Mean D.`)
shapiro.test(Trees$Height)
shapiro.test(Trees$`Health Status`)
shapiro.test(Trees$Volume[Trees$Specie == "Quercus robur"])
shapiro.test(Trees$Volume[Trees$Specie == "Acer platanoides"])
shapiro.test(Trees$Volume[Trees$Specie == "Fraxinus excelsior"])
#North & South sites
shapiro.test(north$Volume)
shapiro.test(south$Volume)
shapiro.test(north$`Mean D.`)
shapiro.test(north$Height)
shapiro.test(north$`Health Status`)
shapiro.test(south$`Mean D.`)
shapiro.test(south$Height)
shapiro.test(south$`Health Status`
shapiro.test(south$Area)
#Species
shapiro.test(QR_N$Volume)
shapiro.test(QR_S$Volume)

shapiro.test(FE_N$Volume)
shapiro.test(FE_S$Volume)

shapiro.test(AP_N$Volume)
shapiro.test(AP_S$Volume)#0.01028 < 0.05
#From the output, the p-value > 0.05 
#implying that the distribution of the data are not significantly different from normal distribution. 
#In other words, we can assume the normality.
#We only can find normal distribution  in  2/3 species 
# In hypothesis testing we can only use Quercus robur & Fraxinus excelsior 
FE_mean_north > FE_mean_south
QR_mean_north > QR_mean_south
#Testing variance 
QR_N$Volume
QR_S$Volume
FE_N$Volume
FE_S$Volume
var.test(QR_N$Volume, QR_S$Volume)
var.test(FE_N$Volume, FE_S$Volume)
#Variance is not same 
