### import data
?read.table
StudData <- read.table("StudDataset.csv", header = T, sep = ",")
StudData
class(StudData)
View(StudData)

### Delete the first column as it is not needed
str(StudData)
ncol(StudData)
nrow(StudData)

sd <- StudData [ , -c(1, ncol(StudData))]

str(sd)
ncol(sd)
nrow(sd)
View(sd)
###Change names of column
##names(stud.df) <- c("Age", "Height", "Country", "City" , "Roommate" , "Travel" , "KM" , "Skmow", 
##"HS", "UP", "UR", "UR", "RC")

names(sd) <- c('Age','Height','Country','City','Roomate','Way.of.treval','Distance',
               'Stat.skill','Hours.in.stat','p.use.of.programing','UseR', 'CR','CEx')

str(sd)
View(sd)

### Change of height
View(sd$Height)
str(sd$Height)
#Which - easy filter
which(sd$Height<100)

sd$Height[sd$Height < 100] <- 181

### Change of Country

class(sd$Country)
str(sd$Country)

sd$Country[sd$Country == "ITALY"] <-  "Italy"
sd$Country[sd$Country == "italy"] <-  "Italy"
sd$Country[sd$Country == "Italy "] <-  "Italy"
sd$Country[sd$Country == "SPAIN"] <-  "Spain"
#Change from character to factor to do more
Country.factor <- as.factor(sd$Country)
class(sd$Country)
Country.factor
levels(Country.factor)
class(Country.factor)
table(Country.factor)
### Transform intro a factor in the data frame 
stud.df$Country <-  as.factor(stud.df$Country)
### Не обязательно так как я для этого сделал отдельную переменую 
### Clean Roommate parameter 
str(sd$Roomate)
table(sd$Roomate)
#Cleaning
sd$Roomate [sd$Roomate > 10] <-  NA
table(sd$Roomate)

###Distance clean
class(sd$Distance)
#At this point we see some of the numbers inscribed with letters, so whole data is "character"
#We need change this because because the distance must be a number
as.numeric(sd$Distance)
#After that we lost our data (((((((((((((
#R.I.P. Distance data
#
View(sd)
#
#We do not lose our data I lie sorry (
#So lets actually clean our data 
sd$Distance[sd$Distance == "600 metri"] <- 0.6

###Create a vector with position incorrect positions
?is.na
?as.numeric
?as.character
is.na(as.character(sd$Distance))
is.na(as.numeric(sd$Distance))

idx <- is.na(as.numeric(sd$Distance))
idx

sd$Distance[idx]
sd[idx[2]] <- 2


####!!!!!
 




stud.df$Skmow
as.factor(stud.df$Skmow)
stud.df <- factor(stud.df$Skmow, levels = c(1,2,3,4,5)
stud.df$

  ###

  
stud.df$HS <- as.numeric(stud.df$HS)

stud.df$HS
View(stud.df)

### USer r
stud.df$UR
class(stud.df$R)
?NULL

ifelse(stud.df$UR)

write.csv()
write.table(x = stud.df,file = "Studentidataclean")

summary(stud.df)


### 

stud.df$Travel <-  as.factor(stud.df$Travel)
stud.df$Travel
sum(stud.df$Travel)


table(stud.df$Travel)
table(stud.df$Country)


#most used way comming to class

table(stud.df$Travel)

which.max(table(stud.df$Travel))


### What is the mean distance by those coming by train and walking

mean(stud.df$KM[stud.df$Travel=="walk"])

aggregate(x = stud.df$KM,
          by=list(Way=stud.df$Travel)
          FUN = mean)


stud.df$KM <-  as.numeric(stud.df$KM)

View(stud.df)
