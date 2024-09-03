##### Importing Data ##### 

install.packages("dplyr")
install.packages("readr")
library(readr)
library(dplyr)
locale_dec_comma <- locale(decimal_mark = ",")
Trees <- read_csv("Trees.csv", locale = locale_dec_comma)
View(Trees)

##### DATA CLEANING ##### 

# Translating species names from Russian to Latin names
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

# Summary of species 
table(Trees$Specie)

# Creating a new column for calculations 
Trees$Species_index <- NA
Trees$Species_index[Trees$Specie == "Ulmus glabra"] <- 0.569
Trees$Species_index[Trees$Specie == "Tilia cordata"] <- 0.472
Trees$Species_index[Trees$Specie == "Prunus armeniaca"] <- 0.550
Trees$Species_index[Trees$Specie == "Quercus robur"] <- 0.494
Trees$Species_index[Trees$Specie == "Fraxinus excelsior"] <- 0.468
Trees$Species_index[Trees$Specie == "Malus sylvestris"] <- 0.515
Trees$Species_index[Trees$Specie == "Robinia pseudoacacia"] <- 0.508
Trees$Species_index[Trees$Specie == "Acer platanoides"] <- 0.470

# Using filters for adding indexes for dead trees
Trees$Species_index[Trees$`Health Status`== 0 & Trees$Specie =="Prunus armeniaca"] <- 0.569
Trees$Species_index[Trees$`Health Status`== 0 & Trees$Specie =="Quercus robur"] <- 0.505
Trees$Species_index[Trees$`Health Status`== 0 & Trees$Specie =="Malus sylvestris"] <- 0.515
Trees$Species_index[Trees$`Health Status`== 0 & Trees$Specie =="Robinia pseudoacacia"] <- 0.537 
table(Trees$Species_index)

# Assign variables to divide species based on health status (D = dead)
UG <- Trees$`Health Status` == 1 & Trees$Specie =="Ulmus glabra"

TC <- Trees$`Health Status` == 1 & Trees$Specie =="Tilia cordata"

AP <- Trees$`Health Status` == 1 & Trees$Specie =="Acer platanoides"

FE <- Trees$`Health Status` == 1 & Trees$Specie =="Fraxinus excelsior"

QR <- Trees$`Health Status` == 1 & Trees$Specie =="Quercus robur"
QR_D <- Trees$`Health Status` == 0 & Trees$Specie =="Quercus robur"

MS <- Trees$`Health Status` == 1 & Trees$Specie =="Malus sylvestris"
MS_D <- Trees$`Health Status` == 0 & Trees$Specie =="Malus sylvestris"

RP <- Trees$`Health Status` == 1 & Trees$Specie =="Robinia pseudoacacia"
RP_D <- Trees$`Health Status` == 0 & Trees$Specie =="Robinia pseudoacacia"

PA <- Trees$`Health Status` == 1 & Trees$Specie =="Prunus armeniaca"
PA_D <- Trees$`Health Status` == 0 & Trees$Specie =="Prunus armeniaca"

# Creating an empty column for Volume calculations 

Trees$Volume <- NA

# Calculating volume of trees (depends on specie and their health status) 

Trees$Volume[UG] <- Trees$Height[UG] * Trees$`S of tree`[UG] * Trees$Species_index[UG]

Trees$Volume[TC] <- Trees$Height[TC] * Trees$`S of tree`[TC] * Trees$Species_index[TC]

Trees$Volume[AP] <- Trees$Height[AP] * Trees$`S of tree`[AP] * Trees$Species_index[AP]

Trees$Volume[FE] <- Trees$Height[FE] * Trees$`S of tree`[FE] * Trees$Species_index[FE]

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


#### DESCRIPTIVE STATISTICS #### 

# Measures of Central tendency of Volume 
# (Volume used as main indicator of trees life state)

mean(Trees$Volume)
var(Trees$Volume)
sd(Trees$Volume)
median(Trees$Volume)
quantile(Trees$Volume)
summary(Trees$Volume)

# Splitting the data into North and South regions 
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

# Creating subsets for each common specie 

# Acer platanoides
AP_N <- subset(north, Specie == "Acer platanoides")
AP_S <- subset(south, Specie == "Acer platanoides")

# Fraxinus excelsior
FE_N <- subset(north, Specie == "Fraxinus excelsior")
FE_S <- subset(south, Specie == "Fraxinus excelsior")

# Quercus robur
QR_N <- subset(north, Specie == "Quercus robur")
QR_S <- subset(south, Specie == "Quercus robur")

## Measures of Central tendency for common species

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
#Test statistic
t.test(QR_N$Volume, QR_S$Volume,  var.equal = F)
t.test(FE_N$Volume, FE_S$Volume, var.equal = F)


#### PLOTS ####  

## 1. BAR PLOT - mean volumes of trees for North and South sites

# Changing font
par(family = "serif")

# Organizing north and south sites  
sites <- c("North", "South")
mean_volumes <- c(mean_north, mean_south)

# Creating bar plot
bar_positions <- barplot(mean_volumes, 
                         names.arg = sites, 
                         col = c("#00B8E7", "#F8766D"),
                         main = "Mean Volume of Trees in North and South Sites",
                         ylab = expression("Mean Volume"~(m^3)),
                         xlab = "Site",
                         ylim = c(0, 0.35))

# Adding the exact mean volume value above each bar for clear comparison
text(x = bar_positions, y = mean_volumes, 
     labels = round(mean_volumes, 3), 
     pos = 3, cex = 0.8, col = "black")


## 2. PIE CHART - health status of trees in each forest (north and south)

# Creating two plots on the same frame
par(mfrow = c(1, 2), family = "serif")

# Separating health status column from the dataset
Health_north <- table(north$`Health Status`)
Health_south <- table(south$`Health Status`)

# Creating a pie chart for North forest
Health_north_df <- data.frame(Health_north)

# Calculating % of alive/dead trees 
sum(Health_north_df$Freq)
Health_north_df$percent <- round(Health_north_df$Freq/sum(Health_north_df$Freq)*100,1)
Health_north_df$perc_label <- paste0(Health_north_df$percent, "%")

# Plotting pie chart
pie_health_north <- pie(x = Health_north_df$Freq,
                        labels = Health_north_df$perc_label,
                        col = c("#FF6169", "lightgreen"),
                        main = "North Forest",
                        cex.main = 1.2,
                        line = 0)

# Creating a color pallet for the pie chart
piehealthcol <- (col= c("lightgreen","#FF6169"))

# Adding a legend
legend("bottomleft", legend = c("Alive: 96.3%",
                                "Dead: 3.7%"),
       fill = piehealthcol,
       bty = "n")

# Creating a pie chart for South forest
Health_south_df <- data.frame(Health_south)

# Calculating % of alive/dead trees 
sum(Health_south_df$Freq)
Health_south_df$percent <- round(Health_south_df$Freq/sum(Health_south_df$Freq)*100,1)
Health_south_df$perc_label <- paste0(Health_south_df$percent, "%")

# Plotting pie chart
pie_health_south <- pie(x = Health_south_df$Freq,
                        labels = Health_south_df$perc_label,
                        col = c("#FF6169", "lightgreen"),
                        main = "South Forest",
                        cex.main = 1.2,
                        line = 0)

# Creating a color pallet for the pie chart
piehealthcol <- (col= c("lightgreen","#FF6169"))

# Adding a legend
legend("bottomleft", legend = c("Alive: 98.7%",
                                "Dead: 1.3%"),
       fill = piehealthcol,
       bty = "n")

# Adding a title to the whole plot
mtext("Health Status of Trees in North and South Forests", 
      side = 1, line = -2, cex = 1.7, font = 2, outer = TRUE)

dev.off()


## 3. BAR PLOT - Number of each species in both forests (North and South)
par(family = "serif")

# Filtering the number of species in the north forest
north_specie_count <- Trees %>%
  filter(Site %in% c("N1", "N2", "N3")) %>%
  group_by(Specie) %>%
  summarise(Count_North = n()) %>%
  arrange(Specie)

# Filtering the number of species in the south forest
south_specie_count <- Trees %>%
  filter(Site %in% c("S1", "S2", "S3")) %>%
  group_by(Specie) %>%
  summarise(Count_South = n()) %>%
  arrange(Specie)

# Merging the North and South species
combined_counts <- full_join(north_specie_count, south_specie_count, by = "Specie")
combined_counts[is.na(combined_counts)] <- 0

# Data for the bar plot
species <- combined_counts$Specie
counts <- rbind(combined_counts$Count_North, combined_counts$Count_South)
colors <- c("yellowgreen", "gold")

# Creating a grouped bar plot
barplot(counts,
        beside = TRUE,
        #names.arg = species,
        col = colors,
        las = 2,
        main = "Number of Each Species in North and South Forests",
        ylab = "Number of Trees",
        xlab = "Species",
        ylim = c(0, max(counts) * 1.2),
        legend = c("North Site", "South Site"),
        args.legend = list(x = "topright", bty = "n"))

# Adding grid lines to the bar plot
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted", lwd = 0.5)

# Placing grid lines behind the bar plot
bar_positions <- barplot(counts,
                         beside = TRUE,
                         col = colors,
                         las = 2,
                         main = "",
                         ylab = "",
                         xlab = "",
                         ylim = c(0, max(counts) * 1.2),
                         xaxt = 'n',  
                         add = TRUE)  

# Calculating the midpoints for each grouped bars
midpoints <- colMeans(bar_positions)

# Creating the x-axis labels
text(x = midpoints,
     y = -0.5,  
     labels = species,
     srt = 35,  
     adj = c(1.0, 0.5),  
     xpd = TRUE, 
     cex = 0.8) 

dev.off()


## 4. BOX PLOT - mean volume of trees in both forests
library(ggplot2)
library(dplyr)

# Filtering the data for the North and South Sites
north <- Trees %>% filter(Site %in% c("N1", "N2", "N3"))
south <- Trees %>% filter(Site %in% c("S1", "S2", "S3"))

# Combining North and South sites in a single data frame
Trees_combined <- rbind(
  north %>% mutate(Region = "North"),
  south %>% mutate(Region = "South")
)

# Plotting
ggplot(Trees_combined, aes(x = Region, y = Volume, fill = Region)) +
  geom_boxplot() +
  scale_fill_manual(values = c("North" = "purple", "South" = "#F8766D")) +
  labs(title = "Volume of Trees in Each Site",
       x = "Region",
       y = expression("Mean Volume"~(m^3))) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5) 
  ) +
  coord_cartesian(ylim = c(0, 0.8))  


## 5. BAR GRAPH - comparing the common species in both forests

#Creating an object for the common species between North and South Sites
common_species <- intersect(north$Specie, south$Specie)

# Sub-setting data
north_common <- subset(north, Specie %in% common_species)
south_common <- subset(south, Specie %in% common_species)

# Calculating the mean volume for each shared species in North and South Sites
mean_volume_north <- north_common %>%
  group_by(Specie) %>%
  summarise(mean_volume = mean(Volume, na.rm = TRUE))

mean_volume_south <- south_common %>%
  group_by(Specie) %>%
  summarise(mean_volume = mean(Volume, na.rm = TRUE))

mean_volume_data <- merge(mean_volume_north, mean_volume_south, by = "Specie", suffixes = c("_North", "_South"))

mean_volume_long <- tidyr::gather(mean_volume_data, key = "Region", value = "Mean_Volume", mean_volume_North, mean_volume_South)

# Creating bar plot
ggplot(mean_volume_long, aes(x = Specie, y = Mean_Volume, fill = Region)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(Mean_Volume, 3)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3) +
  labs(title = "Mean Volume of Common Tree Species in North and South Sites",
       x = "Species",
       y = expression("Mean Volume"~(m^3)),
       fill = "Site") +
  scale_fill_manual(values = c("mediumturquoise", "palegreen"),
                    labels = c("North Site", "South Site")) +
  theme_minimal()


## 6. REGRESSION LINE - volume and diameter   

# Creating the scatter plot
ggplot(Trees, aes(x = `Mean D.`, y = Volume, color = Specie)) +
  geom_point(alpha = 0.6, size = 1) +  # Scatter plot points with transparency
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.5) +  # Adding regression line
  labs(title = "Correlation of Mean Diameter and Volume by Species",
       x = "Mean Diameter (Mean D.)",
       y = expression("Mean Volume"~(m^3)),
       color = "Species") +  
  theme_minimal()





#### INFERENTIAL STATISTICS #### 

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
