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

### Graphs
##  Pie charts: Health Status for North and South Forests
## Separating health status column from the dataset
Health_north <- table(north$`Health Status`)
Health_south <- table(south$`Health Status`)
    
    ## creating a pie chart for North forest
    #  creating a data frame
Health_north_df <- data.frame(Health_north)

    # Calculating % of alive/dead trees 
sum(Health_north_df$Freq)
Health_north_df$percent <- round(Health_north_df$Freq/sum(Health_north_df$Freq)*100,1)
Health_north_df$perc_label <- paste0(Health_north_df$percent, "%")
    
    # Plotting pie chart
pie_health_north <- pie(x = Health_north_df$Freq,
    labels = Health_north_df$perc_label,
    col = c("#FF6169", "lightgreen"),
    main = "Health Status of Trees in the North Forest")

    # color pallet for the pie chart
piehealthcol <- (col= c("lightgreen","#FF6169"))

    # Adding a legend
legend("topright", legend = c("Alive: 90.5%",
                            "Dead: 9.5%"),
       fill = piehealthcol)

    ## creating a pie chart for South forest
    #  creating a data frame
Health_south_df <- data.frame(Health_south)

    # Calculating % of alive/dead trees 
sum(Health_south_df$Freq)
Health_south_df$percent <- round(Health_south_df$Freq/sum(Health_south_df$Freq)*100,1)
Health_south_df$perc_label <- paste0(Health_south_df$percent, "%")

    # Plotting pie chart
pie_health_south <- pie(x = Health_south_df$Freq,
    labels = Health_south_df$perc_label,
    col = c("#FF6169", "lightgreen"),
    main = "Health Status of Trees in the South Forest")

    # color pallet for the pie chart
piehealthcol <- (col= c("lightgreen","#FF6169"))

    # Adding a legend
legend("topright", legend = c("Alive: 86.3%",
                                "Dead: 13.7%"),
       fill = piehealthcol)

## Plotting Bar Graphs
    # creating a vector
speci_vol_forest_N <- c(AP_mean_north,FE_mean_north,QR_mean_north)
speci_vol_forest_S <- c(AP_mean_south,FE_mean_south,QR_mean_south)
species_vol_forest_name <- c("A.p. N","A.p. S","F.e. N","F.e. S","Q.r. N", "Q.r. S")         # 1 version
species_vol_forest_name <- c("Acer platanoides","Fraxinus excelsior","Quercus robur")        # another version but still needs fixing!!!!
barplot(species_vol_forest, xlab= "Tree Species", ylab= "Mean Volume of Trees in Cubic Meters",
        names.arg = species_vol_forest_name, main = "Comparison of Tree Volume of Species in North and South Forests",
        ylim=c(0,0.6))

    # Bar plot where the bars are next to each other
?barplot
survey <- rbind(speci_vol_forest_N, speci_vol_forest_S)
barplot(survey,
        beside = TRUE,
        col = c("dodgerblue3", "skyblue1","darkgreen", "lightgreen","orange", "lightyellow"), 
        legend.text = rownames(species_vol_forest_name),
        args.legend = list(cex=0.75,x = "topright"),
        ylim = c(0,0.6), main = "Comparison of Tree Volume of Species in North and South Forests")
?barplot
