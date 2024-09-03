#--------------------------------------
    # INFERENTIAL STATISTICS

#required packages
install.packages("coin") # For Mann-Whitney U test (wilcox_test)

#loading packages
library(coin)

#---------------
    # Checking for normality of the whole data set
   
    ## VISUAL CHECKS

    # Histogram:
    # We can also visualize if our data is normally distributed by plotting it on a histogram
hist(Trees$Volume, main = "Histogram of Tree Volume", xlab = expression("Mean Volume"~(m^3)), col = "lightblue", breaks = 20)
    # the data is heavily right skewed

    # Q-Q Plot:
    # Q-Q plot allows us to visualize normality of a dataset by plotting the points along a 45 degree angle line
    # If the points fall along the line, this indicated that the data follows a normal distribution
qqnorm(Trees$Volume, main = "Q-Q Plot of Tree Volume")
qqline(Trees$Volume, col = "red", lwd = 2)
    # the points do not fall on the 45 degree line which indicated that the data is not normally distributed

#---------------
    # A more precise way to check for normality is by performing statistical tests
   
    ## STATISTICAL CHECKS

shapiro.test(Trees$Volume)

    # Shapiro-Wilk normality test

    # data:  Trees$Volume
    # = 0.85497, p-value = 6.491e-16

    # The p value is less than 0.05 which means we can reject the null hypothesis that the Volume of Trees data is normally distributed



    # Checking normality for subsets

    # Normality of the volume in the north site

    shapiro.test(north$Volume) # Data not normally distributed

    #Shapiro-Wilk normality test

    #data:  north$Volume
    #W = 0.97544, p-value = 0.01465


    shapiro.test(south$Volume) # DATA NORMALLY DISTRIBUTED
    # Shapiro-Wilk normality test

    # data:  south$Volume
    # W = 0.98441, p-value = 0.07465


#---------------
# Checking normality of the remaining variables
shapiro.test(Trees$`Mean D.`) # not normally distributed
shapiro.test(Trees$Height)    # not normally distributed
shapiro.test(Trees$Volume[Trees$Specie == "Quercus robur"])       # not normally distributed
shapiro.test(Trees$Volume[Trees$Specie == "Acer platanoides"])    # not normally distributed
shapiro.test(Trees$Volume[Trees$Specie == "Fraxinus excelsior"])  # not normally distributed

# Checking Normality for variables in the North & South sites
shapiro.test(north$`Mean D.`) # not normally distributed
shapiro.test(north$Height)    # not normally distributed
shapiro.test(south$`Mean D.`) # not normally distributed
shapiro.test(south$Height)    # not normally distributed

# Checking Normality for Species
             shapiro.test(QR_N$Volume) #NORMALLY DISTRIBUTED
             shapiro.test(QR_S$Volume) #NORMALLY DISTRIBUTED
             
             shapiro.test(FE_N$Volume) #NORMALLY DISTRIBUTED
             shapiro.test(FE_S$Volume) #NORMALLY DISTRIBUTED
             
             shapiro.test(AP_N$Volume) # not normally distributed
             shapiro.test(AP_S$Volume) # NORMALLY DISTRIBUTED
             
#Testing if the variance is equal in 3 species in the North and South sites
             
             var.test(QR_N$Volume, QR_S$Volume) # variances are not equal
             var.test(FE_N$Volume, FE_S$Volume) # variances are not equal
             var.test(AP_N$Volume, AP_S$Volume) # variances are not equal
#---------------
## 1. Welch's t-test: to test the means of two datasets with unequal variances

             #H0: The means of Volume of the Fraxinus excelsior specie in the North and South site are equal
             #H1: The means of Volume of the Fraxinus excelsior specie in the North and South site are not equal
             
             
             # Welch Two Sample t-test
             t.test(FE_N$Volume, FE_S$Volume, alternative = "two.sided", var.equal = FALSE) 
             
             # Results:
             # data:  FE_N$Volume and FE_S$Volume
             # t = 9.9957, df = 34.524, p-value = 9.968e-12
             # alternative hypothesis: true difference in means is not equal to 0
             # 95 percent confidence interval:
             #  0.2709357 0.4091217
             # sample estimates:
             #  mean of x  mean of y 
             # 0.42751286 0.08748418            
             
             # Interpretation:
             # Since the p-value lies below the threshold of 0.05 we can conclude the the means of Fraxinus excelsior specie
             # in the North and South sites are not equal, therefore we can reject the null hypothesis at 5% significance level

## 2. Welch's t-test: to test the means of two datasets with unequal variances
             
             #H0: The means of Volume of Fraxinus excelsior specie in the North and South site are equal
             #H1: The means of Volume of Fraxinus excelsior species is greater in the North than in the South site
             
             
             # Welch Two Sample t-test
             t.test(FE_N$Volume, FE_S$Volume, alternative = "greater", var.equal = FALSE) 
             
             # data:  FE_N$Volume and FE_S$Volume
             # t = 9.9957, df = 34.524, p-value = 4.984e-12
             # alternative hypothesis: true difference in means is greater than 0
             # 95 percent confidence interval:
             #   0.2825323       Inf
             # sample estimates:
             #   mean of x  mean of y 
             # 0.42751286 0.08748418            
             
             # Interpretation:
             # Since the p-value lies below the threshold of 0.05 we can conclude the the mean Volume of Fraxinus excelsior specie
             # in the North Site is greater than in the South site, therefore we can reject the null hypothesis at 5% csignificance level
             
                          
## 3. Welch's t-test: to test the means of two datasets with unequal variances
             
             #H0: The means of Quercus robur specie in the North and South site are equal
             #H1: The means of Quercus robur species in the North and South site are not equal
             
             
             # Welch Two Sample t-test
             t.test(QR_N$Volume, QR_S$Volume, alternative = "two.sided", var.equal = FALSE) 
             
             # Results:
             # data:  QR_N$Volume and QR_S$Volume
             # t = 11.843, df = 36.389, p-value = 4.803e-14
             # alternative hypothesis: true difference in means is not equal to 0
             # 95 percent confidence interval:
             #  0.2215291 0.3130358
             # sample estimates:
             #  mean of x  mean of y 
             # 0.35912813 0.09184569
             
             # Interpretation:
             # Since the p-value lies below the threshold of 0.05 we can conclude the the means of Quercus robur specie
             # in the North and South sites are not equal, therefore we can reject the null hypothesis at 5% confidence level

## 4. Welch's t-test: to test the means of two datasets with unequal variances
             
             #H0: The means of Volume of the Quercus robur specie in the North and South site are equal
             #H1: The means of Volume of Quercus robur specie in the North site is greater than in the South
             
             
             # Welch Two Sample t-test
             t.test(QR_N$Volume, QR_S$Volume, alternative = "greater", var.equal = FALSE) 
             
             # Results:
             # data:  QR_N$Volume and QR_S$Volume
             # t = 11.843, df = 36.389, p-value = 2.401e-14
             # alternative hypothesis: true difference in means is greater than 0
             # 95 percent confidence interval:
             #   0.2291914       Inf
             # sample estimates:
             #   mean of x  mean of y 
             # 0.35912813 0.09184569
             
             # Interpretation:
             # Since the p-value lies below the threshold of 0.05 we can conclude the the mean Volume of Quercus robur specie
             # in the North site is greater than in the South site, therefore we can reject the null hypothesis at 5% confidence level

## 5. Mann-Whitney U test (Wilcoxon rank-sum test): to test the means of not normally distributed data
             
             # To compare the mean Volume of  Acer platanoides specie in North and South Sites
             # The shapiro wilk test above showed that the mean volume of Acer platanoides in the north site was not normally distributed,
             # meanwhile the mean volume of Acer platanoides in the south site was normally distributed
             

             #H0: The means of Volume of the Acer platanoides specie in the North and South sites are equal
             #H1: The means of Volume of the Acer platanoides specie in the North site is greater than in the South
             
             # Wilcoxon rank-sum test: since one variable is normally distributed and one is not
             suppressWarnings(
             wilcox.test(AP_N$Volume, AP_S$Volume, alternative = "greater")
             )
             # Results:
             # Wilcoxon rank sum test with continuity correction
             # 
             # data:  AP_N$Volume and AP_S$Volume
             # W = 486.5, p-value = 0.0001796
             # alternative hypothesis: true location shift is greater than 0
             
             # Interpretation:
             # Since the p-value lies below the threshold of 0.05 we can conclude the the means of Volume of the
             # Acer platanoides specie in the North site is greater than in the South, therefore we can reject the null hypothesis at 5% confidence level
             
                                      
## 6. Mann-Whitney U test (Wilcoxon rank-sum test): to test the means of not normally distributed data
             
             # To compare the means of Volumes in North and South Sites
             # The shapiro wilk test above showed that the volume in the north site was not normally distributed,
             # meanwhile the volume in the south site was normally distributed
             
             # We can also visualize the distributions of Volumes based on Sites (North and South) using a histogram
             par(mfrow=c(1,2)) 
             hist(north$Volume, main="Distribution of Volume in the North Site", xlab= expression("Mean Volume"~(m^3)), col="lightblue")
             hist(south$Volume, main="Distribution of Volume in the South Site", xlab= expression("Mean Volume"~(m^3)), col="lightgreen")
             
             #H0: The means of the volume in The North and South sites are equal
             #H1: The means of the volume in The North and South sites are not equal
             
             # Wilcoxon rank-sum test: since one variable is normally distributed and one is not
             wilcox.test(north$Volume, south$Volume, alternative = "two.sided")
             
             # Results:
             # Wilcoxon rank sum test with continuity correction
             
             # data:  north$Volume and south$Volume
             # W = 18614, p-value < 2.2e-16
             # alternative hypothesis: true location shift is not equal to 0
             
             # Interpretation:
             # Since the p-value lies below the threshold of 0.05 we can conclude the the means of Volume
             # in the North and South sites are not equal, therefore we can reject the null hypothesis at 5% confidence level

             
##7. Correlation testing: using Pearson correlation coefficient to test whether a correlation exists between the Mean Diameter of trees and their Volume
             
             # H0: There is no correlation between the mean diameter and the volume variables of a tree (r = 0).
             # H0: There exists a correlation between the mean diameter and the volume variables of a tree (r ≠ 0).
             cor.test(Trees$`Mean D.`,Trees$Volume, method = "pearson")
             
             # Results:
             #              Pearson's product-moment correlation
             # 
             # data:  Trees$`Mean D.` and Trees$Volume
             # t = 12.641, df = 291, p-value < 2.2e-16
             # alternative hypothesis: true correlation is not equal to 0
             # 95 percent confidence interval:
             #  0.5160068 0.6646335
             # sample estimates:
             #       cor 
             # 0.5953901
           
             # Interpretation:
             # Since the p value lies below the threshold of 0.05 we can reject the null hypothesis that no correlation
             # exists between the mean diameter of trees and their volume. The value of 0.59 indicates that the two variables 
             # are have a moderate positive correlation


##8. Chi-square Test of Independence: to check if there is an association between health status of trees and the site that they are in (North or South)
             
             #H0: Health status of trees is independent of the site that they are in
             #H1: Health status of trees and the site that they are in are dependent
            
             #Chi-square test of independence
              suppressWarnings(
             chisq.test(table(Trees_combined$`Health Status`, Trees_combined$Region)))
            
             # Results:
             # Pearson's Chi-squared test with Yates' continuity correction
             # 
             # data:  table(Trees_combined$`Health Status`, Trees_combined$Region)
             # X-squared = 0.92069, df = 1, p-value = 0.3373 
              
             # Interpretation:
             # Since the p value does not lie below the threshold of 0.05, we cannot reject the null hypothesis,
             # meaning that the Health Status of trees and the Site that they are in are independent from each other at 5% significance level
              
