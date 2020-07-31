# install readxl library package as it is necessary to read xlsx  file in R programming
install.packages("readxl")
# Every time we have to call library whenever we have to use function defined inside it
library(readxl)

# Below code is to read data from xlsx file
hospital <- read_xlsx("C:/Users/shekh/Desktop/DataScience/Project 7 data sets/1555054100_hospitalcosts.xlsx")
head(hospital)

# Dataset Description:
# Here is a detailed description of the given dataset:


# Attribute	   Description
# Age 	       Age of the patient discharged
# Female       A binary variable that indicates if the patient is female
# Los	         Length of stay in days
# Race 	       Race of the patient (specified numerically)
# 
# Totchg	     Hospital discharge costs
# Aprdrg	     All Patient Refined Diagnosis Related Groups

# Analysis to be done: 

# 1. To record the patient statistics, the agency wants to find the age category of people who frequent
#    the hospital and has the maximum expenditure.

# a.find the age category of people who frequent the hospital




summary(as.factor(hospital$AGE))

# OUTPUT ::
#   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17 
# 307  10   1   3   2   2   2   3   2   2   4   8  15  18  25  29  29  38 


# # As a result we can see the infant with AGE = 0, is frequent visitor in the hospital and frequency
#   of visit is 307
# 
# b.  the age category of people has the maximum expenditure.

library(dplyr)

df <- summarise(group_by(hospital,AGE), TotalCharge = sum(TOTCHG))
df
View(df)
rm(df)

arrange(df,desc(TotalCharge))[1,]

# OUTPUT ::
#   # A tibble: 1 x 2
#   AGE TotalCharge
# <dbl>       <dbl>
#   1     0      678118

# The age category of people whose expenditure is maximum is 0

# 
# 2. In order of severity of the diagnosis and treatments and to find out the expensive treatments,
#    the agency wants to find the diagnosis-related group that has maximum hospitalization and expenditure.

  # a.In order of severity of the diagnosis and treatments and to find out the expensive treatments,



library(dplyr)

df2 <- summarise(group_by(hospital,APRDRG ), TotChgAge = sum(TOTCHG))
df

arrange(df, desc(TotChgAge))
arrange(df2, desc(TotChgAge))[1,]

# OUTPUT ::
#   # A tibble: 1 x 2
#   APRDRG TotChgAge
# <dbl>     <dbl>
#   1    640    437978


#############################################################################################

# 
# # 3. To make sure that there is no malpractice, the agency needs to analyze if the
# race of the patient is related to the hospitalization costs.

str(hospital$RACE)
str(hospital$TOTCHG)

malpractice_analysis <- aov(TOTCHG~RACE, data = hospital)
summary(malpractice_analysis)


# 
# # OUTPUT ::
# #   summary(malpractice_analysis)
# # Df    Sum Sq  Mean Sq F value Pr(>F)
# # RACE          1 2.488e+06  2488459   0.164  0.686
# # Residuals   497 7.540e+09 15170268     
# 
# alpha = 0.5
# p-value = 0.686
# As p value is greater than alpha, so total cost is not related to race




##############################################################################################

# # 4. To properly utilize the costs, the agency has to analyze the severity of the hospital
#      costs by age and gender for the proper allocation of resources.


analyse_age <- aov(TOTCHG~AGE, data = hospital)
summary(analyse_age)
# Df    Sum Sq   Mean Sq F value  Pr(>F)   
# AGE           1 1.308e+08 130822234   8.787 0.00318 **
#   Residuals   498 7.414e+09  14887377                   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# 
# As from above analysis we are coming to conclusion that total charge is affected by
# age

analyse_gender <- aov(TOTCHG~FEMALE, data = hospital)
summary(analyse_gender)
# Df    Sum Sq  Mean Sq F value Pr(>F)
# FEMALE        1 2.734e+07 27337922   1.811  0.179
# Residuals   498 7.517e+09 15095177 

# From above analysis we are coming to conclusion that total charge is not affected by gender.

boxplot(TOTCHG ~ FEMALE, data = hospital)
# We can see through the boxplot the above scenario

###############################################################################################




# 5. Since the length of stay is the crucial factor for inpatients, the agency wants to find if the length of
#    stay can be predicted from age, gender, and race.
#

linear_model <- lm(LOS ~ AGE + FEMALE + RACE, data = hospital)
summary(linear_model)

# OUTPUT ::
#   
#   Call:
#   lm(formula = LOS ~ AGE + FEMALE + RACE, data = hospital)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -3.22  -1.22  -0.85   0.15  37.78 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.94377    0.39318   7.487 3.25e-13 ***
#   AGE         -0.03960    0.02231  -1.775   0.0766 .  
# FEMALE       0.37011    0.31024   1.193   0.2334    
# RACE        -0.09408    0.29312  -0.321   0.7484    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.363 on 495 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.007898,	Adjusted R-squared:  0.001886 
# F-statistic: 1.314 on 3 and 495 DF,  p-value: 0.2692



# From the above we are coming to conclusion that length of stay(LOS) is not affected by 
# age(AGE), gender and by RACE.


##############################################################################################





# 6. To perform a complete analysis, the agency wants to find the variable that mainly affects hospital costs.


charge_LOS <- aov(TOTCHG~LOS, data = hospital)
summary(charge_LOS)

# OUTPUT ::
# Df    Sum Sq   Mean Sq F value Pr(>F)    
# LOS           1 2.930e+09 2.930e+09   316.2 <2e-16 ***
#   Residuals   498 4.615e+09 9.266e+06                   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Total charge is affected by LOS(length of stay)


charge_gender <- aov(TOTCHG~FEMALE, data = hospital)
summary(charge_gender)

# OUTPUT ::
#   Df    Sum Sq  Mean Sq F value Pr(>F)
# FEMALE        1 2.734e+07 27337922   1.811  0.179
# Residuals   498 7.517e+09 15095177
#Gender does not effect the total hospitalization cost.


charge_age <- aov(TOTCHG~AGE, data = hospital)
summary(charge_age)

# OUTPUT ::
# Df    Sum Sq   Mean Sq F value  Pr(>F)   
# AGE           1 1.308e+08 130822234   8.787 0.00318 **
#   Residuals   498 7.414e+09  14887377                   
# ---
#   Signify. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Total charge is affected by age, but we can also analyze this by the help of plot.

charge_severity <- aov(TOTCHG ~ APRDRG, data = hospital)
summary(charge_severity)


# OUTPUT ::
# Df    Sum Sq   Mean Sq F value   Pr(>F)    
# APRDRG        1 8.216e+08 821627648   60.86 3.63e-14 ***
#   Residuals   498 6.723e+09  13500218                     
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# We can clearly see from above result that severity of diagnosis affects the the
# hospitalization charge.



# From above, we come to conclusion that hospitalization cost is affected by Length of stay(LOS) and severity of 
# diagnosis i.e(APRDRG). And little bit is affected by AGE.






