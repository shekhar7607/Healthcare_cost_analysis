# install readxl library package as it is necessary to read xlsx  file in R programming
install.packages("readxl")
library(readxl)
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


# 
# # 3. To make sure that there is no malpractice, the agency needs to analyze if the
# race of the patient is related to the hospitalization costs.

str(hospital$RACE)
str(hospital$TOTCHG)

malpractice_analysis <- aov(TOTCHG~RACE, data = hospital)
summary(malpractice_analysis)
cor(hospital$TOTCHG,hospital$RACE)

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


# # 4. To properly utilize the costs, the agency has to analyze the severity of the hospital
#      costs by age and gender for the proper allocation of resources.


cor(hospital$TOTCHG, hospital$AGE)
# [1] 0.1316797 , from here we come to conclusion that age is not effecting the total cost.

cor(hospital$TOTCHG,hospital$FEMALE)
# [1] -0.06019504
# It seems that gender also does not effect the hospitalization






# 5. Since the length of stay is the crucial factor for inpatients, the agency wants to find if the length of
#    stay can be predicted from age, gender, and race.
# 
# 6. To perform a complete analysis, the agency wants to find the variable that mainly affects hospital costs.


cor(hospital$TOTCHG,hospital$LOS)
# [1] 0.623193


cor(hospital$TOTCHG,hospital$APRDRG)
# [1] -0.3300012


cor(hospital$TOTCHG,hospital$AGE)
# [1] 0.1316797

cor(hospital$TOTCHG,hospital$FEMALE)
# [1] -0.06019504

# From cor we came to the conclusion that hospitalisation cost mainly depend on 
# LOS(length of stay)


