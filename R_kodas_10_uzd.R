#----------------------------------------------------------------------------
# 2 Užduotis
#----------------------------------------------------------------------------

install.packages("haven")
library(haven)
dataset=read_sas("C:/Users/Klaudija/Desktop/KTU/R/donate.sas7bdat")

## Ar yra sąlyginių išskirčių?

for(i in 2:length(dataset)){
  lower_bound <- quantile(dataset[i], 0.25,na.rm=T)
  upper_bound <- quantile(dataset[i], 0.75,na.rm=T)
  IQR=upper_bound -lower_bound
  find=which((dataset[i]>=lower_bound -3*IQR & dataset[i]<lower_bound-1.5*IQR) | 
               (dataset[i]>upper_bound+1.5*IQR & dataset[i]<=upper_bound+3*IQR))
  b=dataset[find,i]
  print(b)
}

## Ar yra grubių išskirčių?

# 1 kintamasis
lower_bound1 <- quantile(dataset$Qtr1, 0.25,na.rm=T)
upper_bound1 <- quantile(dataset$Qtr1, 0.75,na.rm=T)

outlier_ind1 <- which(dataset$Qtr1 < lower_bound1 -3*IQR(dataset$Qtr1, na.rm = TRUE) | dataset$Qtr1  > upper_bound1+3*IQR(dataset$Qtr1, na.rm = TRUE))
outlier_ind1

# 2 kintamasis
lower_bound2 <- quantile(dataset$Qtr2, 0.25,na.rm=T)
upper_bound2 <- quantile(dataset$Qtr2, 0.75,na.rm=T)

outlier_ind2 <- which(dataset$Qtr2 < lower_bound2 -3*IQR(dataset$Qtr2, na.rm = TRUE) | dataset$Qtr2  > upper_bound2+3*IQR(dataset$Qtr2, na.rm = TRUE))
outlier_ind2

# 3 kintamasis
lower_bound3 <- quantile(dataset$Qtr3, 0.25,na.rm=T)
upper_bound3 <- quantile(dataset$Qtr3, 0.75,na.rm=T)

outlier_ind3 <- which(dataset$Qtr3 < lower_bound3 -3*IQR(dataset$Qtr3, na.rm = TRUE) | dataset$Qtr3  > upper_bound3+3*IQR(dataset$Qtr3, na.rm = TRUE))
outlier_ind3

# 4 kintamasis
lower_bound4 <- quantile(dataset$Qtr4, 0.25,na.rm=T)
upper_bound4 <- quantile(dataset$Qtr4, 0.75,na.rm=T)

outlier_ind4 <- which(dataset$Qtr4 < lower_bound4 -3*IQR(dataset$Qtr4, na.rm = TRUE) | dataset$Qtr4  > upper_bound4+3*IQR(dataset$Qtr4, na.rm = TRUE))
outlier_ind4

# Grubias pašalinti
clean_dataset <- dataset[-outlier_ind4, ]
