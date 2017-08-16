properties = read.csv('prop.csv')
library(dplyr)
library(Hmisc)
sample = properties[sample(nrow(properties), 10000), ]
subset = sample %>% select(.,bedroomcnt,bathroomcnt,fullbathcnt,threequarterbathnbr,
                           hashottuborspa,regionidcounty,regionidcity,regionidzip,regionidneighborhood,latitude, longitude)
aggr(subset) #to visualize (roughly) missingness values
                           
sum(is.na(properties$bathroomcnt))/nrow(properties) #visualize each variable's missingness precisely

###Katie's predictors
#calculatedbathnbr- removed. This variable is redundant with bathroomcnt
#bedroomcnt
#bathroomcnt
#fullbathcnt- imputed below, but still consider dropping; this information is very similar to the
  #information on bathroom count, except that only "full" bathrooms are considered
#threequarterbathnbr- consider dropping; not highly impactful predictor
#hashottuborspa
#regionidcounty
#regionidcity
#regionidzip
#regionidneighborhood- removed
#latitude
#longitude
#architecturestypetypeid- removed
#typeconstructiontypeid- removed
#buildingclasstypeid- removed

###Katie's imputations (or predictor removals) below

#remove calculatedbathnbr; it is redundant with bathroomcnt

#Imputation for bedroomcnt (.38% missing)
properties$bedroomcnt[is.na(properties$bedroomcnt)] = mean(properties$bedroomcnt, na.rm=TRUE)

#Imputation for bathroomcnt
properties$bathroomcnt[is.na(properties$bathroomcnt)] = mean(properties$bathroomcnt, na.rm=TRUE)

#Imputation for fullbathcount (potentially remove later; highly redundant with bathroomcnt) 
properties$fullbathcnt[is.na(properties$fullbathcnt)] = mean(properties$fullbathcnt, na.rm=TRUE)

#Threequarterbathnbr
properties$threequarterbathnbr = ifelse(is.na(properties$threequarterbathnbr),0,1)

#Adjustment of hottub values; none missing now
properties$hashottuborspa <- sub("^$", "0", properties$hashottuborspa)
properties$hashottuborspa[properties$hashottuborspa != 0] <- 1

#Imputation for regionidcounty
set.seed(0)
properties$regionidcounty = impute(properties$regionidcounty, "random")

#Imputation for regionidcity
set.seed(0)
properties$regionidcity = impute(properties$regionidcity, "random")

#Imputation for regionidzip
set.seed(0)
properties$regionidzip = impute(properties$regionidzip, "random")

#regionidneighborhood has over 60% missingness and has been dropped

#latitude
set.seed(0)
properties$latitude = impute(properties$latitude, "random")

#longitude
set.seed(0)
properties$longitude = impute(properties$longitude, "random")




