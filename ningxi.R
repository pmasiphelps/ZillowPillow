setwd("~/Documents/DSA/projects/zillow")
library(data.table)
train <- read.csv("~/Documents/DSA/projects/zillow/train_2016_v2.csv")
properties = fread('properties_2016.csv')
properties = as.data.frame(properties)
library(dplyr)
sample = read.csv("~/Documents/DSA/projects/zillow/sample_submission.csv")

ningxi.var = properties %>% select(censustractandblock,rawcensustractandblock,fips,parcelid,yearbuilt,
buildingqualitytypeid,heatingorsystemtypeid,airconditioningtypeid,numberofstories,storytypeid,poolcnt,
pooltypeid7,pooltypeid2,pooltypeid10,poolsizesum)

# save(ningxi.var, file = "zillow_working.Rda") - how does this work?

colMeans(is.na(ningxi.var))*100
# censustractandblock rawcensustractandblock        fips               parcelid              yearbuilt 
# 2.5166010              0.3831212               0.3831212            0.0000000              2.0074923 
# buildingqualitytypeid  heatingorsystemtypeid  airconditioningtypeid numberofstories      storytypeid 
# 35.0637491             39.4884526             72.8154101             77.1517782           99.9455986 
# poolcnt                pooltypeid7            pooltypeid2           pooltypeid10        poolsizesum 
# 82.6634379             83.7378991             98.9255387             98.7626025          99.0633847 

sum(!is.na(ningxi.var$pooltypeid7)) # 485459, Pool without Hot Tub
sum(!is.na(ningxi.var$pooltypeid2)) # 32075, Pool with Spa/Hot Tub
sum(!is.na(ningxi.var$poolcnt)) # 517534 (= pooltypeid7 + pooltypeid2)

# 1. Seems like we decided to toss poolcnt as pooltypeid7 and pooltypeid2 sum to poolcnt 
# and provide a more granular breakdown
# 2. censustractandblock is a series of numbers starting with '61100' and rawcensustractandblock
# starting with '603.' Since rawcensustractandblock has a smaller percentage of missingness I
# suggest we toss censustractandblock and keep rawcensustractandblock if we decide to keep one.
# 3. fips only has 3 unique values: 6037, 6059, and 6111 (not counting NAs)
# 4. pooltypeid10 - refers to spa or hot tub. There's another very similar feature. Keep or toss?
# 5. I feel most of these missing features are not completely missing at random, so using random
# imputation may not be the best option

properties['poolcnt','pooltypeid10','storytypeid'] = NULL # didn't run this yet

library(Hmisc)
set.seed(0)
properties$longitude = impute(properties$longitude, "random")

set.seed(0)
properties$latitude = impute(properties$latitude, "random")

# to impute with VIM with means
library(VIM)
imputed.vim = ningxi.var %>% select(buildingqualitytypeid,heatingorsystemtypeid,
                                    airconditioningtypeid,numberofstories,poolsizesum) %>% impute(mean)

# if that doesn't work, try:
# imputed.vim = impute(df,mean)

# to impute with caret with preprocessing based on longtitude and latitude
library(caret)
longlat = properties %>% select(longitude,latitude,fips,rawcensustractandblock,yearbuilt)
preproc = preProcess(longlat, method = 'knnImpute',
                     k = as.integer(sqrt(nrow(properties))))
to.impute = ningxi.var %>% select(fips,rawcensustractandblock,yearbuilt)
predicted = predict(preproc,to.impute)
