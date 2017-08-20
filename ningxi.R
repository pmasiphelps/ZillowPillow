setwd("~/Documents/DSA/projects/zillow")
library(data.table)
trainset <- read.csv("train_2016_v2.csv")
properties = fread('properties_2016.csv')
properties = as.data.frame(properties)
library(dplyr)
sample = read.csv("~/Documents/DSA/projects/zillow/sample_submission.csv")
testset = read.csv("~/Documents/DSA/projects/zillow/imputed.csv")

testset = left_join(testset,trainset)
testset = sample_n(testset,10000)
testset$transactiondate=NULL
#save(testset, file = 'testset.Rda')
load('testset.Rda')

ningxi.var = properties %>% select(censustractandblock,rawcensustractandblock,fips,parcelid,yearbuilt,
buildingqualitytypeid,heatingorsystemtypeid,airconditioningtypeid,numberofstories,storytypeid,poolcnt,
pooltypeid7,pooltypeid2,pooltypeid10,poolsizesum)

#save(ningxi.var, file = "zillow_working.Rda")
load('zillow_working.Rda')

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

#properties['poolcnt','pooltypeid10','storytypeid'] = NULL 

library(Hmisc)
set.seed(0)
properties$longitude = impute(properties$longitude, "random")

set.seed(0)
properties$latitude = impute(properties$latitude, "random")
properties$fips = impute(properties$regionidcounty)

# to impute with VIM with means
library(VIM)
imputed.vim = ningxi.var %>% select(buildingqualitytypeid,heatingorsystemtypeid,
                                    airconditioningtypeid,numberofstories,poolsizesum) %>% impute(mean)

# to impute with caret with preprocessing based on longtitude and latitude
library(caret)
# longlat = properties %>% select(longitude,latitude,fips,rawcensustractandblock,yearbuilt)
# preproc = preProcess(longlat, method = 'knnImpute',
#                      k = as.integer(sqrt(nrow(properties))))
#to.impute = ningxi.var %>% select(fips,rawcensustractandblock,yearbuilt)
predicted = predict(preproc,longlat)
names(properties)
unique(properties$regionidcounty)
# fips.census = properties %>% select(fips,censustractandblock,rawcensustractandblock,regionidneighborhood)
# preproc = preProcess(fips.census, )

names(testset)

lr1 = lm(logerror ~ landtaxvaluedollarcnt + bathroomcnt + bedroomcnt + 
          hashottuborspa + taxdelinquencyflag + structuretaxvaluedollarcnt + 
          garagecarcnt + landtaxvaluedollarcnt + buildingqualitytypeid +
          pooltypeid2 + pooltypeid7 + yearbuilt + 
          airconditioningtypeid + heatingorsystemtypeid, data = testset) 
summary(lr1)
# yearbuilt, airconditioningtypeid, pooltypeid2, buildingqualitytypeid, garagecarcnt have 
# large p-values. lr2 replaces these variables with regionidcounty,
# fips (a location-related variablre), and calculatedfinishedsquarefeet
lr2 = lm(logerror ~ landtaxvaluedollarcnt + bathroomcnt + bedroomcnt + 
           hashottuborspa + taxdelinquencyflag + structuretaxvaluedollarcnt + regionidcounty +
           landtaxvaluedollarcnt + buildingqualitytypeid + fips + pooltypeid7 + 
           calculatedfinishedsquarefeet + heatingorsystemtypeid, data = testset) 
summary(lr2)
library(car)
vif(lr1)
vif(lr2)
# Not surprisingly, calculatedfinishedsquarefeet has a high VIF, north of 3.66.
# regionidcounty and fips are correlated and both have high VIFs, and so does bathroomcnt
# and bedroomcnt. Consider dropping these room count variables if we include square footage
plot(lr2) # residuals aren't normally distributed or homoskedastic. 
lr3 = lm(logerror ~ landtaxvaluedollarcnt +
           hashottuborspa + taxdelinquencyflag +
           landtaxvaluedollarcnt + buildingqualitytypeid + fips + 
           calculatedfinishedsquarefeet + heatingorsystemtypeid, data = testset) 
summary(lr3)c
vif(lr3)
influencePlot(lr3) # extreme outliers exist
anova(lr3, lr1) 
AIC(lr1, lr2, lr3)

# Since OLS didn't work so well, trying linear models with regularization

grid = 10^seq(10,-10,length=100)
library(glmnet)
x = model.matrix(lr3) # creates a new matrix with logerror as the intercept and dropping the intercept term
y = testset$logerror
ridgemodels = glmnet(x, y, alpha = 0, lambda = grid) #fitting 100 ridge models

# using 90% of testset data to cross validate with 10-fold to find best lambda 
set.seed(0)
test.train = sample(1:nrow(testset), .9*nrow(testset))
set.seed(0)
cv.ridge = cv.glmnet(x[test.train,], y[test.train], lambda = grid, alpha = 0, nfolds = 10)
predicted = predict.cv.glmnet(cv.ridge, s = 'lambda.min', newx = x[-test.train,]) # fit chosen lambda on test set  
mean((predicted - y[-test.train])^2) # MSE is 0.02614972
cv.ridge$lambda.min # best lambda is 0.00475081
predict(ridgemodels, type = "coefficients", s = cv.ridge$lambda.min)

predicted.all = predict.cv.glmnet(cv.ridge, s = 'lambda.min', newx = x) # fit chosen lambda on whole set
mean((predicted.all - y)^2) # MSE is 0.02587926

lassomodels = glmnet(x, y, alpha = 1, lambda = grid) #fitting 100 lasso models
cv.lasso = cv.glmnet(x[test.train,], y[test.train], lambda = grid, alpha = 1, nfolds = 10)

predicted.all.lasso = predict.cv.glmnet(cv.lasso, s = 'lambda.min', newx = x) # fit chosen lambda on whole set
mean((predicted.all.lasso - y)^2) # MSE is 0.02587941

# choosing the best model for glm
chosen = glmnet(x, y, alpha = 0, lambda = cv.ridge$lambda.min)