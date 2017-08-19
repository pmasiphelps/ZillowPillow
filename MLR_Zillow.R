library(data.table)
library(dplyr)
library(e1071)
library(datasets)
library(rcompanion)
library(car)
imputed = fread('imputed.csv')
head(imputed)
colnames(imputed)
sapply(imputed, class)

#both imputed and train parcelid must be in integer form before merge
imputed$parcelid = as.integer(imputed$parcelid)

train <- fread(input = "https://s3.us-east-2.amazonaws.com/aws-emr-dedicated/data/zillow/train_2016_v2.csv",
               na.strings = "")
allData = imputed %>% 
  inner_join(train, by="parcelid")
head(allData)

#removing certain variables before modeling
#Variables removed are those at would not make as much sense in a linear regression
allData1 = allData %>% select(.,-parcelid, -V1, -airconditioningtypeid, -buildingqualitytypeid
                              -heatingorsystemtypeid, -heatingorsystemtypeid, propertycountylandusecode, -regionidzip, -regionidcounty, -transactiondate, -regionidcity)
allData1$yearbuilt = as.numeric(allData1$yearbuilt)
allData1$taxdelinquencyflag = as.factor(allData1$taxdelinquencyflag)
allData1$taxdelinquencyyear = as.factor(allData1$taxdelinquencyyear)
allData1$propertycountylandusecode = as.factor(allData1$propertycountylandusecode)
allData1$poolcnt = as.numeric(allData1$poolcnt)
allData1$numberofstories = as.numeric(allData1$numberofstories)

head(allData1)
colnames(allData1)
sapply(allData1, class)

#####Transformation of variables#####

#predictors with a right tail: bathroomcnt, calculatedfinishedsqft, fullbathcnt, lotsizesquarefeet
#structuretaxvaluedollarcnt, landtaxvaluedollarcnt
#predictors with a left tail: longitude

#logerror
logerror_plus = allData1$logerror + 5 #to eliminate negative values
logerror.trans = sqrt(logerror_plus)
plotNormalHistogram(logerror.sqrt)

#bathroomcnt
hist(allData1$bathroomcnt)
skew.score <- function(c, x) (skewness(log(x + c)))^2
cval <- seq(0, 20, l = 101)
skew <- cval * 0
for (i in 1:length(cval)) skew[i] <- skewness(log(cval[i] + allData1$bathroomcnt))
plot(cval, skew, type = "l", ylab = expression(b[3](c)), xlab = expression(c))
abline(h = 0, lty = 3)
best.c <- optimise(skew.score, c(0, 20), x = allData$bathroomcnt)$minimum
best.c #2.146665
bathroomcnt.trans <- log(allData1$bathroomcnt + best.c)
plotNormalHistogram(bathroomcnt.trans, col = "azure")
plotNormalHistogram(allData1$bathroomcnt)
qqnorm(bathroomcnt.trans)
qqline(bathroomcnt.trans, 
       col="red")

#calculatedfinishedsquarefeet
plotNormalHistogram(allData1$calculatedfinishedsquarefeet)
calculatedfinishedsquarefeet.trans = log(allData1$calculatedfinishedsquarefeet)
plotNormalHistogram(calculatedfinishedsquarefeet.trans)

#fullbathcnt
allData1$fullbathcnt <- ifelse(allData1$fullbathcnt < 0, 0, allData1$fullbathcnt)
allData1$fullbathcnt = allData1$fullbathcnt + 1
plotNormalHistogram(allData1$fullbathcnt)

skew.score <- function(c, x) (skewness(log(x + c)))^2
cval <- seq(0, 20, l = 101)
skew <- cval * 0
for (i in 1:length(cval)) skew[i] <- skewness(log(cval[i] + allData1$fullbathcnt))
plot(cval, skew, type = "l", ylab = expression(b[3](c)), xlab = expression(c))
abline(h = 0, lty = 3)
best.c <- optimise(skew.score, c(0, 20), x = allData$fullbathcnt)$minimum
best.c #6.685315
fullbathcnt.trans <- log(allData1$fullbathcnt + best.c)
plotNormalHistogram(fullbathcnt.trans, col = "azure")

qqnorm(fullbathcnt.trans)
qqline(fullbathcnt.trans, 
       col="red")

#lotsizesquarefeet
plotNormalHistogram(allData1$lotsizesquarefeet)
summary(allData1$lotsizesquarefeet)
lotsizesquarefeet.trans = (allData1$lotsizesquarefeet)^(1/3)
plotNormalHistogram(lotsizesquarefeet.trans)

#structuretaxvaluedollarcnt
plotNormalHistogram(allData1$structuretaxvaluedollarcnt)
structuretaxvaluedollarcnt.trans = (allData1$structuretaxvaluedollarcnt)^(1/3)
plotNormalHistogram(structuretaxvaluedollarcnt.trans)

#landtaxvaluedollarcnt
plotNormalHistogram(allData1$landtaxvaluedollarcnt)
landtaxvaluedollarcnt.trans = (allData1$landtaxvaluedollarcnt)^(1/3)
plotNormalHistogram(landtaxvaluedollarcnt.trans)

#longitude
plotNormalHistogram(allData1$longitude)
longitude_plus = allData1$longitude + 120
longitude.trans = (longitude_plus)^(1.75)
plotNormalHistogram(longitude.trans)
qqnorm(longitude.trans)
qqline(longitude.trans, 
       col="red")

###Create new dataframe with previous + transformed data###
colnames(allData1)
logerror.scaled = scale(logerror.trans)
bathroomcnt.scaled = scale(bathroomcnt.trans)
calculatedfinishedsquarefeet.scaled = scale(calculatedfinishedsquarefeet.trans)
fullbathcnt.scaled = scale(fullbathcnt.trans)
lotsizesquarefeet.scaled = scale(lotsizesquarefeet.trans)
structuretaxvaluedollarcnt.scaled = scale(structuretaxvaluedollarcnt.trans)
landtaxvaluedollarcnt.scaled = scale(landtaxvaluedollarcnt.trans)
longitude.scaled = scale(longitude.trans)
latitude.scaled = scale(allData1$latitude)
bedroomcnt.scaled = scale(allData1$bedroomcnt)
unitcnt.scaled = scale(allData1$unitcnt)
garagecnt.scaled = scale(allData1$garagecarcnt)
numberofstories.scaled = scale(allData1$numberofstories)

allData.scaled = data.frame(logerror.scaled, bathroomcnt.scaled, calculatedfinishedsquarefeet.scaled,
                           lotsizesquarefeet.scaled, structuretaxvaluedollarcnt.scaled, 
                           landtaxvaluedollarcnt.scaled, longitude.scaled, latitude.scaled, bedroomcnt.scaled,
                           unitcnt.scaled, garagecnt.scaled, numberofstories.scaled)

###Run the model###
#running basic multiple linear regression model
model.transformed = lm(logerror.scaled ~ ., data = allData.scaled)

summary(model.transformed) #Many predictor variables are not significant, yet the
#overall regression is significant.

plot(model.transformed) #Assessing the assumptions of the model.

######Changing the number of variables#####
model.empty = lm(logerror.scaled ~ 1, data = allData.scaled) #The model with an intercept ONLY.
model.full = lm(logerror.scaled ~ ., data = allData.scaled) #The model with ALL variables.
scope = list(lower = formula(model.empty), upper = formula(model.full))
backwardAIC = step(model.transformed, scope, direction = "backward", k = 3)

summary(backwardAIC)
plot(backwardAIC)
influencePlot(backwardAIC)
vif(backwardAIC)
avPlots(backwardAIC)
confint(backwardAIC)
