library(data.table)
library(dplyr)
library(flexclust)

#Bring in data
properties <- fread(input = "https://s3.us-east-2.amazonaws.com/aws-emr-dedicated/data/zillow/properties_2016.csv", 
                    na.strings = "") 
###gradient boosting!
## convert lat/lon
properties <- properties %>%
  mutate(latitude = latitude/1e6, longitude = longitude/1e6)
train <- fread(input = "https://s3.us-east-2.amazonaws.com/aws-emr-dedicated/data/zillow/train_2016_v2.csv",
               na.strings = "")

imputed = read.csv('imputed.csv')
properties_sub = properties %>% select(.,structuretaxvaluedollarcnt, taxvaluedollarcnt, latitude, longitude,landtaxvaluedollarcnt, parcelid)
new = properties_sub %>% na.omit()
dim(new)
allData = new %>% 
  inner_join(train, by="parcelid")

#Selecting variables of interest to cluster against log-error
allData = allData %>% select(.,structuretaxvaluedollarcnt, taxvaluedollarcnt, latitude, longitude,landtaxvaluedollarcnt, logerror)

#Sampling 20,000 rows (so my computer doesn't crash)
sampled_Data = sample_n(allData, 20000)
#Scaling all sampled data
allData.scale = as.data.frame(scale(sampled_Data))

set.seed(0)
d = dist(allData.scale)


fit.average = hclust(d, method = "average")

#######
#Cut the dendrogram into groups of data.
clusters.average = cutree(fit.average, k = 5)
clusters.average
#Insert this column into dataframe and group_by nutrition

#Viewing the groups of data.
table(clusters.average)

#Aggregating the original data by the cluster assignments.
aggregate(sampled_Data, by = list(cluster = clusters.average), median)

###My Data
#Upload properties and train files
imputed = read.csv('imputed.csv')
properties2 = imputed %>% select(.,structuretaxvaluedollarcnt, landtaxvaluedollarcnt, parcelid)
properties2 %>% filter(.,landtaxvaluedollarcnt <= 0)
properties2$structuretaxvaluedollarcnt = properties2$structuretaxvaluedollarcnt + 1
properties2$landtaxvaluedollarcnt = properties2$landtaxvaluedollarcnt + 1
colMeans(is.na(properties2)) #Making sure that all NA values are taken care of
properties2.scale = as.data.frame(scale(properties2))
allData = properties2 %>% 
  inner_join(train, by="parcelid")
allData_subset = allData %>% select(.,structuretaxvaluedollarcnt, landtaxvaluedollarcnt, logerror)
allData_sub = sample_n(allData_subset, 30000)
allData.scale = as.data.frame(scale(allData_sub))
sapply(allData.scale, sd)
set.seed(0)

d = dist(allData.scale)

fit.average = hclust(d, method = "average")

##Further investigation

#Conducting the K-Means algorithm on the scaled, partial dataset
set.seed(0)
km.zillow = kmeans(allData.scale, centers = 5)

#Inspecting the output of the kmeans() function.
km.zillow
km.zillow$tot.withinss

#Visualizing the results against the truth.
par(mfrow = c(1, 1))
plot(allData.scale$structuretaxvaluedollarcnt, allData.scale$logerror,
     xlab = "structure_tax_value_dollar_cnt", ylab = "logerror",
     main = "Single K-Means Attempt", col = km.zillow$cluster)