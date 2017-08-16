
library(dplyr)
library(Hmisc)
library(VIM)
library(caret)


### Load Data:
properties <- fread(input = "properties_2016.csv", 
                    na.strings = "", 
                    colClasses = list(character=c("parcelid", "airconditioningtypeid", "architecturalstyletypeid",
                                                  "buildingclasstypeid", "decktypeid", "fips", "hashottuborspa",
                                                  "heatingorsystemtypeid", "pooltypeid10", "pooltypeid2", "pooltypeid7",
                                                  "propertycountylandusecode", "propertylandusetypeid", 
                                                  "propertyzoningdesc", "rawcensustractandblock", "regionidcity", 
                                                  "regionidcounty", "regionidneighborhood", "regionidzip", "storytypeid", 
                                                  "typeconstructiontypeid", "fireplaceflag", "assessmentyear",
                                                  "taxdelinquencyflag", "censustractandblock"))) 
transactions <- fread(input = "train_2016_v2.csv", 
                      na.strings = "", 
                      colClasses = list(character=c("parcelid")))


### Convert to factors (keeping raw properties file unchanged):
factor_cols <- c('unitcnt', 'threequarterbathnbr', 
                 'hashottuborspa', 'regionidcounty', 
                 'regionidcity', 'propertylandusetypeid', 
                 'regionidzip','propertycountylandusecode', 
                 'propertyzoningdesc', 'heatingorsystemtypeid', 
                 'airconditioningtypeid', 'numberofstories', 
                 'fips')

imputed <- properties %>% 
  mutate_at(.funs = as.factor, 
            .vars = intersect(names(imputed), factor_cols))


### Imputing and converting to proper data types:
imputed <- imputed %>% 
  mutate(latitude = latitude/1e6, 
         longitude = longitude/1e6, 
         basementsqft = as.numeric(ifelse(is.na(basementsqft), 
                                           0, basementsqft)),
         lotsizesquarefeet = as.numeric(ifelse(is.na(lotsizesquarefeet), 
                                      0, lotsizesquarefeet)), 
         unitcnt = as.numeric(ifelse(is.na(unitcnt), 
                                      1, unitcnt)),
         calculatedfinishedsquarefeet = as.numeric(ifelse(is.na(calculatedfinishedsquarefeet), 
                                                          mean(calculatedfinishedsquarefeet, na.rm=TRUE), 
                                                          calculatedfinishedsquarefeet)), 
         bedroomcnt = as.numeric(ifelse(is.na(bedroomcnt), 
                                       median(bedroomcnt, na.rm=TRUE),
                                       bedroomcnt)),
         bathroomcnt = as.numeric(ifelse(is.na(bathroomcnt),
                                        median(bathroomcnt, na.rm=TRUE),
                                        bathroomcnt)),
         fullbathcnt = as.numeric(ifelse(is.na(fullbathcnt), 
                                        median(fullbathcnt, na.rm=TRUE),
                                        fullbathcnt)),
         threequarterbathnbr = as.numeric(ifelse(is.na(threequarterbathnbr),
                                     0, threequarterbathnbr)),
         hashottuborspa = as.factor(ifelse(is.na(hashottuborspa),
                                     0, hashottuborspa)),
         fips = as.factor(ifelse(is.na(fips), regionidcounty, fips)),
         regionidcounty = as.factor(ifelse(is.na(regionidcounty),
                                      impute(regionidcounty, "random"),
                                      regionidcounty)),
         regionidcity = as.factor(ifelse(is.na(regionidcity),
                                      impute(regionidcity, "random"),
                                      regionidcity)),
         regionidzip = as.factor(ifelse(is.na(regionidzip),
                                      impute(regionidzip, "random"),
                                      regionidzip)),
         latitude = as.numeric(ifelse(is.na(latitude),
                                      impute(latitude, "random"),
                                      latitude)),
         longitude = as.numeric(ifelse(is.na(longitude),
                                      impute(longitude, "random"),
                                      longitude)), 
         fips = as.numeric(ifelse(is.na(fips),
                                  impute(fips, "random"),
                                  fips)), 
         censustractandblock = as.factor(ifelse(is.na(censustractandblock), 
                                                impute(censustractandblock, "random"), 
                                                censustractandblock)), 
         rawcensustractandblock = as.numeric(ifelse(is.na(rawcensustractandblock), 
                                                    impute(rawcensustractandblock, "random"), 
                                                    rawcensustractandblock)), 
         fireplacecnt = as.numeric(ifelse(is.na(as.numeric(fireplacecnt)), 
                                         0, as.numeric(fireplacecnt))),
         garagetotalsqft = as.numeric(ifelse(is.na(as.numeric(garagetotalsqft)), 
                                         0, as.numeric(garagetotalsqft))),
         garagecarcnt = as.numeric(ifelse(is.na(as.numeric(garagecarcnt)), 
                                         0, as.numeric(garagecarcnt))),
         propertylandusetypeid = as.factor(ifelse(is.na(propertylandusetypeid), 
                                        0, propertylandusetypeid)),
         propertycountylandusecode = as.factor(ifelse(is.na(propertycountylandusecode), 
                                            0, propertycountylandusecode)),
         taxdelinquencyyear = year(ymd(as.Date(as.character(taxdelinquencyyear), 
                                                 "%y"))), 
         taxdelinquencyyear = as.factor(ifelse(is.na(taxdelinquencyyear), 
                                               0, taxdelinquencyyear)), 
         taxdelinquencyflag = as.factor(ifelse(is.na(taxdelinquencyflag), 
                                                  0, 1)), 
         pooltypeid7 = as.factor(ifelse(is.na(pooltypeid7), 
                                         0, pooltypeid7)),
         pooltypeid2 = as.factor(ifelse(is.na(pooltypeid2), 
                                         0, pooltypeid2)),
         poolsizesum = as.numeric(ifelse(is.na(as.numeric(poolsizesum)), 
                                         0, as.numeric(poolsizesum))),
         heatingorsystemtypeid = as.factor(ifelse(is.na(heatingorsystemtypeid), 
                                                  '13', heatingorsystemtypeid)),
         airconditioningtypeid = as.factor(ifelse(is.na(airconditioningtypeid), 
                                                  '5', airconditioningtypeid)),
         buildingqualitytypeid = as.numeric(ifelse(is.na(as.numeric(buildingqualitytypeid)), 
                                                   median(as.numeric(buildingqualitytypeid), na.rm=TRUE), 
                                                   as.numeric(buildingqualitytypeid))),
         numberofstories = as.factor(ifelse(is.na(as.numeric(numberofstories)), 
                                            median(as.numeric(numberofstories), na.rm=TRUE), 
                                            as.numeric(numberofstories))),
         yearbuilt = as.factor(ifelse(is.na(yearbuilt), 
                                      median(yearbuilt, na.rm=TRUE),
                                      yearbuilt))
         )

### zoning_property: 35.4% missing. suggest no imputing for missing values. include in random forest model but not OLS linear reg


### tax_building: (1) for properties with no building, just land (tax_building = NA and tax_land = tax_total): impute zero for tax_building
imputed$structuretaxvaluedollarcnt[(is.na(imputed$structuretaxvaluedollarcnt)) & 
                           !(is.na(imputed$landtaxvaluedollarcnt)) & 
                           (imputed$landtaxvaluedollarcnt == imputed$taxvaluedollarcnt)] <- 0

### tax_land: (1) for properties with no land (apartments?), just building (tax_land = NA and tax_building = tax_total): impute zero for tax_land
imputed$landtaxvaluedollarcnt[(is.na(imputed$landtaxvaluedollarcnt)) & 
                       !(is.na(imputed$structuretaxvaluedollarcnt)) & 
                       (imputed$structuretaxvaluedollarcnt == imputed$taxvaluedollarcnt)] <- 0



### tax_land and tax_building: (2) for properties with tax_property, but no assessment values (tax_land or tax_building)
### Step 1: get the median property tax rate for properties with no missing values
median_tax_rate <- (imputed %>% 
                      filter(!is.na(taxvaluedollarcnt) & !is.na(taxamount)) %>% 
                      mutate(tax_rate = taxamount/taxvaluedollarcnt) %>% 
                      summarise(median(tax_rate)))[[1]]
### median tax rate = .01269028

### Step 2: only 15 out of 23,902 have more than zero beds/baths, so we can impute a tax_land value as a function of 
### tax_property, and impute tax_building = 0
imputed$landtaxvaluedollarcnt[is.na(imputed$structuretaxvaluedollarcnt) & 
                                !(is.na(imputed$taxamount)) & 
                                is.na(imputed$landtaxvaluedollarcnt)] <- imputed$taxamount[is.na(imputed$structuretaxvaluedollarcnt) & 
                                                                                             !(is.na(imputed$taxamount)) & 
                                                                                             is.na(imputed$landtaxvaluedollarcnt)]/median_tax_rate


imputed$structuretaxvaluedollarcnt[is.na(imputed$structuretaxvaluedollarcnt) & 
                                     !(is.na(imputed$taxamount))] <- 0


### Step 1: create new columns with mean tax_building and mean tax_land by by zip, nbed, nbath

imputed <- imputed %>% 
  group_by(regionidzip, bedroomcnt, bathroomcnt) %>% 
  mutate(mean_zip_bb_tax_build = mean(structuretaxvaluedollarcnt, na.rm = TRUE)) %>% 
  ungroup

imputed <- imputed %>% 
  group_by(regionidzip, bedroomcnt, bathroomcnt) %>% 
  mutate(mean_zip_bb_tax_land = mean(landtaxvaluedollarcnt, na.rm = TRUE)) %>% 
  ungroup

### Step 2: impute the new column values for tax_building and tax_land

imputed$structuretaxvaluedollarcnt[is.na(imputed$structuretaxvaluedollarcnt) & 
                           is.na(imputed$taxamount)] <- imputed$mean_zip_bb_tax_build[is.na(imputed$structuretaxvaluedollarcnt) & 
                                                                                                   is.na(imputed$taxamount)]
imputed$landtaxvaluedollarcnt[is.na(imputed$landtaxvaluedollarcnt) & 
                       is.na(imputed$taxamount)] <- imputed$mean_zip_bb_tax_land[is.na(imputed$landtaxvaluedollarcnt) & 
                                                                                              is.na(imputed$taxamount)]



imputed <- imputed %>% 
  mutate(landtaxvaluedollarcnt = as.numeric(ifelse(is.na(landtaxvaluedollarcnt), 
                                                   median(landtaxvaluedollarcnt, na.rm=TRUE), 
                                                   landtaxvaluedollarcnt)),
         structuretaxvaluedollarcnt = as.numeric(ifelse(is.na(structuretaxvaluedollarcnt), 
                                                        median(structuretaxvaluedollarcnt, na.rm=TRUE), 
                                                        structuretaxvaluedollarcnt))
         )


### Drop columns:
drops <- c('decktypeid', 'finishedfloor1squarefeet', 
           'finishedsquarefeet6', 'finishedsquarefeet12', 
           'finishedsquarefeet13', 'finishedsquarefeet15', 
           'finishedsquarefeet50', 'yardbuildingsqft17', 
           'yardbuildingsqft26', 'calculatedbathnbr', 
           'assessmentyear', 'taxvaluedollarcnt', 
           'poolcnt', 'pooltypeid10', 'storytypeid', 
           'architecturalstyletypeid', 'buildingclasstypeid', 
           'fireplaceflag', 'roomcnt', 'regionidneighborhood', 
           'typeconstructiontypeid', 'propertyzoningdesc', 
           'taxamount', 'mean_zip_bb_tax_build', 
           'mean_zip_bb_tax_land')

imputed <- imputed[ , !(names(imputed) %in% drops)]

summary(imputed)

write.csv(imputed, file = 'imputed.csv')

### Create train data set: transactions inner_join imputed: 
train <- transactions %>% 
  mutate(year = year(transactiondate),
         month = month(transactiondate)) %>%
  select(-transactiondate) %>%
  inner_join(imputed, by = 'parcelid') 


write.csv(train, file = 'train.csv')

# rename variables (optional)
imputed <- imputed %>% rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet, 
  area_garage = garagetotalsqft,
  area_total_calc = calculatedfinishedsquarefeet,
  num_unit = unitcnt, 
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bath = fullbathcnt,  
  num_75_bath = threequarterbathnbr, 
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,  
  tax_building = structuretaxvaluedollarcnt,
  tax_land = landtaxvaluedollarcnt,
  tax_delinquency = taxdelinquencyflag,
  tax_delinquency_year = taxdelinquencyyear,
  zoning_property = propertyzoningdesc,
  zoning_landuse = propertylandusetypeid,
  zoning_landuse_county = propertycountylandusecode,
  flag_fireplace = fireplaceflag, 
  flag_tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style= architecturalstyletypeid
  )

transactions <- transactions %>% rename(
  id_parcel = parcelid,
  date = transactiondate
  )





