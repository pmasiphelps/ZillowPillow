#######################################
####   MASTER DATA CLEANING FILE   ####
#######################################

####   Cleaning up properties dataset   ####

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(datetime)

# rename variables (optional)
properties <- properties %>% rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26, 
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet, 
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_entry_floor = finishedsquarefeet50,
  num_unit = unitcnt, 
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,  
  num_75_bath = threequarterbathnbr, 
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,  
  tax_total = taxvaluedollarcnt,
  tax_building = structuretaxvaluedollarcnt,
  tax_land = landtaxvaluedollarcnt,
  tax_property = taxamount,
  tax_year = assessmentyear,
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
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style= architecturalstyletypeid
)

transactions <- train %>% rename(
  id_parcel = parcelid,
  date = transactiondate
)

properties1 <- properties

###############
###   Pat   ###
###############

### flag_fireplace: high missingness, useless. filter out

### tax_year: all 2015, useless. filter out

### tax_property: amount of taxes charged on property. highly correlated with tax assessment variables

### num_fireplace: 90% missing at random. difficult to make informed imputation. filter out

### area_garage: impute zero for NAs
properties1$area_garage[is.na(properties1$area_garage)] <- 0

### num_garage: impute zero for NAs
properties1$num_garage[is.na(properties1$num_garage)] <- 0

### zoning_property: 35.4% missing. suggest no imputing for missing values. include in random forest model but not OLS linear reg
properties1$zoning_property <- factor(properties1$zoning_property)

### zoning_landuse: no missing values. change to factor
properties1$zoning_landuse <- factor(properties1$zoning_landuse)

### zoning_landuse_county: no missing values. change to factor
properties1$zoning_landuse_county <- factor(properties1$zoning_landuse_county)

### tax_delinquency_year: change from 2 digit year to 4 digit year in date format.
### suggest include in random forest model, not linear reg
properties1$tax_delinquency_year <- as.Date(as.character(properties1$tax_delinquency_year), "%y")
properties1$tax_delinquency_year <- ymd(properties1$tax_delinquency_year)
properties1$tax_delinquency_year <- year(properties1$tax_delinquency_year)

### tax_delinquency: change to 1/0 dummy variable
properties1$tax_delinquency <- ifelse(properties1$tax_delinquency=="Y",1,0)
properties1$tax_delinquency[is.na(properties1$tax_delinquency)] <- 0
properties1$tax_delinquency <- factor(properties1$tax_delinquency)

### tax_building: (1) for properties with no building, just land (tax_building = NA and tax_land = tax_total): impute zero for tax_building
properties1$tax_building[(is.na(properties1$tax_building)) & 
                           !(is.na(properties$tax_land)) & 
                           (properties$tax_land == properties$tax_total)] <- 0

### tax_land: (1) for properties with no land (apartments?), just building (tax_land = NA and tax_building = tax_total): impute zero for tax_land
properties1$tax_land[(is.na(properties1$tax_land)) & 
                       !(is.na(properties$tax_building)) & 
                       (properties$tax_building == properties$tax_total)] <- 0

### tax_land and tax_building: (2) for properties with tax_property, but no assessment values (tax_land or tax_building)
### Step 1: get the median property tax rate for properties with no missing values
median_tax_rate <- (properties1 %>% filter(!is.na(tax_total) & !is.na(tax_property)) %>% 
                      mutate(tax_rate = tax_property/tax_total) %>% 
                      summarise(median(tax_rate)))[[1]]
### median tax rate = .01269028

### Step 2: only 15 out of 23,902 have more than zero beds/baths, so we can impute a tax_land value as a function of 
### tax_property, and impute tax_building = 0

properties1$tax_land[is.na(properties1$tax_building) & 
                       !(is.na(properties1$tax_property)) & 
                       is.na(properties1$tax_land)] <- properties1$tax_property[is.na(properties1$tax_building) & 
                                                                                  !(is.na(properties1$tax_property)) & 
                                                                                  is.na(properties1$tax_land)]/median_tax_rate
properties1$tax_building[is.na(properties1$tax_building) & 
                           !(is.na(properties1$tax_property))] <- 0

### where no tax_building, tax_land, tax_property, or tax_total, but more than zero 
### bathrooms or bedrooms, we impute the mean tax_building and tax_land for that 
### particular zipcode, number of bedrooms, and number of bathrooms in the missing 
### tax_building and tax_land columns
### Step 1: create new column with mean tax_building by by zip, nbed, nbath

properties1 <- properties1 %>% 
  group_by(region_zip, num_bedroom, num_bathroom) %>% 
  mutate(mean_zip_bb_tax_build = mean(tax_building, na.rm = TRUE)) %>% 
  ungroup

### Step 2: impute the new column value for tax_building where tax_building missing and 
### nbath or nbed > 0
properties1$tax_building[is.na(properties1$tax_building) & 
                           (properties1$num_bedroom > 0 | 
                              properties1$num_bathroom > 0) & 
                           !(is.na(properties1$num_bathroom))] <- properties1$mean_zip_bb_tax_build[is.na(properties1$tax_building) & 
                                                                                                      (properties1$num_bedroom > 0 |
                                                                                                         properties1$num_bathroom > 0) & 
                                                                                                      !(is.na(properties1$num_bathroom))]

### Step 3: impute zero for tax_land for the above observations where we imputed the mean tax_building value
properties1$tax_land[is.na(properties1$tax_land) & 
                       properties1$tax_building > 0 &
                       !(is.na(properties1$num_bathroom))] <- 0

### tax_total: equal to sum of tax_land and tax_property. useless.

###   Katie


###   Ningxi


###   John

