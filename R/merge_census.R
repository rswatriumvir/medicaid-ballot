# load libraries
library(tidyverse)
library(tabulizer)
library(tabulizerjars)
library(sjmisc)
library(janitor)
library(data.table)
library(tidycensus)

census_api_key("cbb745d65863b387da6bb0fc1cb9673540b32726",install = TRUE)
# load data
df = read_rds("output_data/aca_refs.rds")

## adding ACS demographic and educational datasets -----------------------------------

#original data from ACS demographics
acs_demo_ut = read_csv("raw_data/ACS_demo_ut.csv")
acs_demo_mo = read_csv("raw_data/ACS_demo_mo.csv")
acs_demo_ok = read_csv("raw_data/ACS_demo_ok.csv") 
acs_demo_mt = read_csv("raw_data/ACS_demo_mt.csv")
acs_demo_id = read_csv("raw_data/ACS_demo_id.csv")
acs_demo_me = read_csv("raw_data/ACS_demo_me.csv")
acs_demo_ne = read_csv("raw_data/ACS_demo_ne.csv") 
acs_educ_id = read.csv("raw_data/ACS_educ_id.csv")
acs_educ_me = read.csv("raw_data/ACS_educ_me.csv")
acs_educ_mo = read.csv("raw_data/acs_educ_mo.csv")

## adding ACS demographic essays -------------------------------------------------------
#utah
acs_demo_ut = as.data.frame(t(acs_demo_ut)) #switching rows and columns
setDT(acs_demo_ut, keep.rownames = TRUE)
dem_ut0 = acs_demo_ut %>%
  select(rn, V2, V3, V4, V30, V75, V81, V82, V83, V84, V85, V87) %>%
  rename(county = rn) %>%
  rename(population = V2) %>%
  rename(male = V3) %>%
  rename(female = V4) %>%
  rename(above_65 = V30) %>% #percentage 65 and above
  rename(hispanic = V75) %>% #Hispanic or Latino of any race
  rename(white = V81) %>% #identified as these designations alone 
  rename(black = V82) %>%
  rename(am_indian_alaskan = V83) %>% #American Indian and Alaskan alone
  rename(asian = V84) %>%
  rename(pacific_is = V85) %>% #Native Hawaiian and Pacific Islander alone
  rename(multi_racial = V87) %>%
  filter(grepl("Percent", county)==TRUE) 

dem_ut0[,2:12] = lapply(dem_ut0[,2:12], function(y) gsub(",", "", y))
dem_ut0[,2:12] = lapply(dem_ut0[,2:12], function(y) gsub("%", "", y))
dem_ut0[,2:12] = lapply(dem_ut0[,2:12], as.numeric)

dem_ut = dem_ut0 %>%
  mutate(above_65 = (above_65 / population) * 100) %>% 
  mutate(county = (gsub("County, Utah!!Percent", "", county))) 


#Missouri 
acs_demo_mo = as.data.frame(t(acs_demo_mo)) #switching rows and columns
setDT(acs_demo_mo, keep.rownames = TRUE)
dem_mo0 = acs_demo_mo %>%
  select(rn, V2, V3, V4, V30, V75, V81, V82, V83, V84, V85, V87) %>%
  rename(county = rn) %>%
  rename(population = V2) %>%
  rename(male = V3) %>%
  rename(female = V4) %>%
  rename(above_65 = V30) %>% #percentage 65 and above
  rename(hispanic = V75) %>% #Hispanic or Latino of any race
  rename(white = V81) %>% #identified as these designations alone 
  rename(black = V82) %>%
  rename(am_indian_alaskan = V83) %>% #American Indian and Alaskan alone
  rename(asian = V84) %>%
  rename(pacific_is = V85) %>% #Native Hawaiian and Pacific Islander alone
  rename(multi_racial = V87) %>%
  filter(grepl("Percent", county)==TRUE) 

dem_mo0[,2:12] = lapply(dem_mo0[,2:12], function(y) gsub(",", "", y))
dem_mo0[,2:12] = lapply(dem_mo0[,2:12], function(y) gsub("%", "", y))
dem_mo0[,2:12] = lapply(dem_mo0[,2:12], as.numeric)

dem_mo = dem_mo0 %>%
  mutate(above_65 = (above_65 / population) * 100) %>% 
  mutate(county = (gsub("County, Missouri!!Percent", "", county)))  

#Oklahoma
acs_demo_ok = as.data.frame(t(acs_demo_ok)) #switching rows and columns
setDT(acs_demo_ok, keep.rownames = TRUE)
dem_ok0 = acs_demo_ok %>%
  select(rn, V2, V3, V4, V30, V75, V81, V82, V83, V84, V85, V87) %>%
  rename(county = rn) %>%
  rename(population = V2) %>%
  rename(male = V3) %>%
  rename(female = V4) %>%
  rename(above_65 = V30) %>% #percentage 65 and above
  rename(hispanic = V75) %>% #Hispanic or Latino of any race
  rename(white = V81) %>% #identified as these designations alone 
  rename(black = V82) %>%
  rename(am_indian_alaskan = V83) %>% #American Indian and Alaskan alone
  rename(asian = V84) %>%
  rename(pacific_is = V85) %>% #Native Hawaiian and Pacific Islander alone
  rename(multi_racial = V87) %>%
  filter(grepl("Percent", county)==TRUE) 

dem_ok0[,2:12] = lapply(dem_ok0[,2:12], function(y) gsub(",", "", y))
dem_ok0[,2:12] = lapply(dem_ok0[,2:12], function(y) gsub("%", "", y))
dem_ok0[,2:12] = lapply(dem_ok0[,2:12], as.numeric)

dem_ok = dem_ok0 %>%
  mutate(above_65 = (above_65 / population) * 100) %>% 
  mutate(county = (gsub("County, Oklahoma!!Percent", "", county)))  

#Montana
acs_demo_mt = as.data.frame(t(acs_demo_mt)) #switching rows and columns
setDT(acs_demo_mt, keep.rownames = TRUE)
dem_mt0 = acs_demo_mt %>%
  select(rn, V2, V3, V4, V30, V75, V81, V82, V83, V84, V85, V87) %>%
  rename(county = rn) %>%
  rename(population = V2) %>%
  rename(male = V3) %>%
  rename(female = V4) %>%
  rename(above_65 = V30) %>% #percentage 65 and above
  rename(hispanic = V75) %>% #Hispanic or Latino of any race
  rename(white = V81) %>% #identified as these designations alone 
  rename(black = V82) %>%
  rename(am_indian_alaskan = V83) %>% #American Indian and Alaskan alone
  rename(asian = V84) %>%
  rename(pacific_is = V85) %>% #Native Hawaiian and Pacific Islander alone
  rename(multi_racial = V87) %>%
  filter(grepl("Percent", county)==TRUE) 

dem_mt0[,2:12] = lapply(dem_mt0[,2:12], function(y) gsub(",", "", y))
dem_mt0[,2:12] = lapply(dem_mt0[,2:12], function(y) gsub("%", "", y))
dem_mt0[,2:12] = lapply(dem_mt0[,2:12], as.numeric)

dem_mt = dem_mt0 %>%
  mutate(above_65 = (above_65 / population) * 100) %>% 
  mutate(county = (gsub("County, Montana!!Percent", "", county)))  

#Idaho
acs_demo_id = as.data.frame(t(acs_demo_id)) #switching rows and columns
setDT(acs_demo_id, keep.rownames = TRUE)
dem_id0 = acs_demo_id %>%
  select(rn, V2, V3, V4, V30, V75, V81, V82, V83, V84, V85, V87) %>%
  rename(county = rn) %>%
  rename(population = V2) %>%
  rename(male = V3) %>%
  rename(female = V4) %>%
  rename(above_65 = V30) %>% #percentage 65 and above
  rename(hispanic = V75) %>% #Hispanic or Latino of any race
  rename(white = V81) %>% #identified as these designations alone 
  rename(black = V82) %>%
  rename(am_indian_alaskan = V83) %>% #American Indian and Alaskan alone
  rename(asian = V84) %>%
  rename(pacific_is = V85) %>% #Native Hawaiian and Pacific Islander alone
  rename(multi_racial = V87) %>%
  filter(grepl("Percent", county)==TRUE) 

dem_id0[,2:12] = lapply(dem_id0[,2:12], function(y) gsub(",", "", y))
dem_id0[,2:12] = lapply(dem_id0[,2:12], function(y) gsub("%", "", y))
dem_id0[,2:12] = lapply(dem_id0[,2:12], as.numeric)

dem_id = dem_id0 %>%
  mutate(above_65 = (above_65 / population) * 100) %>% 
  mutate(county = (gsub("County, Idaho!!Percent", "", county)))  

#Maine
acs_demo_me = as.data.frame(t(acs_demo_me)) #switching rows and columns
setDT(acs_demo_me, keep.rownames = TRUE)
dem_me0 = acs_demo_me %>%
  select(rn, V2, V3, V4, V30, V75, V81, V82, V83, V84, V85, V87) %>%
  rename(county = rn) %>%
  rename(population = V2) %>%
  rename(male = V3) %>%
  rename(female = V4) %>%
  rename(above_65 = V30) %>% #percentage 65 and above
  rename(hispanic = V75) %>% #Hispanic or Latino of any race
  rename(white = V81) %>% #identified as these designations alone 
  rename(black = V82) %>%
  rename(am_indian_alaskan = V83) %>% #American Indian and Alaskan alone
  rename(asian = V84) %>%
  rename(pacific_is = V85) %>% #Native Hawaiian and Pacific Islander alone
  rename(multi_racial = V87) %>%
  filter(grepl("Percent", county)==TRUE) 

dem_me0[,2:12] = lapply(dem_me0[,2:12], function(y) gsub(",", "", y))
dem_me0[,2:12] = lapply(dem_me0[,2:12], function(y) gsub("%", "", y))
dem_me0[,2:12] = lapply(dem_me0[,2:12], as.numeric)

dem_me = dem_me0 %>%
  mutate(above_65 = (above_65 / population) * 100) %>% 
  mutate(county = (gsub("County, Maine!!Percent", "", county)))  

#Nebraska
acs_demo_ne = as.data.frame(t(acs_demo_ne)) #switching rows and columns
setDT(acs_demo_ne, keep.rownames = TRUE)
dem_ne0 = acs_demo_ne %>%
  select(rn, V2, V3, V4, V30, V75, V81, V82, V83, V84, V85, V87) %>%
  rename(county = rn) %>%
  rename(population = V2) %>%
  rename(male = V3) %>%
  rename(female = V4) %>%
  rename(above_65 = V30) %>% #percentage 65 and above
  rename(hispanic = V75) %>% #Hispanic or Latino of any race
  rename(white = V81) %>% #identified as these designations alone 
  rename(black = V82) %>%
  rename(am_indian_alaskan = V83) %>% #American Indian and Alaskan alone
  rename(asian = V84) %>%
  rename(pacific_is = V85) %>% #Native Hawaiian and Pacific Islander alone
  rename(multi_racial = V87) %>%
  filter(grepl("Percent", county)==TRUE) 

dem_ne0[,2:12] = lapply(dem_ne0[,2:12], function(y) gsub(",", "", y))
dem_ne0[,2:12] = lapply(dem_ne0[,2:12], function(y) gsub("%", "", y))
dem_ne0[,2:12] = lapply(dem_ne0[,2:12], as.numeric)

dem_ne = dem_ne0 %>%
  mutate(above_65 = (above_65 / population) * 100) %>% 
  mutate(county = (gsub("County, Nebraska!!Percent", "", county)))  

#ACS educational data ----------------------------------------------------------------
#Idaho
acs_educ_id = as.data.frame(t(acs_educ_id)) #switching rows and columns
setDT(acs_educ_id, keep.rownames = TRUE)
educ_id0 = acs_educ_id %>%
  select(rn, V7, V16, V63) %>%
  rename(county = rn) %>%
  filter((grepl("Total..Estimate", county)) == TRUE) %>%
  slice(-(1)) 

educ_id0[,2:4] = lapply(educ_id0[,2:4], function(y) gsub(",", "", y))
educ_id0[,2:4] = lapply(educ_id0[,2:4], as.numeric)

educ_id = educ_id0 %>%
  mutate(bachelors_25 = (V16 / V7) * 100) %>%
  rename(median_income = V63) %>%
  select(county, median_income, bachelors_25) %>%
  mutate(county = (gsub(".County..Idaho..Total..Estimate", "", county))) 

#Missouri
acs_educ_mo = as.data.frame(t(acs_educ_mo)) #switching rows and columns
setDT(acs_educ_mo, keep.rownames = TRUE)

educ_mo0 = acs_educ_mo %>%
  select(NAME, S1501_C01_006E, S1501_C01_012E, S1501_C01_059E) %>%
  rename(median_income = S1501_C01_059E) %>%
  rename(county = NAME) %>%
  mutate(county = (gsub("County, Missouri", "", county))) 

educ_mo0[,2:4] = lapply(educ_mo0[,2:4], function(y) gsub(",", "", y))
educ_mo0[,2:4] = lapply(educ_mo0[,2:4], as.numeric)

educ_mo = educ_mo0 %>%
  mutate(bachelors_25 = (S1501_C01_012E / S1501_C01_006E) * 100) %>%
  slice(-(1)) %>%
  select(county, bachelors_25, median_income)
  
educ_mo$county[115] = "St. Louis City"

#Maine
acs_educ_me = as.data.frame(t(acs_educ_me)) #switching rows and columns
setDT(acs_educ_me, keep.rownames = TRUE)
educ_me0 = acs_educ_me %>%
  select(rn, V7, V16, V63) %>%
  rename(county = rn) %>%
  filter((grepl("Total..Estimate", county)) == TRUE) %>%
  slice(-(1)) 

educ_me0[,2:4] = lapply(educ_me0[,2:4], function(y) gsub(",", "", y))
educ_me0[,2:4] = lapply(educ_me0[,2:4], as.numeric)

educ_me = educ_me0 %>%
  mutate(bachelors_25 = (V16 / V7) * 100) %>%
  rename(median_income = V63) %>%
  select(county, median_income, bachelors_25) %>%
  mutate(county = (gsub(".County..Maine..Total..Estimate", "", county))) 




  


#merging demographic datasets
acs_demo = rbind(dem_ut, dem_id, dem_ne, dem_me, dem_mt, dem_ok, dem_mo)
