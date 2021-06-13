# load libraries
library(tidyverse)
library(tabulizer)
library(tabulizerjars)
library(sjmisc)
library(janitor)
library(data.table)
library(tidycensus)
library(stringr)


# load data
df = read_rds("output_data/aca_refs.rds")

## adding ACS demographic and educational datasets -----------------------------------

#original data from ACS demographics
acs_demo_ut = read.csv("raw_data/ACS_demo_ut.csv", strip.white=TRUE)
acs_demo_mo = read.csv("raw_data/ACS_demo_mo.csv", strip.white=TRUE)
acs_demo_ok = read.csv("raw_data/ACS_demo_ok.csv", strip.white=TRUE)
acs_demo_mt = read.csv("raw_data/ACS_demo_mt.csv", strip.white=TRUE)
acs_demo_id = read.csv("raw_data/ACS_demo_id.csv", strip.white=TRUE)
acs_demo_me = read.csv("raw_data/ACS_demo_me.csv", strip.white=TRUE)
acs_demo_ne = read.csv("raw_data/ACS_demo_ne.csv", strip.white=TRUE) 

acs_educ_id = read.csv("raw_data/ACS_educ_id.csv", strip.white=TRUE)
acs_educ_me = read.csv("raw_data/ACS_educ_me.csv", strip.white=TRUE)
acs_educ_mo = read.csv("raw_data/acs_educ_mo.csv", strip.white=TRUE)
acs_educ_ut = read.csv("raw_data/acs_educ_ut.csv", strip.white=TRUE)
acs_educ_ne = read.csv("raw_data/acs_educ_ne.csv", strip.white=TRUE)
acs_educ_ok = read.csv("raw_data/acs_educ_ok.csv", strip.white=TRUE)
acs_educ_mt = read.csv("raw_data/acs_educ_mt.csv", strip.white=TRUE)

#original election data
county_elections = read.csv("raw_data/countypres_2000-2016.csv")

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
  mutate(county = (gsub(".County..Utah..Percent", "", county))) %>%
  mutate(state = "Utah")


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
  mutate(county = (gsub(".County..Missouri..Percent", "", county))) %>%
  mutate(state = "Missouri")

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
  filter(grepl("Percent", county)==TRUE) %>%
  mutate(state = "Oklahoma")
  

dem_ok0[,2:12] = lapply(dem_ok0[,2:12], function(y) gsub(",", "", y))
dem_ok0[,2:12] = lapply(dem_ok0[,2:12], function(y) gsub("%", "", y))
dem_ok0[,2:12] = lapply(dem_ok0[,2:12], as.numeric)

dem_ok = dem_ok0 %>%
  mutate(above_65 = (above_65 / population) * 100) %>% 
  mutate(county = (gsub(".County..Oklahoma..Percent", "", county)))  

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
  mutate(county = (gsub(".County..Montana..Percent", "", county))) %>%
  mutate(state = "Montana")

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
  mutate(county = (gsub(".County..Idaho..Percent", "", county))) %>%
  mutate(state = "Idaho")

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
  mutate(county = (gsub(".County..Maine..Percent", "", county))) %>%
  mutate(state = "Maine")

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
  mutate(county = (gsub(".County..Nebraska..Percent", "", county))) %>%
  mutate(state = "Nebraska")

#ACS educational data ----------------------------------------------------------------
#Idaho
acs_educ_id = as.data.frame(t(acs_educ_id)) #switching rows and columns
setDT(acs_educ_id, keep.rownames = TRUE)
educ_id0 = acs_educ_id %>%
  select(rn, V7, V16, V63) %>%
  rename(county = rn) %>%
  filter((grepl("Total..Estimate", county)) == TRUE) 

educ_id0[,2:4] = lapply(educ_id0[,2:4], function(y) gsub(",", "", y))
educ_id0[,2:4] = lapply(educ_id0[,2:4], as.numeric)

educ_id = educ_id0 %>%
  mutate(bachelors_25 = (V16 / V7) * 100) %>%
  rename(median_income = V63) %>%
  select(county, median_income, bachelors_25) %>%
  mutate(county = (gsub(".County..Idaho..Total..Estimate", "", county))) %>%
  mutate(state = "Idaho")

#Missouri
setDT(acs_educ_mo, keep.rownames = TRUE)

educ_mo0 = acs_educ_mo %>%
  select(NAME, S1501_C01_006E, S1501_C01_012E, S1501_C01_059E) %>%
  rename(median_income = S1501_C01_059E) %>%
  rename(county = NAME) %>%
  mutate(county = (gsub(" County, Missouri", "", county))) 

educ_mo0[,2:4] = lapply(educ_mo0[,2:4], function(y) gsub(",", "", y))
educ_mo0[,2:4] = lapply(educ_mo0[,2:4], as.numeric)

educ_mo = educ_mo0 %>%
  mutate(bachelors_25 = (S1501_C01_012E / S1501_C01_006E) * 100) %>%
  slice(-(1)) %>%
  select(county, bachelors_25, median_income) %>%
  mutate(state = "Missouri")
  
educ_mo$county[115] = "St. Louis City"

#Maine 
acs_educ_me = as.data.frame(t(acs_educ_me))
setDT(acs_educ_me, keep.rownames = TRUE)
educ_me0 = acs_educ_me %>%
  select(rn, V7, V16, V63) %>%
  rename(county = rn) %>%
  filter((grepl("Total..Estimate", county)) == TRUE) 

educ_me0[,2:4] = lapply(educ_me0[,2:4], function(y) gsub(",", "", y))
educ_me0[,2:4] = lapply(educ_me0[,2:4], as.numeric)

educ_me = educ_me0 %>%
  mutate(bachelors_25 = (V16 / V7) * 100) %>%
  rename(median_income = V63) %>%
  select(county, median_income, bachelors_25) %>%
  mutate(county = (gsub(".County..Maine..Total..Estimate", "", county))) %>%
  mutate(state = "Maine")


#Utah
acs_educ_ut = as.data.frame(t(acs_educ_ut)) #switching rows and columns
setDT(acs_educ_ut, keep.rownames = TRUE)
educ_ut0 = acs_educ_ut %>%
  select(rn, V7, V16, V63) %>%
  rename(county = rn) %>%
  filter((grepl("Total..Estimate", county)) == TRUE) 
  

educ_ut0[,2:4] = lapply(educ_ut0[,2:4], function(y) gsub(",", "", y))
educ_ut0[,2:4] = lapply(educ_ut0[,2:4], as.numeric)

educ_ut = educ_ut0 %>%
  mutate(bachelors_25 = (V16 / V7) * 100) %>%
  rename(median_income = V63) %>%
  select(county, median_income, bachelors_25) %>%
  mutate(county = (gsub(".County..Utah..Total..Estimate", "", county))) %>%
  mutate(state = "Utah")
  
#Oklahoma
acs_educ_ok = as.data.frame(t(acs_educ_ok)) #switching rows and columns
setDT(acs_educ_ok, keep.rownames = TRUE)
educ_ok0 = acs_educ_ok %>%
  select(rn, V7, V16, V63) %>%
  rename(county = rn) %>%
  filter((grepl("Total..Estimate", county)) == TRUE) 

educ_ok0[,2:4] = lapply(educ_ok0[,2:4], function(y) gsub(",", "", y))
educ_ok0[,2:4] = lapply(educ_ok0[,2:4], as.numeric)

educ_ok = educ_ok0 %>%
  mutate(bachelors_25 = (V16 / V7) * 100) %>%
  rename(median_income = V63) %>%
  select(county, median_income, bachelors_25) %>%
  mutate(county = (gsub(".County..Oklahoma..Total..Estimate", "", county))) %>%
  mutate(state = "Oklahoma")

#Montana
acs_educ_mt = as.data.frame(t(acs_educ_mt)) #switching rows and columns
setDT(acs_educ_mt, keep.rownames = TRUE)
educ_mt0 = acs_educ_mt %>%
  select(rn, V7, V16, V63) %>%
  rename(county = rn) %>%
  filter((grepl("Total..Estimate", county)) == TRUE) 

educ_mt0[,2:4] = lapply(educ_mt0[,2:4], function(y) gsub(",", "", y))
educ_mt0[,2:4] = lapply(educ_mt0[,2:4], as.numeric)

educ_mt = educ_mt0 %>%
  mutate(bachelors_25 = (V16 / V7) * 100) %>%
  rename(median_income = V63) %>%
  select(county, median_income, bachelors_25) %>%
  mutate(county = (gsub(".County..Montana..Total..Estimate", "", county))) %>%
  mutate(state = "Montana")

#Nebraska 
setDT(acs_educ_ne, keep.rownames = TRUE)

educ_ne0 = acs_educ_ne %>%
  select(NAME, S1501_C01_006E, S1501_C01_012E, S1501_C01_059E) %>%
  rename(median_income = S1501_C01_059E) %>%
  rename(county = NAME) %>%
  mutate(county = (gsub(" County, Nebraska", "", county))) 

educ_ne0[,2:4] = lapply(educ_ne0[,2:4], function(y) gsub(",", "", y))
educ_ne0[,2:4] = lapply(educ_ne0[,2:4], as.numeric)

educ_ne = educ_ne0 %>%
  mutate(bachelors_25 = (S1501_C01_012E / S1501_C01_006E) * 100) %>%
  slice(-(1)) %>%
  select(county, bachelors_25, median_income) %>%
  mutate(state = "Nebraska")

#filter previous elections
elec_2016 = county_elections %>%
  filter(state_po == "ME"| state_po == "MT"| state_po == "UT"| state_po == "ID"| state_po == "NE"| state_po == "OK"| state_po == "MO") %>%
  filter(year == 2016) %>%
  filter(party == "democrat" | party == "republican") %>%
  mutate(percentage = (candidatevotes / totalvotes) * 100) %>%
  select(state, county, percentage, party) %>%
  pivot_wider(names_from = party, values_from = percentage) %>%
  slice(-(325)) 

# fixing formatting in dem_ne & dem_mo--------------------------------------------------------
dem_ne[7]$county = "Box Butte"
dem_ne[52]$county = "Keya Paha"
dem_ne[73]$county = "Red Willow"
dem_ne[79]$county = "Scotts Bluff"

dem_mo[16]$county = "Cape Girardeau"
dem_mo[72]$county = "New Madrid"
dem_mo[92]$county = "St. Charles"
dem_mo[93]$county = "St. Clair"
dem_mo[94]$county = "Ste. Genevieve"
dem_mo[95]$county = "St. Francois"
dem_mo[96]$county = "St. Louis"
dem_mo[115]$county = "St. Louis City"

#merging educational & demographic datasets --------------------------------------------

acs_demo = rbind(dem_ut, dem_id, dem_ne, dem_me, dem_mt, dem_ok, dem_mo)
acs_educ = rbind(educ_ut, educ_id, educ_ne, educ_me, educ_mt, educ_ok, educ_mo)

#fixing formatting issues in acs_educ 
acs_educ[33]$county = "Bear Lake"
acs_educ[184]$county = "Big Horn"
acs_educ[2]$county = "Box Elder"
acs_educ[194]$county = "Deer Lodge"
acs_educ[201]$county = "Golden Valley"
acs_educ[205]$county = "Judith Basin"
acs_educ[278]$county = "Le Flore"
acs_educ[207]$county = "Lewis and Clark"
acs_educ[64]$county = "Nez Perce"
acs_educ[220]$county = "Powder River"
acs_educ[303]$county = "Roger Mills"
acs_educ[18]$county = "Salt Lake"
acs_educ[19]$county = "San Juan"
acs_educ[229]$county = "Silver Bow"
acs_educ[231]$county = "Sweet Grass"
acs_educ[71]$county = "Twin Falls"

acs_demo[33]$county = "Bear Lake"
acs_demo[184]$county = "Big Horn"
acs_demo[2]$county = "Box Elder"
acs_demo[194]$county = "Deer Lodge"
acs_demo[201]$county = "Golden Valley"
acs_demo[205]$county = "Judith Basin"
acs_demo[278]$county = "Le Flore"
acs_demo[207]$county = "Lewis and Clark"
acs_demo[64]$county = "Nez Perce"
acs_demo[220]$county = "Powder River"
acs_demo[303]$county = "Roger Mills"
acs_demo[18]$county = "Salt Lake"
acs_demo[19]$county = "San Juan"
acs_demo[229]$county = "Silver Bow"
acs_demo[231]$county = "Sweet Grass"
acs_demo[71]$county = "Twin Falls"


acs_final = merge(acs_demo, acs_educ, by = c("county", "state"))

elec_2016 = as.data.frame(elec_2016) #converting 2016 election data from table to dataframe

acs_final[373]$county = "St. Louis County" #St Louis City is diff from St Louis County

demog_elec = merge(acs_final, elec_2016, by = c("county", "state")) #demographics and electoral data

##convert both county names to lowercase in order to merge successfully
df$county = tolower(df$county) 

demog_elec$county = tolower(demog_elec$county)

df = as.data.frame(df) #convert to dataframe
df$county = trimws(df$county, which = "right")

demog_elec[227,]$county = "lewis & clark"
df[96,]$county = "le flore"

aca_refs = merge(df, demog_elec, by = c("county", "state"))

##write final rds script

write_rds(aca_refs, "output_data/aca_refs_final.rds")



