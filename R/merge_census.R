# load libraries
library(tidyverse)
library(tabulizer)
library(tabulizerjars)
library(sjmisc)
library(janitor)
library(data.table)

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
acs_demo_ut = read_csv("raw_data/ACS_demo_ut.csv") 

#utah
acs_demo_ut = as.data.frame(t(acs_demo_ut)) #switching rows and columns
setDT(acs_demo_ut, keep.rownames = TRUE)
acs_demo_ut[2:12] = as.numeric(acs_demo_ut[2:12])
DF[cols.num] <- sapply(DF[cols.num],as.numeric)
dem_ut = acs_demo_ut %>%
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
  mutate(above_65 = (above_65 / population)) %>%
  slice(-(1)) %>%
  filter(grepl("Percent", county)==TRUE) %>%
  mutate(county = (gsub("County, Utah!!Percent", "", county)))

#Missouri 
acs_demo_mo = as.data.frame(t(acs_demo_mo)) #switching rows and columns
setDT(acs_demo_mo, keep.rownames = TRUE)
dem_mo = acs_demo_mo %>%
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
  mutate(above_65 = (above_65 / population)) %>%
  slice(-(1)) %>%
  filter(grepl("Percent", county)==TRUE) %>%
  mutate(county = (gsub("County, Utah!!Percent", "", county)))

