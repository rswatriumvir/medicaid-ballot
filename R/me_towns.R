# load libraries
library(tidyverse)
library(tabulizer)
library(tabulizerjars)
library(sjmisc)
library(janitor)
library(data.table)
library(tidycensus)
library(stringr)

#load data -----------------------------------------------------------------------------------

df = read_rds("output_data/me_munic")
acs_dem_me_tn = read.csv("raw_data/acs_me_town_demo.csv", strip.white = TRUE)


#clean demographic data ----------------------------------------------------------------------
acs_dem_me_tn = as.data.frame(t(acs_dem_me_tn)) #switching rows and columns
setDT(acs_dem_me_tn, keep.rownames = TRUE)
dem_me0 = acs_dem_me %>%
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