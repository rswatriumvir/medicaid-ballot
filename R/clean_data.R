library(tidyverse)
library(readxl)
#load libraries

okl=read_csv("raw_data/oklahoma_june_2020_elec.csv")
id=read_excel("raw_data/idaho_2018_results.xls")
mt=read_excel("raw_data/montana_2018_tobacco.xlsx")
#load data

okl_dt=okl %>% 
  filter(race_description=="STATE QUESTION NO. 802 INITIATIVE PETITION NO. 419") %>%
  select(elec_date, county, cand_name, cand_tot_votes) %>%
  pivot_wider(names_from=cand_name, values_from=cand_tot_votes)
#cleaning oklahoma data

mt_dt=mt %>%
  select(2,3,4) 
  





