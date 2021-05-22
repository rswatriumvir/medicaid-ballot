library(tidyverse)
#load libraries

df=read_csv("raw_data/oklahoma_june_2020_elec.csv")
#load data

okl=df %>% 
  filter(race_description=="STATE QUESTION NO. 802 INITIATIVE PETITION NO. 419") %>%
  select(elec_date, county, cand_name, cand_tot_votes) %>%
  pivot_wider(names_from=cand_name, values_from=cand_tot_votes)


