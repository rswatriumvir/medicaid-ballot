library(tidyverse)
library(readxl)
library(dplyr)
#load libraries

okl=read_csv("raw_data/oklahoma_june_2020_elec.csv")
id=read_excel("raw_data/idaho_2018_results.xls")
mt=read_excel("raw_data/montana_2018_tobacco.xlsx")
#load data

okl_dt=okl %>% 
  filter(race_description=="STATE QUESTION NO. 802 INITIATIVE PETITION NO. 419") %>%
  select(elec_date, county, cand_name, cand_tot_votes) %>%
  pivot_wider(names_from=cand_name, values_from=cand_tot_votes) %>%
  mutate(State="Oklahoma") %>%
  mutate(Total_Votes=`FOR THE PROPOSAL - YES`+ `AGAINST THE PROPOSAL - NO`) %>%
  mutate(Share_For=(`FOR THE PROPOSAL - YES`/Total_Votes)*100) %>%
  mutate(Share_Against=(`AGAINST THE PROPOSAL - NO`/Total_Votes)*100)
#cleaning oklahoma data

mt_dt=mt %>%
  select(...2,...3,...4) %>%
  mt_dt=mt_dt[-c(1,2,3),] 
  #mutate(State="Montana") %>%
  #rename(county=...2) %>%
  #rename(Votes_For=...3) %>%
  #rename(Votes_Against=...4)
  ##mutate(Total_Votes=mt_dt$...3+mt_dt$...4, na.rm=TRUE)%>%
  ##mutate(Share_For=mt_dt$...3/Total_Votes, na.rm=TRUE) %>%
  ##mutate(Share_Against=mt_dt$...4/Total_Votes, na.rm=TRUE)
  





