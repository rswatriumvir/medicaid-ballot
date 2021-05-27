library(tidyverse)
library(readxl)
library(dplyr)
library(tabulizer)
library(tabulizerjars)
#load libraries

okl=read_csv("raw_data/oklahoma_june_2020_elec.csv")
id=read_excel("raw_data/idaho_2018_results.xls", sheet="Props - Voting Stats")
mt=read_excel("raw_data/montana_2018_tobacco.xlsx")
ut=extract_tables("2018 General Election Canvass Utah.pdf")
nb=extract_tables("nebraska_2018_elec.pdf")
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
  slice(-(1:6)) %>%
  mutate(State="Montana") %>%
  rename(county=...2) %>%
  rename(Votes_For=...3) %>%
  rename(Votes_Against=...4) %>%
  as.double(Votes_For) %>%
  as.double(Votes_Against) %>%
  mutate(Total_Votes=Votes_For+Votes_Against) %>%
  mutate(Share_For=Votes_For/Total_Votes) %>%
  mutate(Share_Against=Votes_Against/Total_Votes)

id_dt=id %>%
  

  





