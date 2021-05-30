# load libraries --------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(tabulizer)
library(tabulizerjars)



# deal with excel -------------------------------------------------------------------



## oklahoma
okl = read_csv("raw_data/oklahoma_june_2020_elec.csv")

okl_dt = okl %>% 
  filter(race_description == "STATE QUESTION NO. 802 INITIATIVE PETITION NO. 419") %>%
  select(elec_date, county, cand_name, cand_tot_votes) %>%
  pivot_wider(names_from=cand_name, values_from=cand_tot_votes) %>%
  rename(votes_for =`FOR THE PROPOSAL - YES`) %>%
  rename(votes_against = `AGAINST THE PROPOSAL - NO`) %>%
  mutate(state = "Oklahoma") %>%
  mutate(total_votes = votes_for + votes_against) %>%
  mutate(share_for = (votes_for / total_votes) * 100) %>%
  mutate(share_against = (votes_against / total_votes) * 100) 


## idaho

# original file has useless first five rows; skip them
id = read_excel("raw_data/idaho_2018_results.xls", 
                sheet="Props - Voting Stats", 
                skip = 5)

id_dt = id %>%
  select(Counties, YES...4, NO...5) %>%
  rename(votes_for = YES...4) %>%
  rename(votes_against = NO...5) %>%
  mutate(state = "Idaho") %>%
  mutate(total_votes = votes_for + votes_against) %>%
  mutate(share_for = (votes_for / total_votes) * 100) %>%
  mutate(share_against = (votes_against / total_votes) * 100) %>%
  mutate(elec_date = '11/06/2018')
  
  
  
  
  


## montana

# original file has useless first 6 rows; skip them
mt = read_excel("raw_data/montana_2018_tobacco.xlsx", 
                skip = 6)




mt_dt = mt %>%
  select(County,`YES on INITIATIVE NO. 185`,`NO on INITIATIVE NO. 185`) %>%
  mutate(State = "Montana") %>%
  mutate(elec_date = '11/06/2018') %>%
  rename(votes_for=`YES on INITIATIVE NO. 185`) %>%
  rename(votes_against=`NO on INITIATIVE NO. 185`) %>%
  mutate(total_votes = votes_for + votes_against) %>%
  mutate(share_for = (votes_for / total_votes) * 100) %>%
  mutate(share_against = (votes_against / total_votes) * 100)


  

  

# deal with PDFs --------------------------------------------------------------------



ut=extract_tables("raw_data/2018 General Election Canvass Utah.pdf")
nb=extract_tables("raw_data/nebraska_2018_elec.pdf")
nb_dt=nb[318] ##list element with proposition data for Nebraska
nb_dt1=do.call(rbind, nb_dt)
#load data


