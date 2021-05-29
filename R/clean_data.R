# load libraries --------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(tabulizer)
library(tabulizerjars)



# deal with excel -------------------------------------------------------------------



## oklahoma
okl = read_csv("raw_data/oklahoma_june_2020_elec.csv")

okl_dt = okl %>% 
  filter(race_description=="STATE QUESTION NO. 802 INITIATIVE PETITION NO. 419") %>%
  select(elec_date, county, cand_name, cand_tot_votes) %>%
  pivot_wider(names_from=cand_name, values_from=cand_tot_votes) %>%
  mutate(State="Oklahoma") %>%
  mutate(Total_Votes=`FOR THE PROPOSAL - YES`+ `AGAINST THE PROPOSAL - NO`) %>%
  mutate(Share_For=(`FOR THE PROPOSAL - YES`/Total_Votes)*100) %>%
  mutate(Share_Against=(`AGAINST THE PROPOSAL - NO`/Total_Votes)*100) 


## idaho

# original file has useless first five rows; skip them
id = read_excel("raw_data/idaho_2018_results.xls", 
                sheet="Props - Voting Stats", 
                skip = 5)



## montana

# original file has useless first 6 rows; skip them
mt = read_excel("raw_data/montana_2018_tobacco.xlsx", 
                skip = 6)




mt_dt=mt %>%
  select(County,`YES on INITIATIVE NO. 185`,`NO on INITIATIVE NO. 185`) %>%
  mutate(State="Montana") %>%
  rename(Votes_For=`YES on INITIATIVE NO. 185`) %>%
  rename(Votes_Against=`NO on INITIATIVE NO. 185`) %>%
  mutate(Total_Votes=Votes_For+Votes_Against) %>%
  mutate(Share_For=(Votes_For/Total_Votes)*100) %>%
  mutate(Share_Against=(Votes_Against/Total_Votes)*100)


  

  

# deal with PDFs --------------------------------------------------------------------



ut=extract_tables("raw_data/2018 General Election Canvass Utah.pdf")
nb=extract_tables("raw_data/nebraska_2018_elec.pdf")
nb_dt=nb[318] ##list element with proposition data for Nebraska
nb_dt1=do.call(rbind, nb_dt)
#load data


