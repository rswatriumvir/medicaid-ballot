# load libraries --------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(tabulizer)
library(tabulizerjars)
library(sjmisc)
library(janitor)
library(data.table)
options(scipen = 999) ##eliminates scientific notation, releavent for Idaho's data which was in scientific notation 

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
  mutate(elec_date = '11/06/2018') %>%
  rename(county = Counties) %>%
  slice(-(45:47))

  
  
  
  


## montana

# original file has useless first 6 rows; skip them
mt = read_excel("raw_data/montana_2018_tobacco.xlsx", 
                skip = 6)




mt_dt = mt %>%
  select(County,`YES on INITIATIVE NO. 185`,`NO on INITIATIVE NO. 185`) %>%
  mutate(state = "Montana") %>%
  mutate(elec_date = '11/06/2018') %>%
  rename(votes_for=`YES on INITIATIVE NO. 185`) %>%
  rename(votes_against=`NO on INITIATIVE NO. 185`) %>%
  mutate(total_votes = votes_for + votes_against) %>%
  mutate(share_for = (votes_for / total_votes) * 100) %>%
  mutate(share_against = (votes_against / total_votes) * 100) %>%
  rename(county = County) %>%
  slice(-(57))

#maine
me = read_excel("raw_data/maine_prop_results.xlsx")
me_dt_tn = me %>% #Maine Municipality Level Data
  select(...1, ...2, `Question 2:  Citizen Initiative`, ...7, ...8) %>%
  rename(county = ...1) %>%
  rename(municipality = ...2) %>%
  rename(votes_for = `Question 2:  Citizen Initiative`) %>%
  rename(votes_against = ...7) %>%
  rename(votes_blank = ...8)

me_dt_tn$votes_for = as.numeric(me_dt_tn$votes_for)
me_dt_tn$votes_against = as.numeric(me_dt_tn$votes_against)
me_dt_tn$votes_blank = as.numeric(me_dt_tn$votes_blank)


me_dt = me_dt_tn %>% #Maine County Level Data
  slice(-(1)) %>%
  select(-(county)) %>%
  select(-(votes_blank)) %>%
  slice(-(513:514)) %>%
  rename(county = municipality)%>% #renaming so we can eliminate the towns in this column
  filter((grepl("County Totals", county) == TRUE)) %>%
  mutate(total_votes = votes_for+votes_against) %>%
  mutate(share_for = (votes_for / total_votes) * 100) %>%
  mutate(share_against = (votes_against / total_votes) * 100) %>%
  mutate(state = "Maine") %>%
  mutate(elec_date = "11/07/2017")
  

me_dt$county = gsub("County Totals", "", me_dt$county)
me_dt$county = gsub(":", "", me_dt$county)
  
#utah
ut = read_excel("raw_data/2018 General Election Canvass Utah.xlsx", sheet = "Statewide Ballot Questions", skip = 2)

ut_dt = ut %>%
  select(County, FOR...6, AGAINST...7) %>% 
  rename(votes_for = FOR...6) %>%
  rename(votes_against = AGAINST...7) %>%
  rename(county = County) %>%
  mutate(total_votes = votes_for+votes_against) %>%
  mutate(share_for = (votes_for / total_votes) * 100) %>%
  mutate(share_against = (votes_against / total_votes) * 100) %>%
  mutate(state = "Utah") %>%
  mutate(elec_date = "11/06/2018") %>%
  slice(-(30:32))

  

  

## deal with PDFs --------------------------------------------------------------------

nb = extract_tables("raw_data/nebraska_2018_elec.pdf", pages = 65)
nb_tb = do.call(rbind, nb)

#nebraska

nb_tb = as.data.frame(nb_tb) #converting nb_tb1 from matrix to dataframe
nb_sl = nb_tb[4:6] #creating new dataframe with other set of counties from columns 4 to 6
nb_tb = nb_tb[1:3] #reducing original table to columns 1 to 3
nb_tb = slice(nb_tb,-(1:2)) #removing redundant rows


#renaming variables to merge using rbind
nb_tb1 = nb_tb %>%
  rename(county = V1) %>%
  rename(votes_for = V2) %>%
  rename(votes_against = V3)

nb_sl1 = nb_sl %>%
  rename(county = V4) %>%
  rename(votes_for = V5) %>%
  rename(votes_against = V6) %>%
  slice(-(48))

nb_tb_fn = rbind(nb_tb1, nb_sl1) 
nb_tb_fn$votes_for = as.numeric(gsub(",", "", nb_tb_fn$votes_for)) # convert variable from character string to numeric with commas removed
nb_tb_fn$votes_against = as.numeric(gsub(",", "", nb_tb_fn$votes_against)) #same process

nb_dt = nb_tb_fn %>%
  mutate(state = "Nebraska") %>%
  mutate(elec_date = "11/06/2018") %>%
  mutate(total_votes = votes_for + votes_against) %>%
  mutate(share_for = (votes_for / total_votes) * 100) %>%
  mutate(share_against = (votes_against / total_votes) * 100)
  

## adding ACS demographic and educational datasets -----------------------------------

'acs_demo_og = read_csv("raw_data/acs_demographics.csv") #original data from ACS demographics
acs_demo_og = as.data.frame(t(acs_demo_og)) #switching rows and columns
setDT(acs_demo_og, keep.rownames = TRUE)
acs_demo = acs_demo_og %>%
  row_to_names(row_number = 1) 
acs_demo$Label
filter((grepl("Percent", Label) == TRUE))'

'acs_demo[[2]]'

## merging datasets ------------------------------------------------------------------

aca_referendums = rbind(mt_dt, okl_dt, id_dt, nb_dt, me_dt, ut_dt)
