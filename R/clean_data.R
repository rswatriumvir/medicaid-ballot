library(tidyverse)
#load libraries

df=read_csv("raw_data/oklahoma_june_2020_elec.csv")
#load data

okl=df %>% 
  filter(race_description=="STATE QUESTION NO. 802 INITIATIVE PETITION NO. 419")
#apples