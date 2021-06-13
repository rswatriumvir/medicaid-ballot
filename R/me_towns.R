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
me_dem