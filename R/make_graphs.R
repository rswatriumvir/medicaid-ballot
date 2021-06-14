# load libraries ---------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(tabulizer)
library(tabulizerjars)
library(sjmisc)
library(janitor)
library(data.table)

# load data --------------------------------------------------------------------------------
df = read_rds("output_data/aca_refs_final.rds")


#Identity outlier counties in terms of past politics ---------------------------------------

#Yes for expanding Medicaid is generally associated with Dem leaning voters, vice-versa for Republicans 

ot_list = df %>%
  mutate(yes_diff = share_for - democrat) %>% #see how left-wing policy support compares to left-wing party support
  filter((yes_diff > 0) == TRUE) %>%
  filter((yes_diff > 10) == TRUE)

