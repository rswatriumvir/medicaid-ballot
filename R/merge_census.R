# load libraries
library(tidyverse)


# load data
df = read_rds("output_data/aca_refs.rds")

## adding ACS demographic and educational datasets -----------------------------------

'acs_demo_og = read_csv("raw_data/acs_demographics.csv") #original data from ACS demographics
acs_demo_og = as.data.frame(t(acs_demo_og)) #switching rows and columns
setDT(acs_demo_og, keep.rownames = TRUE)
acs_demo = acs_demo_og %>%
  row_to_names(row_number = 1) 
acs_demo$Label
filter((grepl("Percent", Label) == TRUE))'

'acs_demo[[2]]'