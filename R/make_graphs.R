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
  filter((yes_diff > 0) == TRUE) %>%  #where aca support outperformed dem support
  filter(state=="Idaho")


ggplot(ot_list, aes(x = democrat, y = yes_diff)) + geom_point()+ geom_text(label = ot_list$county) + labs(x = "Democratic Vote Share, 2016 Presidential", y = "Margin Between Yes Voteand 2016", title = "Yes Margin Vs 2016 Dem Presidential Vote")


