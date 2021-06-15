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

df$yes_diff = df$share_for - df$democrat #see how left-wing policy support compares to left-wing party support

df$non_white = 100 - df$white

ot_list = df %>% #outlier list
  filter((yes_diff > 0) == TRUE)   #where aca support outperformed dem support



ggplot(ot_list, aes(x = democrat, y = yes_diff)) + geom_point() + labs(x = "Democratic Vote Share, 2016 Presidential", y = "Margin Between Yes Vote and 2016", title = "Yes Margin Vs 2016 Dem Presidential Vote") + geom_smooth(method = "lm", formula = y~x)

#plotting education polarization, recently higher educated counties have begun to support dems
ggplot(df, aes(x = bachelors_25, y = share_for )) + geom_point() + geom_smooth(method = "lm", formula = y~x) + labs(x = "Percentage of Population with Bachelors over 25", y = "Percentage Supporting ACA Expansion", title = "Yes Vote Vs College Education")

#income polarization, Medicaid Expansion might generate more support from lower income voters
ggplot(df, aes(x = median_income, y = share_for)) + geom_point() + geom_smooth(method = "lm", formula = y~x) + labs(x = "County Median Income", y = "Percentage Supporting ACA Expansion", title = "Yes Vote Vs Median Income")

#measuring racial polarization 

ggplot(df, aes(x = non_white, y = share_for)) + geom_point() + geom_smooth(method = "lm", formula = y~x) + labs(x = "Non-White Share of Population", y = "share_for", title = "Yes Vote Vs Non-White")

#find trend in rural counties (less than 100k people)
rural = df %>%
  filter(population < 100000) 

ggplot(rural, aes(x = median_income, y = share_for))+ geom_point() + geom_smooth(method = "lm", formula = y~x) + labs(x = "Income", y = "Yes Vote Percentage", title = "Yes Vote Vs Income in Rural Areas") 

ggplot(rural, aes(x = population, y = share_for))+ geom_point() + geom_smooth(method = "lm", formula = y~x) + labs(x = "Population", y = "Yes Vote Percentage", title = "Yes Vote Vs Populatuon in Rural Areas") 

ggplot(rural, aes(x = non_white, y = share_for))+ geom_point() + geom_smooth(method = "lm", formula = y~x) + labs(x = "Non_White", y = "Yes Vote Percentage", title = "Yes Vote Vs Non_White in Rural Areas") 

rural %>%
  filter(yes_diff > 10) %>%
  ggplot(aes(x = non_white, y = share_for))+ geom_point() + geom_smooth(method = "lm", formula = y~x) + labs(x = "Non_White", y = "Yes Vote Percentage", title = "Yes Vote Vs Non_White in Rural Areas") 



