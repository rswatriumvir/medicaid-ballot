# load libraries ----------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(tabulizer)
library(tabulizerjars)
library(sjmisc)
library(janitor)
library(data.table)
library(ggrepel)
library(ggbeeswarm)
library(hrbrthemes)


# load data -----------------------------------------------------------------------------
df = read_rds("output_data/aca_refs_final.rds")


#Identity outlier counties in terms of past politics ------------------------------------

#Yes for expanding Medicaid is generally associated with Dem leaning voters, vice-versa for Republicans 

df$yes_diff = df$share_for - df$democrat #see how left-wing policy support compares to left-wing party support

df$non_white = 100 - df$white

ot_list = df %>% #outlier list
  filter((yes_diff > 0) == TRUE)   #where aca support outperformed dem support



ggplot(ot_list, aes(x = democrat, y = yes_diff) + geom_point() + labs(x = "Democratic Vote Share, 2016 Presidential", y = "Margin Between Yes Vote and 2016", title = "Yes Margin Vs 2016 Dem Presidential Vote") + geom_smooth(method = "lm", formula = y~x)

#plotting education polarization, recently higher educated counties have begun to support dems
ggplot(df, aes(x = bachelors_25, y = share_for)) + geom_point() + geom_smooth(method = "lm", formula = y~x) + labs(x = "Percentage of Population with Bachelors over 25", y = "Percentage Supporting ACA Expansion", title = "Yes Vote Vs College Education")

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

ggplot(rural, aes(x = bachelors_25, y = share_for))+ geom_point() + geom_smooth(method = "lm", formula = y~x) + labs(x = "Education", y = "Yes Vote Percentage", title = "Yes Vote Vs Education in Rural Areas") 

rural_states = rural %>%
  group_by(state) %>%
  summarise(yes_diff = mean(yes_diff), share_for = mean(share_for))

#Rural Maine, Idaho, and Utah seem to have the largest share of voters for the expansion. Maine is a swing state that leans slightly left, but Idaho and Utah are both deep red states with large Mormon populations. Mormon populations tend to be a lot more supportive of private charity. Maybe the reason for Idaho and Utah's large rural voter bases supporting the expansion might be the religious angle? 

##see how counties with Mormon population vs non-Mormon population in Idaho compare
mormon_idaho = df %>%
  filter(state == "Idaho") %>%
  filter(county == "fremont"|county == "bingham"|county == "madison"|county == "bear lake"|county == "franklin"|county == "oneida"|county == "caribou"|county == "bannock"|county == "cassia"|county == "butte"|county == "jefferson"|county == "bonneville") %>%
  summarise(yes_diff = mean(yes_diff), share_for = mean(share_for))

non_mormon_idaho = df %>%
  filter(state == "Idaho") %>%
  filter(county != "fremont"|county != "bingham"|county != "madison"|county != "bear lake"|county != "franklin"|county != "oneida"|county != "caribou"|county != "bannock"|county != "cassia"|county != "butte"|county != "jefferson"|county != "bonneville") %>%
  summarise(yes_diff = mean(yes_diff), share_for = mean(share_for))

#not a large diff, Non-Mormon counties actually have higher percentage for Yes votes

rural %>%
  filter(share_for > 20 & share_for < 35) %>%
  ggplot(aes(x = median_income, y = share_for)) + geom_point() + labs(x = "Income", y = "Yes Vote Percentage", title = "Yes Vote Vs Income in Rural Areas") + geom_text_repel(aes(label = county), size = 3)


#making graphs comparing state populations to national average 

ggplot(df, aes(x = state, y = median_income, 
               fill = state, size = population)) + geom_quasirandom(shape = 21, 
               color = "white", alpha = .8) + coord_flip() +
               geom_hline(yintercept = 31133, lty = 2, size = 1) +
               labs(title = "Median County Income Compared",
                    x = NULL,
                    y = "Median Income") +
               scale_y_continuous(limits = c(20000, 55000)) +
               theme(legend.position = "none")

ggsave("figures/county-income.png", device = "png")

ggplot(df, aes(x = state, y = bachelors_25, 
               fill = state, size = population)) + geom_quasirandom(shape = 21, 
               color = "white", alpha = .8) + coord_flip() +
               geom_hline(yintercept = 36.6, lty = 2, size = 1) +
               labs(title = "Education of Counties Compared",
                    x = NULL,
                    y = "Percentage of Population Above 25 with at least Bachelors") +
               scale_y_continuous(limits = c(0, 100), 
                     labels = scales::percent_format(scale = 1)) +
               theme(legend.position = "none")

ggsave("figures/county-education.png", device = "png")

ggplot(df, aes(x = state, y = non_white, 
               fill = state, size = population)) + geom_quasirandom(shape = 21, 
               color = "white", alpha = .8) + coord_flip() +
               geom_hline(yintercept = 40, lty = 2, size = 1) +
               labs(title = "Diversity of Counties Compared",
                    x = NULL,
                    y = "Percentage of Population that is Non-White") +
               scale_y_continuous(limits = c(0, 100), 
                     labels = scales::percent_format(scale = 1)) +
               theme(legend.position = "none")

ggsave("figures/diversity.png", device = "png")

ggplot(df, aes(x = state, y = republican, 
               fill = state, size = population)) + geom_quasirandom(shape = 21, 
               color = "white", alpha = .8) + coord_flip() +
               geom_hline(yintercept = 46.1, lty = 2, size = 1) +
               labs(title = "Conservatism of Counties Compared",
                    x = NULL,
                    y = "Percentage of Vote for Republicans in 2016 Presidential") +
               scale_y_continuous(limits = c(0, 100), 
                     labels = scales::percent_format(scale = 1)) +
               theme(legend.position = "none")

ggsave("figures/county-rep.png", device = "png")

ggplot(df, aes(x = state, y = democrat, 
               fill = state, size = population)) + geom_quasirandom(shape = 21, 
               color = "white", alpha = .8) + coord_flip() +
               geom_hline(yintercept = 48.2, lty = 2, size = 1) +
               labs(title = "Liberalism of Counties Compared",
                    x = NULL,
                    y = "Percentage of Vote for Democrats in 2016 Presidential") +
               scale_y_continuous(limits = c(0, 100), 
                     labels = scales::percent_format(scale = 1)) +
               theme(legend.position = "none")

ggsave("figures/county-dem.png", device = "png") 


  


