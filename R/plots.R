# libraries
library(tidyverse)
library(ggbeeswarm)
library(hrbrthemes)
library(ggrepel)

# read data
df = read_rds("output_data/aca_refs.rds")


# labels for overall margin
margin = df %>% 
  group_by(state) %>% 
  summarise(total_votes = sum(total_votes), 
            votes_for = sum(votes_for)) %>% 
  mutate(pct = votes_for/total_votes*100) %>% 
  mutate(label = paste0(state, ":", "\n", round(pct, 1), "%"))


# make swarm plot
ggplot(df, aes(x = state, y = share_for, 
               fill = state, size = total_votes)) + 
  geom_quasirandom(shape = 21, 
                   color = "white", alpha = .8) + 
  theme_ipsum() + 
  coord_flip() + 
  theme(legend.position = "none") + 
  scale_fill_viridis_d(option = "inferno", end = .8) + 
  labs(title = "Support for Medicaid Expansion", 
       subtitle = "County-level results from Medicaid expansion referendums in five states.", 
       x = NULL, 
       y = "% of voters in favor of expansion") + 
  scale_y_continuous(limits = c(0, 100), 
                     labels = scales::percent_format(scale = 1)) + 
  geom_hline(yintercept = 50, lty = 2, size = 1) +
  scale_color_viridis_d(option = "inferno", end = .8)


# output
ggsave("figures/vote-dist.pdf", device = cairo_pdf)
ggsave("figures/vote-dist.png", device = "png")



# raincloud plot
ggplot(df, aes(x = state, y = share_for, 
               color = state, fill = state, 
               size = total_votes)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    alpha = .3,
    outlier.shape = NA
  ) + 
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .3
  ) + 
  coord_flip() + 
  theme_ipsum() + 
  theme(legend.position = "none") + 
  scale_fill_viridis_d(option = "inferno", end = .8) + 
  labs(title = "Support for Medicaid Expansion", 
       subtitle = "County-level results from Medicaid expansion referendums in five states.", 
       x = NULL, 
       y = "% of voters in favor of expansion") + 
  scale_y_continuous(limits = c(0, 100), 
                     labels = scales::percent_format(scale = 1)) + 
  geom_hline(yintercept = 50, lty = 2, size = 1) + 
  scale_color_viridis_d(option = "inferno", end = .8)
