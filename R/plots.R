# libraries
library(tidyverse)
library(ggbeeswarm)
library(hrbrthemes)

# read data
df = read_rds("output_data/aca_refs.rds")


# labels for big cities
big_city = df %>% 
  group_by(state) %>% 
  filter(total_votes == max(total_votes))


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
  geom_label_repel(data = big_city, aes(x = state, y = share_for, 
                                        label = county, color = state), 
                   max.overlaps = Inf, size = 3, fill = "white", 
                   box.padding = 0) + 
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
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
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

