# libraries
library(tidyverse)
library(ggbeeswarm)
library(hrbrthemes)
library(ggrepel)

# read data
df = read_rds("output_data/aca_refs.rds")


# make swarm plot
ggplot(df, aes(x = state, y = share_for, fill = state, size = total_votes)) + 
  geom_quasirandom(shape = 21, 
                   color = "white", alpha = .8) + 
  theme_ipsum() + 
  coord_flip() + 
  theme(legend.position = "none") + 
  scale_fill_viridis_d(option = "magma", end = .8) + 
  labs(title = "Support for Medicaid Expansion", 
       subtitle = "County-level results from Medicaid expansion referendums in five states.", 
       x = NULL, 
       y = "% of voters in favor of expansion") + 
  scale_y_continuous(limits = c(0, 100), 
                     labels = scales::percent_format(scale = 1)) + 
  geom_hline(yintercept = 50, lty = 2, size = 1)


# output
ggsave("figures/vote-dist.pdf", device = cairo_pdf)
ggsave("figures/vote-dist.png", device = "png")
