library(tidyverse)
library(ggrepel)

tuesdata <- tidytuesdayR::tt_load(2022, week = 12)
babynames <- tuesdata$babynames

plot_data1 <- babynames %>% 
  filter(year %in% c(max(year), 2010, 2005, 2000)) %>% 
  group_by(sex, year) %>% 
  arrange(desc(prop), .by_group = TRUE) %>% 
  mutate(rank = 1:n()) %>% 
  slice_head(n = 5)

# plot
p <- plot_data1 %>% 
  group_by(year, rank) %>% 
  mutate(most_popular = prop == max(prop),
         x = if_else(most_popular, prop + .005, prop - .005)) %>% 
  ggplot(aes(prop, rank, label = name, color = sex)) +
    geom_point(aes(color = sex), size = 11, alpha = .5) +
    theme_minimal() +
    guides(color = "none") +
    labs(title = "How Common is \"Too Common\"?",
         subtitle = "The most popular girls names are more popular than the most popular boys names. Not long ago, the reverse was true.",
         x = "...out of 1000 babies", y = "Rank",
         caption = "Louis Teitelbaum | #TidyTuesday | Source: `babynames` R package") +
    facet_wrap(~year, nrow = 1) + 
    scale_y_reverse(breaks = 1:10,
                    minor_breaks = NULL,
                    limits = c(5.5, .5)) +
    scale_x_continuous(breaks = c(.006, .008, .01, .012, .014),
                       minor_breaks = NULL,
                       labels = c("6", "8", "10", "12", "14"),
                       limits = c(.001, .022)
                       ) +
    theme(plot.title = element_text(family = "Avenir Next", colour = "#67000d", size = 30,
                                    hjust = 0.6),
          plot.subtitle = element_text(family = "Avenir Next", colour = "#67000d", size = 12,
                                       hjust = 0.6, lineheight = unit(1, "in")),
          plot.caption = element_text(family = "Avenir Next", colour = "#67000d", size = 8,
                                      margin = margin(t = .1, unit = "in")),
          plot.margin = margin(t = .2, b = .2, l = .2, r = .2, unit = "in"),
          axis.title = element_text(family = "Avenir Next", colour = "#67000d", size = 10),
          axis.text = element_text(family = "Avenir Next", colour = "#67000d", size = 10),
          strip.text = element_text(family = "Avenir Next", colour = "#67000d", size = 10))

# Export
ggsave(plot = p, filename = file.path("plots/", "baby_names.png"), 
       dpi = 500, width = 32, height = 10, bg = "white", units = "cm")



# Second plot
plot_data2 <- babynames %>% 
  filter(year %in% c(2017, 2000, 1980, 1960, 1940, 1920, 1900, 1880)) %>% 
  group_by(year) %>% 
  arrange(desc(prop), .by_group = TRUE) %>% 
  mutate(rank = 1:n()) %>% 
  slice_head(n = 10)

p2 <- plot_data2 %>% mutate(sex = if_else(sex == "M", "Boys", "Girls")) %>% 
  ggplot(aes(prop, rank, color = as.factor(year), group = as.factor(year))) +
  geom_segment(aes(xend = -.1, yend = rank), linewidth = 12, lineend = "round") +
  geom_text(aes(label = name, x = prop -.0028), color = "#9ECAE1", family = "Avenir Next",
            data = plot_data2 %>% filter(year %in% c(2017, 1980) & rank < 4)) + 
  geom_text(aes(label = name, x = prop -.0028), color = "#4292C6", family = "Avenir Next",
            data = plot_data2 %>% filter(year %in% c(1940, 1880) & rank < 4)) + 
  theme_minimal() +
  labs(title = "Popular Names are Getting Less Popular",
       subtitle = "In 1880 the most popular baby names were very popular. In 2017 nobody wants to name their baby something so cliche.",
       x = "...out of 1000 babies", y = "Rank",
       caption = "Louis Teitelbaum | #TidyTuesday | Source: `babynames` R package") +
  scale_y_reverse(breaks = 1:10,
                  minor_breaks = NULL,
                  limits = c(10.5, .5)) +
  scale_x_continuous(breaks = c(.01, .02, .03, .04, .05, .06, .07),
                     minor_breaks = NULL,
                     labels = c("10", "20", "30", "40", "50", "60", "70")) +
  coord_cartesian(xlim = c(0, .08)) +
  scale_color_brewer(palette = "Blues", direction = -1) + 
  theme(plot.title = element_text(family = "Avenir Next", colour = "#67000d", size = 30,
                                  hjust = 0.6),
        plot.subtitle = element_text(family = "Avenir Next", colour = "#67000d", size = 12,
                                     hjust = 0.6, lineheight = unit(1, "in")),
        plot.caption = element_text(family = "Avenir Next", colour = "#67000d", size = 8,
                                    margin = margin(l = .1, unit = "in"), hjust = 1.13),
        plot.margin = margin(t = .2, b = .2, l = .2, r = .2, unit = "in"),
        axis.title = element_text(family = "Avenir Next", colour = "#67000d", size = 10),
        axis.text = element_text(family = "Avenir Next", colour = "#67000d", size = 10),
        strip.text = element_text(family = "Avenir Next", colour = "#67000d", size = 10),
        legend.key.width = unit(.1, "in"),
        legend.title = element_blank(),
        legend.key.height = unit(.39, "in"),
        legend.text = element_text(family = "Avenir Next", colour = "#67000d", size = 10,
                                   margin = margin(l = .15, unit = "in")))

# Export
ggsave(plot = p2, filename = file.path("plots/", "baby_names2.png"), 
       dpi = 500, width = 30, height = 15, bg = "white", units = "cm")

