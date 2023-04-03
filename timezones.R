library(tidyverse)
library(lubridate)
library(sf) # simple features - for shape files
library(ggnewscale)
library(extrafont)

# Load and Clean Tidy Tuesday data
tuesdata <- tidytuesdayR::tt_load('2023-03-28')
timezones <- tuesdata$timezones
transitions <- tuesdata$transitions %>% 
  mutate(begin = if_else(begin == "-32767-01-01T00:00:00Z", "0000-01-01T00:00:00Z", begin),
         end = if_else(end == "32767-12-31T00:00:00Z", "2023-03-28T00:00:00Z", end),
         across(c(begin, end), as_date))

# Load Timezone coordinate data (downloaded from https://github.com/evansiroky/timezone-boundary-builder/releases)
tz_shapes <- st_read("/Users/louisteitelbaum/Projects/tidy_tuesday/timezones", 
                     "combined-shapefile") %>% rename(zone = tzid)
# Load Country data
world_shapes <- map_data("world")

# WRANGLE
plot_data <- timezones %>% select(zone) %>% 
  # For each timezone:
  # When (if ever) did it first observe daylight savings?
  left_join(transitions %>% 
              filter(dst == TRUE) %>% 
              group_by(zone) %>% 
              summarise(earliest_dst = min(begin))) %>% 
  # Has it observed daylight savings in the last year?
  left_join(transitions %>% 
              filter(end > today()-360) %>% 
              group_by(zone) %>% 
              summarise(dst_now = any(dst))) %>% 
  # Join Timezone coordinate data
  right_join(tz_shapes) %>% 
  # Label if never observed
  mutate(earliest_dst = replace_na(earliest_dst, today()),
         dst_now = replace_na(dst_now, FALSE))

# PLOT
p <- plot_data %>% 
  ggplot() + 
    # Underlying Map of World Countries
    geom_map(aes(map_id = region), 
             fill = "#538a84", color = "#2e4d49",
             data = world_shapes, map = world_shapes) + 
    # Time-zones Now Observing DST (Blue)
    geom_sf(data = plot_data %>% filter(dst_now == TRUE),
            aes(fill = earliest_dst, geometry = geometry), 
            alpha = .8, color = NA) +
    scale_fill_gradient(name = "First Daylight Savings          \n(Now Observing)    ",
                        low = "#08306b", high = "#deebf7",
                        limits = c(ymd(19000101), today()),
                        breaks = c(ymd(19000101), ymd(19200101), ymd(19400101), 
                                   ymd(19600101), ymd(19800101), ymd(20000101), ymd(20200101)),
                        labels = c("1900", "1920", "1940", "1960", "1980", "2000", "2020")) + 
    # Time-zones No Longer Observing DST (Green)
    new_scale_fill() + 
    geom_sf(data = plot_data %>% filter(dst_now == FALSE),
            aes(fill = earliest_dst, geometry = geometry), alpha = .7, color = NA) +
    scale_fill_gradient(name = "First Daylight Savings          \n(No Longer Observing)",
                        low = "#00441b", high = "#e5f5e0",
                        limits = c(ymd(19000101), today()),
                        breaks = c(ymd(19000101), ymd(19200101), ymd(19400101), 
                                   ymd(19600101), ymd(19800101), ymd(20000101), today()),
                        labels = c("", "", "", "", "", "", "Never\nObserved")) + 
    # Labels and Aesthetics
    labs(x = "", y = "",
         title = "The Spread of Daylight Savings",
         subtitle = str_wrap("Many regions around the globe adopted daylight savings time between 1916 to 1945. More joined the trend around 1980. Now though, all but North America and Europe have mostly done away with the time change.", 105),
         caption = "Louis Teitelbaum | #TidyTuesday | Source: IANA tz database, OpenStreetMap") +
    theme_void() +
    scale_y_continuous(limits = c(-75, 90)) + 
    theme(panel.background = element_rect(fill = "#c1e8e2", colour = "#c1e8e2"),
          panel.grid = element_blank(),
          plot.title = element_text(family = "Avenir Next", colour = "#67000d", size = 40,
                                    hjust = 0.6),
          plot.subtitle = element_text(family = "Avenir Next", colour = "#67000d", size = 12,
                                       hjust = 0.6, lineheight = unit(1, "in")),
          plot.caption = element_text(family = "Avenir Next", colour = "#67000d", size = 12),
          plot.margin = margin(t = 0, b = .1, l = 0, r = .8, unit = "in"),
          legend.margin = margin(t = -0.05, b = 0, l = .2, r = .2, unit = "in"),
          legend.box.margin = margin(t = 0.2, b = 0.2, l = .2, r = .2, unit = "in"),
          # legend.key.height = unit(.5, "in"),
          legend.key.width = unit(1.9, "in"),
          legend.title = element_text(family = "Avenir Next", colour = "#67000d", size = 12),
          legend.text = element_text(family = "Avenir Next", colour = "#67000d", size = 10),
          legend.box = "vertical",
          legend.position = "bottom")

# Export
ggsave(plot = p, filename = file.path("plots/", "daylight_savings.png"), 
       dpi = 400, width = 14, height = 10, bg = "#c1e8e2", units = "in")
