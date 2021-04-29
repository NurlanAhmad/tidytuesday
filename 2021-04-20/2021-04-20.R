# libraries --------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(lubridate)
library(extrafont)
loadfonts(device = "win")

# import data ------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix_titles


# data wrangling ---------------------------------------------------------

netflix %>% mutate(days = date_added %>% parse_number(),
                   months = date_added %>% word(),
                   months_num = months %>% match(month.name) %>% as.numeric(),
                   year1 = date_added %>% str_extract_all("\\d{4}"),
                   added_date = make_date(year1, months_num, days),
                   added_year = year(added_date)) %>%
            select(-c(days, months, months_num, year1)) -> data




data %>% 
  filter(type == "Movie") %>%
  ggplot(aes(x = added_year, y = release_year)) +
  geom_jitter(show.legend = FALSE,
              alpha = 0.3,
              width = 0.3,
              color = "red") +
  scale_y_continuous(limits = c(1940, 2021), position = "left", breaks = c(1940, 2020)) +
  scale_x_continuous(breaks = c(2008, 2020)) +
  ggtitle("Movies") +
  xlab("Year Added on Netflix") +
  ylab("Year Released") +
  theme(plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black", color = "black"),
        axis.line = element_line(color = "gray30", linetype = "dashed", size = 1),
        axis.ticks = element_blank(),
        axis.text.x = element_text(colour = "gray30"),
        axis.title.y = element_text(color = "gray30"),
        axis.title.x = element_text(color = "gray30"),
        axis.text.y = element_text(color = "grey30"),
        panel.grid = element_blank(),
        plot.title = element_text(color = "red", hjust = 0.5, size = 16)) -> p_movies


data %>% 
  filter(type == "TV Show") %>%
  ggplot(aes(x = added_year, y = release_year)) +
  geom_jitter(show.legend = FALSE,
              alpha = 0.3,
              width = 0.3,
              color = "red") +
  scale_y_continuous(limits = c(1940, 2021), position = "left", breaks = c(1940, 2020)) +
  scale_x_continuous(breaks = c(2008, 2020)) +
  ggtitle("TV Shows") +
  xlab("Year Added on Netflix") +
  ylab("Year Released") +
  theme(plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black", color = "black"),
        axis.line = element_line(color = "gray30", linetype = "dashed", size = 1),
        axis.ticks = element_blank(),
        axis.text.x = element_text(colour = "gray30"),
        axis.title.y = element_text(color = "gray30"),
        axis.title.x = element_text(color = "gray30"),
        axis.text.y = element_text(color = "grey30"),
        panel.grid = element_blank(),
        plot.title = element_text(color = "red", hjust = 0.5, size = 16)) -> p_shows

p_movies + p_shows +
  plot_annotation(title = "Netflix",
                  subtitle = "Content original release years vs. year added to Netflix\n",
                  theme = theme(plot.title = element_text(size = 30, hjust = .5, color = "red"),
                                plot.subtitle = element_text(size = 16, hjust = .5, color = "gray30"),
                                plot.margin = unit(c(1,1,1,1), "cm"),
                                plot.background = element_rect(color = "black", fill = "black"))) -> viz

viz































































































