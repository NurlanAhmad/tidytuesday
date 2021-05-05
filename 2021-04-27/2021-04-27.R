library(tidyverse)
library(tidytext)
library(showtext)
library(viridis)

font_add_google("Rubik")
showtext_auto()

data <- tidytuesdayR::tt_load(2021, week = 18)
departures <- data$departures %>% janitor::clean_names()



d_notes <-
  departures %>%
  select(departure_code, notes, ceo_dismissal) %>%
  filter(!is.na(departure_code), departure_code %in% 1:6) %>%
  mutate(
    departure_code = factor(case_when(
                               departure_code == 1 ~ "death",
                               departure_code == 2 ~ "illness",
                               departure_code == 3 ~ "dismissed for job performance",
                               departure_code == 4 ~ "dismissed for legal violations or concerns",
                               departure_code == 5 ~ "retired",
                               departure_code == 6 ~ "new opportunity",
                               departure_code == 7 ~ "other",
                               departure_code == 8 ~ "missing",
                               departure_code == 9 ~ "execucomp error"), order = TRUE)
    )


other_words <- data.frame(word = c("chairman",
                                   "executive",
                                   "officer",
                                   "ceo",
                                   "board",
                                   "president",
                                   "founder",
                                   "chief",
                                   "mr",
                                   "directors",
                                   "inc",
                                   "corporation",
                                   "company's",
                                   "also",
                                   "director",
                                   "founder",
                                   "sec",
                                   "corp",
                                   "announced",
                                   "years",
                                   "year",
                                   "co",
                                   "said",
                                   1:10))


d_notes %>% 
  unnest_tokens(word, notes) %>%
  anti_join(get_stopwords()) %>%
  anti_join(other_words) %>%
  count(departure_code, word, sort = TRUE) %>%
  group_by(departure_code) %>%
  slice_max(n, n = 15, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, n, departure_code)) %>%
  ggplot(aes(n, word, fill = log(n))) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  scale_y_reordered() +
  scale_fill_viridis() +
  facet_wrap(~departure_code, scales = "free") +
  labs(
    x = "Word frequency",
    y = NULL,
    title = "Frequently used words* in description by reason for CEO departure",
    caption = "*Stop words and other irrelevant words removed"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Rubik", size = 40),
    plot.title = element_text(family = "Rubik", face = "bold", color = "black", size = 85, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 50),
    strip.text = element_text(face = "bold", size = 40),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.caption = element_text(lineheight = 0.2, size = 40),
    plot.margin = margin(10, 10, 10, 10)
  )


  
  
















ceo_waffle <- 
  departures %>%
  mutate(
    reason = case_when(
      departure_code == 1 ~ "death",
      departure_code == 2 ~ "illness",
      departure_code == 3 ~ "dismissed for job performance",
      departure_code == 4 ~ "dismissed for legal violations or concerns",
      departure_code == 5 ~ "retired",
      departure_code == 6 ~ "new opportunity",
      departure_code == 7 ~ "other",
      departure_code == 8 ~ "missing",
      departure_code == 9 ~ "execucomp error",)
  ) %>%
  drop_na(reason) %>%
  filter(reason != "execucomp error") %>%
  filter(reason != "other") %>%
  filter(reason != "missing") %>%
  filter(reason > 2017) %>%
  count(reason, sort = TRUE) %>%
  mutate(reason = factor(reason))


waffle <- 
  ceo_waffle %>% 
  ggplot(aes(x = reason, y = n, fill = reason, values = n)) +
  geom_tile() + 
  scale_fill_manual(name = NULL,
                    values = c("retired" = "#ffc93c",
                               "dismissed for job performance" = "#ffee93",
                               "dismissed for legal violations or concerns" = "#grey",
                               "illness" = "#1b2021",
                               "new opportunity" = "#31326f",
                               "death" = "white")) +
  coord_equal() + 
  labs(title = "Reasons CEOs leave the company", subtitle = "counting from 2017 until 2020") + 
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 7, colour = "#8E8E8E", family = "Bahnschrift"),
    legend.spacing.x = unit(0.2, "cm"),
    legend.key.size = unit(0.7, "line"),
    legend.key = element_rect(size = 0.1, color = NA),
    legend.background = element_rect(color = NA),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 20, hjust = 0.5, family = "Bahnschrift"),
    plot.subtitle = element_text(hjust = 20, family = "Bahnschrift")
  )
  
























































