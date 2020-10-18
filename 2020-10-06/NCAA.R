# load libraries

pacman::p_load(tidyverse, lubridate, data.table)


# load data

tuesdata <- tidytuesdayR::tt_load('2020-10-06')
tournament <- tuesdata$tournament


# exploratory analysis

tournament %>%
  filter(year==1990) %>%
  select(full_percent, full_w, full_l) %>%
  summarize(
    range_pct = range(full_percent),
    range_w = range(full_w),
    range_l = range(full_l)
  )

tournament %>%
  filter(year == 1990) %>%
  arrange(desc(full_percent))

tournament %>% 
  select(year, school, full_percent) %>%
  group_by(school) %>%
  summary()

tournament %>%
  select(year, school, full_percent) %>%
  group_by(school) %>%
  tally(sort = TRUE) %>%
  ungroup() %>%
  head(30) %>%
  pull(school) -> name_vector

# visualizations

tournament %>% 
  select(year, school, full_percent) %>%
  filter(school %in% name_vector) %>%
  ggplot(aes(x = year, y = full_percent)) + 
  geom_line(aes(color = school)) + 
  facet_wrap(~school)+
  guides(color=FALSE) + 
  theme_light()


tournament %>% 
  select(year, school, full_percent) %>%
  filter(school %in% name_vector) %>%
  mutate(direction = ifelse(full_percent > 74.2, 'up', 'down')) %>%
  ggplot(aes(x=year, y=full_percent))+
  geom_line()+
  geom_point()+
  geom_ribbon(aes(x = year, ymin = 74.2, ymax=full_percent, fill="red"), alpha = 0.15)+
  geom_ribbon(aes(x = year, ymin = full_percent, ymax = 74.2, fill="blue"), alpha = 0.15) +
  facet_wrap(~school)+
  geom_hline(yintercept = 74, color = 'black')+
  theme_light()


tournament %>%
  select(year, school, full_percent) %>%
  # use name_vector to filter for programs with most data availability
  filter(school %in% name_vector) %>%
  mutate(
    median_percent = 74.2,
    # key to having two shades between two lines
    z = ifelse(full_percent > median_percent, full_percent, median_percent)
  ) %>%
  ggplot(aes(x=year, y=full_percent)) +
  geom_line() +
  geom_ribbon(aes(ymin=median_percent, ymax=full_percent), fill='#46edc8') +
  # using z as ymax is the key to having two shaded colors
  geom_ribbon(aes(ymin=median_percent, ymax=z), fill='#374d7c') +
  geom_hline(yintercept = 74.2) +
  facet_wrap(~school) +
  theme(
    strip.text.x = element_text(face = 'bold', size = 10, family = 'Lato'),
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill = 'whitesmoke'),
    panel.background = element_rect(fill = 'whitesmoke'),
    plot.title = element_text(face = 'bold', family = 'Lato', size = 20)
  ) +
  labs(
    x = 'Years',
    y = 'Total Sum Win / Loss Percent (%)',
    title = "Sustained Excellence among NCAA Women's College Basketball Programs",
    subtitle = "These 30 programs were chosen based on data availability. They are benchmarked against the median win / loss percentages (%)\namong all programs. UConn is the gold standard for sustained excellence as is Stanford, Tennesee and Louisiana Tech.\nThese programs are consistently above the median, in some cases achieving high win/loss percentages over decades.\n"
  )
































