# upload libraries
pacman::p_load(tidyverse, here, scales, gganimate)

# get data
tuesdata <- tidytuesdayR::tt_load('2020-10-13')
datasaurus_dozen <- tuesdata$datasaurus

datasaurus_dozen %>%
  filter(dataset == "dino") %>%
  mutate(y1 = ifelse(y < 50 & y > 25 & x < 60, 
                     y-2,y)) %>%
  ggplot(aes(x = x,y = y1)) +
  geom_point(aes(color = y < 50 & y > 25 & x < 60))+
  theme_void()+
  theme(legend.position = "none")


close_mouth <- datasaurus_dozen %>%
  filter(dataset == "dino") %>%
  transmute(dataset = "dino2", x, 
            y = ifelse(y < 50 & y > 25 & x < 60, y-2,y))

open_mouth <- datasaurus_dozen %>%
  filter(dataset == "dino") %>%
  transmute(dataset = "dino3", x, 
            y = ifelse(y < 50 & y > 25 & x < 60, y+2,y))

dino_animated <- bind_rows(close_mouth,open_mouth)

dino_gganimated <- dino_animated %>%
  ggplot(aes(x = x, y = y, color = y)) + 
  geom_point() +
  theme_void()  + 
  scale_color_gradient(low="blue", high="red",
                       guide = F) +
  transition_states(dataset, wrap = T,
                    transition_length = 1,
                    state_length = 1)

dino_gganimated <- dino_animated %>%
  ggplot(aes(x = x, y = y, color = y)) + 
  geom_point() +
  theme_void()  +
  scale_color_gradient(low="blue", high="red",
                       guide = F) +
  transition_states(dataset, wrap = T)

animate(dino_gganimated, nframes = 12)