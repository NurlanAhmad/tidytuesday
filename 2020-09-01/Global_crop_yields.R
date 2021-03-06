# load libraries -----------------------------------------------------------
library(tidyverse)
library(ggpubr)
library(viridis)

# get Tidytuesday data -----------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2020, week = 36)
key_crop_yields <- tuesdata$key_crop_yields

#filter for US and reshape to long
aze_crop <- key_crop_yields %>%
  filter(Entity == "Azerbaijan") %>%
  gather(Crop, Tonnes, 4:14)

aze_crop$Crop %>% str_remove('\\(tonnes per hectare\\)') %>%
                  str_extract("[a-zA-Z]+") -> aze_crop$Crop #remove text after crop name


# Heatmap -----------------------------------------------------------------
p1 <- aze_crop %>%
  filter(Tonnes != "NA") %>%
  ggplot(aes(x= Crop, y = Year, fill = Tonnes))+
  geom_tile(color = "grey")+
  coord_flip()+
  scale_y_continuous(breaks = seq(1960, 2018, by = 5))+
  scale_fill_viridis(discrete=FALSE, breaks = c(5,15,25,35,45)) +
  theme(
    panel.background = element_rect(fill = "#4f5d75"),
    plot.background = element_rect(fill = "#4f5d75", color = NA),
    panel.grid = element_blank(),
    axis.text = element_text(color = "white", size = 12, face = "bold"),
    axis.text.y = element_text(margin = margin(r=-15)),
    axis.title = element_text(color = "white", size = 15),
    axis.ticks = element_line(color="white"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    legend.background = element_blank(),
    legend.margin=margin(t = -.7, r = 1.5, unit='cm')
  )+
  labs(
    x = "",
    y = ""
  )


# Line plot ---------------------------------------------------------------
p2 <- aze_crop %>%
  filter(Tonnes != "NA") %>%
  ggplot(aes(x= Year, y = Tonnes, color = Crop))+
  geom_line(size = 1)+
  scale_x_continuous(breaks = seq(1960, 2018, by = 5))+
  scale_color_viridis(discrete=TRUE, option = "plasma") +
  theme(
    panel.background = element_rect(fill = "#4f5d75"),
    plot.background = element_rect(fill = "#4f5d75", color = NA),
    panel.grid = element_blank(),
    axis.text = element_text(color = "white", size = 10, face = "bold"),
    axis.title.y = element_text(margin = margin(r=10)),
    axis.title = element_text(color = "white", size = 14),
    axis.ticks = element_line(color="white"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(color = "white", face = "bold"),
    legend.title = element_text(color = "white", face = "bold"),
    legend.key = element_rect(fill = "#4f5d75"),
    legend.background = element_rect(fill = "#4f5d75"),
    legend.margin=margin(t = -.5, unit='cm'),
    plot.title = element_text(color = "white", size = 20, hjust = .5, face = "bold"),
    plot.subtitle = element_text(color = "white", size = 14, hjust = .5),
    plot.margin = unit(c(.5,.5,.5,.5), "lines")
  )+
  guides(color = guide_legend(nrow = 1))+
  labs(
    x = "",
    y = "Tonnes",
    title = "Republic of Azerbaijan",
    subtitle = "Azerbaijan crop yields over time (1961-2018)"
  )

p <- ggarrange(p2, p1, ncol = 1, nrow = 2, heights = c(1.5, 1.6))