library(tidyverse)
library(ggflags)
#library(ggrepel)

tuesdata <- tidytuesdayR::tt_load(2020, week = 36)


#Fertilizer

fertilizer <- tuesdata$cereal_crop_yield_vs_fertilizer_application %>% 
  filter(!is.na(Code)) %>% 
  select(Entity,Code,Year,fertilizer=`Nitrogen fertilizer use (kilograms per hectare)`,
         crop_yield=`Cereal yield (tonnes per hectare)`)

#Get the population from the land use dataset and filter out countries with less than 
#15MM inhabitants
land_use <- tuesdata$land_use_vs_yield_change_in_cereal_production %>% 
  filter(!is.na(Code),!is.na(`Cereal yield index`)) %>% 
  group_by(Entity,Code) %>% 
  summarise(population=last(`Total population (Gapminder)`)) %>% 
  ungroup() %>% 
  filter(population>10000000)

#Calculate the before and after, using the average of two years to be slightly
#more robust against outliers
final <- fertilizer %>% group_by(Entity,Code) %>% 
  summarise(fertilizer_n=sum(!is.na(fertilizer)),
            fertilizer=sum(fertilizer,na.rm=T),
            before=max((crop_yield[Year==2001]+crop_yield[Year==2000])/2),
            after=max((crop_yield[Year==2016]+crop_yield[Year==2017])/2)) %>% 
  mutate(perc_change_crop_yield=(after/before)-1) %>% 
  ungroup() %>% 
  filter(!is.nan(perc_change_crop_yield),!is.na(perc_change_crop_yield),
         fertilizer_n==16) %>% 
  inner_join(land_use,by=c("Code","Entity")) 

textcol <- "midnightblue"

final %>% 
       mutate(code_icons=case_when(Entity=="Chile" ~ "cl",
                                    Entity=="Netherlands" ~ "nl",
                                    Entity=="Egypt" ~ "eg",
                                    Entity=="France" ~ "fr",
                                    Entity=="Germany" ~ "de",
                                    Entity=="United States" ~ "us",
                                    Entity=="Poalnd" ~ "pl",
                                    Entity=="United Kingdom" ~ "gb",
                                    Entity=="South Korea" ~ "kr",
                                    Entity=="China" ~ "cn",
                                    Entity=="Colombia"~"co",
                                    Entity=="Bangladesh"~"bd",
                                    Entity=="Japan"~"jp",
                                    Entity=="Vietnam"~"vn",
                                    Entity=="Brazil"~"br",
                                    Entity=="Argentina"~"ar",
                                    Entity=="Iran"~"ir",
                                    Entity=="Turkey"~"tr"))%>%
       ggplot(aes(x=fertilizer,y=after))+
       geom_point(size=2)+
       geom_segment(aes(xend=fertilizer,yend=before))+
       geom_flag(aes(country=code_icons),size=8) +
       labs(x="Nitrogen fertilizer use (kg per hectare)",y="Crop yield (tonnes per hectare)",
                       title="Fertilizers and their effect on crop yield",
                       subtitle="How do crop yields change between 2002 and 2017 depending on the amount of fertilizers used?",
                       caption="Data from Our World In Data")+
       theme(plot.background = element_rect(fill = "ivory"),
                       panel.background = element_rect(fill="ivory2"),
                         axis.title = element_text(family = "sans" ,size=14,colour=textcol),
                         axis.text = element_text(family = "sans" ,size=14,colour=textcol),
                         plot.title = element_text(family = "sans", face = "bold", size = 20, colour = textcol),
                         plot.subtitle = element_text(family = "sans" ,size=16, colour = textcol))


# Potato map
library(tmap)
data(World)

potatoes <- tuesdata$key_crop_yields %>%
  filter(Year>=2008,!is.na(Code)) %>%
  group_by(Code,Entity) %>% 
  summarise(potato_tph=mean(`Potatoes (tonnes per hectare)`,na.rm=T))

World2 <- World %>% 
  left_join(potatoes,by=c("iso_a3"="Code"))

tm_shape(World2,projection=4326)+
  tm_polygons(col="potato_tph",palette="BuGn")

World2 %>% sf::st_transform(4326) %>% 
  ggplot()+geom_sf(aes(fill=potato_tph))
