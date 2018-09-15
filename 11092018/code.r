library(tidyverse)

#read data
data <- read.csv("cats_vs_dogs.csv")

#get map data for US states
map <- map_data("state")

data %>% mutate(region=tolower(state),
                `Dogs` = dog_population/n_households,
                `Cats` = cat_population/n_households) %>%
  select(region,`Dogs`,`Cats`) %>%
  gather(key="key",value=`Avg per household`,-region) %>%
  ggplot(aes(map_id=region)) +
  facet_grid(key ~ ., switch="y") +
  geom_map(map=map,aes(fill=`Avg per household`),colour="white") +
  expand_limits(x = map$long, y = map$lat) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  scale_fill_distiller("Average pets per household",
                       type="seq",palette="Purples",direction=1,
                       breaks=c(0.25,0.45,0.65,0.85)) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        text = element_text(color="grey50"),
        panel.grid = element_blank(),
        strip.text.y= element_text(colour="grey50",
                                   angle=180)) +
  labs(title="Cats are most common in the North of the USA, while Dogs are prefered in South",
       caption="#TidyTuesday - Data from data.world/datanerd - Design by @stevejburr")

ggsave("plot.png",dpi="retina",width=8,height=6)




