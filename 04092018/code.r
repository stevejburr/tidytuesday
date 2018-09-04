#setwd("C:/Data/Personal/Tidy Tuesday/04092018")

library(tidyverse)
library(ggrepel)

data <- read_csv("fastfood_calories.csv")

#scatter of sodium / sat fat
#sodium
#calories
#sat_fat

#grab top 7 products by sodium
#grab top 7 products by sat_fat
#label both of these on chart

data %>% arrange(-sodium) %>% top_n(7,sodium) %>% select(item) %>% pull() -> big_sodium
data %>% arrange(-sat_fat) %>% top_n(7,sat_fat) %>% select(item) %>% pull() -> big_sat_fat
data %>% filter(sodium>=2300 & sat_fat>=20) %>% select(item) %>% pull() -> danger_zone

labels <- c(big_sodium,big_sat_fat,danger_zone) %>% unique()

data %>% mutate(label=if_else(item %in% labels,item,"")) -> data


#line at 20g of sat_fat = the recommended amount
#line at 2300 mg of sodium = the recommended amount

#colours

# Arbys - #d71921
# Burger King - #ec7801
# Chick Fil-A - #5b6770
# Dairy Queen - #009eb7
# Mcdonalds - #fcb827
# Sonic - #fcdd2a
# Subway - #0f9246
# Taco Bell - #682a8a

ourColours <- c("#d71921","#ec7801","#5b6770","#009eb7","#fcb827","#fcdd2a","#0f9246","#682a8a")

data %>% select(restaurant,label,sodium,calories,sat_fat) %>%
  ggplot(aes(x=sodium,y=sat_fat)) +
  geom_rect(data=data[1,],aes(ymin=20,ymax=55,xmin=0,xmax=2300),alpha=0.5,fill="grey90")+
  geom_rect(data=data[1,],aes(ymin=0,ymax=20,xmin=2300,xmax=6500),alpha=0.5,fill="grey90")+
  geom_rect(data=data[1,],aes(ymin=20,ymax=55,xmin=2300,xmax=6500),alpha=1,fill="grey90")+
  geom_point(aes(size=calories,col=as.factor(restaurant))) +
  geom_text_repel(aes(label=label),size=3,col="grey50") +
  geom_hline(yintercept = 20,col="grey50",linetype="dashed") +
  geom_label(data=data[1,],aes(x=6200,y=21,label="Daily RDA"),alpha=1,size=3,fill=NA,hjust=1,col="grey50",label.size=NA) +
  geom_vline(xintercept= 2300,col="grey50",linetype="dashed") +
  geom_label(data=data[1,],aes(x=2310,y=1,label="Daily RDA"),alpha=0.5,size=3,fill=NA,hjust=0,col="grey50",label.size=NA) +
  scale_y_continuous("Saturated Fat (g)")+
  scale_x_continuous("Sodium (mg)")+
  scale_size("Total Calories") +
  scale_colour_manual("Restaurant",values=ourColours)+
  guides(size = guide_legend(override.aes = list(colour="grey50", alpha = 1))) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text = element_text(colour="grey50")) +
  labs(title="What are the unhealthiest fast food meals you can buy in the USA?",
       subtitle="Based on Saturated Fat and Sodium content",
       caption="Design by @stevejburr - Data from fastfoodnutrition.org - #TidyTuesday")


ggsave("plot.png",width=10,height=10,dpi="retina")
