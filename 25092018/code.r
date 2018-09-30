library(tidyverse)
library(ggalluvial)
library(grid)
library(gridExtra)


threat <- read_csv("table_2.csv")
source <- read_csv("table_4.csv")

threat_gdp <- read_csv("table_3.csv")

#combine data
threat %>% select(-rank) %>% rename(threat="invasion_cost") -> threat

source %>% select(-rank) %>% rename(source="invasion_cost") -> source

threat %>% left_join(source) -> alluvial_data

alluvial_data %>% gather(key="key",value="value",-country) %>%
  group_by(key) %>%
  arrange(key,-value) %>% 
  mutate(ranking=row_number(),ranking=if_else(ranking>9,10,as.double(ranking))) -> alluvial_data
  
alluvial_data %>% mutate(country=if_else(ranking!=10,country,"All Others")) %>%
  group_by(key,country) %>% 
  summarise(value=sum(value)) %>%
  ungroup() %>%
  arrange(key,-value) -> alluvial_data

alluvial_data %>% filter(key=="source") -> top_source
alluvial_data %>% filter(key!="source") -> top_threat

top_source %>% pull(country) -> top_sources
top_threat %>% pull(country) -> top_threats

top_sources[top_sources %in% top_threats] -> both


alluvial_data %>% 
  mutate(both=if_else(country%in%both,"Both","Not Both")) %>%
  mutate(country=if_else(country=="Russian","Russia",country)) %>%
  ggplot()+
  geom_col(aes(x=key,y=value,group=key,fill=both),colour="black",alpha=0.25) +
  geom_text(aes(x=key,y=value,label=country),position=position_stack(vjust=0.5),size=2.5) +
  scale_fill_manual(values=c("blue","white"),guide=FALSE) +
  scale_y_continuous("",labels=scales::dollar) +
  scale_x_discrete("",labels=c("Major sources of damage\nby invasive species","Countries most under threat\n by invasive species")) +
  theme_minimal() +
  theme(text=element_text(colour="grey50"),
        panel.grid=element_blank(),
        axis.text.x=element_text(size=6.5)) -> sideplot


threat_gdp %>% filter(rank<=10) %>% mutate(country=if_else(row_number()==4,"Guinea-Bissau",country)) %>%
  ggplot(aes(x=reorder(country,-rank),y=gdp_proportion)) +
  geom_col(fill="green",alpha=0.25) +
  geom_text(aes(label=scales::percent(gdp_proportion)),hjust=1,colour="grey50")+
  scale_y_continuous("% GDP at risk from invasive species",labels=NULL)+
  scale_x_discrete("")+
  theme_minimal() +
  theme(text=element_text(colour="grey50"),
        panel.grid=element_blank())+
  coord_flip() -> mainplot

top <- textGrob(
"International trade puts countries at risk from invasive species - the biggest sources are the major world economies, and they stand to lose most in absolute terms.
However, smaller economies (particularly in Africa) are more at risk relative to the size of their economies",
  gp=gpar(fontsize=14, col="grey50"),hjust=0,x=0.01)

png('Plot.png', width=1626, height=874, res=100,type="cairo-png")
grid.arrange(top=top,sideplot,mainplot,ncol=2,nrow=1,widths=c(0.25,0.75),
             bottom = textGrob(
               "By @stevejburr - Data Source= Paini et al, 2016",
               gp = gpar(fontface = 3, fontsize = 9, col="grey50"),
               hjust = 1,
               x = 1
             ),padding = unit(1.5, "line"))
dev.off()
