library(tidyverse)
library(gridExtra)
library(grid)

data1 <- read_csv("hypoxia.csv")
data2 <- read_csv("us-airports.csv")

#col 5/7/8- show cut off with >90% write commentary based on article

#create an altitude in ft column, duplicate on other side in mts

#drop first row which is just labelling
data <- data1[-1,c(6,7,9,10)]
data$heights <- c(0,10000,20000,30000,40000,50000)

#do % sat O2 with > 90
# and Alv pCO2 >35

dataO2 <- data[,c(1,3,5)]
dataCO2 <- data[,c(2,4,5)]

dataO2 %>% gather(key="key",value="value",-heights) %>%
  mutate(value=as.numeric(value),
         value=if_else(is.na(value),0,value)) %>%
  ggplot() +
  geom_hline(aes(yintercept=90),colour="grey50") +
  scale_x_continuous("Height / ft",sec.axis = sec_axis(~.*0.3048, name="Height / m"),labels=scales::comma) +
  scale_y_continuous("% of haemoglobin molecules saturated with O2",breaks=c(25,50,75,90,100)) +
  scale_colour_manual("",values=c("#BCB6FF","#97F9F9"),labels=c("Without oxygen","With oxygen")) +
  geom_point(aes(y=value,x=heights,colour=key),size=3) +
  annotate("text",x=10000,y=83,label="If flying without oxygen,\nabove 10,000ft the oxygen content \nof the blood drops dangerously low",
           colour="grey50",size=3.5) +
  annotate("text",x=40000,y=75,label="Even with a supply of pure oxygen, \nwithout a pressurised mask it \nbecomes dangerous to fly above 40,000ft",
           colour="grey50",size=3.5) +
  annotate("text",x=48000,y=91,label="Critical satuation value",size=2,colour="grey50")+
  theme_minimal()+
  theme(text=element_text(colour="grey50"),
        panel.grid = element_blank(),) +
  labs(title="Using oxygen helps, but above about 40,000ft a pressurised mask is required",
       subtitle="This graph shows the % of haemoglobin molecules in the blood which are fully saturated with oxygen at different heights") -> O2Plot




dataCO2 %>% gather(key="key",value="value",-heights) %>%
  mutate(value=as.numeric(value),
         value=if_else(is.na(value),0,value)) %>%
  ggplot() +
  geom_hline(aes(yintercept=35),colour="grey50") +
  scale_x_continuous("Height / ft",sec.axis = sec_axis(~.*0.3048, name="Height / m"),labels=scales::comma) +
  scale_y_continuous("Partial pressure of CO2 in the alveoli / mm Hg",breaks=c(25,50,75,90,100)) +
  scale_colour_manual("",values=c("#BCB6FF","#97F9F9"),labels=c("Without oxygen","With oxygen")) +
  geom_point(aes(y=value,x=heights,colour=key),size=3) +
  annotate("text",x=10000,y=30,label="If flying without oxygen,\nabove 10,000ft the CO2 pressure\nbecomes a problem",
           colour="grey50",size=3.5) +
  annotate("text",x=40000,y=25,label="Even with a supply of pure oxygen, \nwithout a pressurised mask it \nbecomes dangerous to fly above 40,000ft.\nWithout oxygen, the pressure of CO2\ndramatically falls further at this altitude.",
           colour="grey50",size=3.5) +
  annotate("text",x=48000,y=36,label="Critical CO2 pressure",size=2,colour="grey50")+
  theme_minimal()+
  theme(text=element_text(colour="grey50"),
        panel.grid = element_blank(),) +
  labs(title="The partial pressure of CO2 is also important, and this also falls with height.",
  subtitle="If it is too low, then the blood vessels in the brain start to constrict, which negatively impacts cognition, while also hindering the release of\noxygen by your red blood cells. When it falls bellow 20mm Hg you start to feel mentally clouded (>35 is ideal), but this is not to do with lack of oxygen.") -> CO2plot

png("plot.png",height=1100,width=800,type="cairo-png")
grid.arrange(O2Plot,CO2plot,
           top = textGrob(
               "The levels of both CO2 and O2 become important when flying above 10,000ft",
               gp = gpar(fontface=2,fontsize=14,col="grey50"),
               hjust=0.5
             ),
           bottom = textGrob(
             "#TidyTuesday Design by - @stevejburr Data Source - Soaring Society of America/Nathan Cook",
             gp = gpar(fontface = 3, fontsize = 9,col="grey50"),
             hjust = 1,
             x = 1
           ),nrow=2,ncol=1,padding=unit(2,"lines"))
dev.off()
