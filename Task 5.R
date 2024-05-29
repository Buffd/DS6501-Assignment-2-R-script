library(dplyr)
library(tidyr)
options(scipen=999)

#load data
inf<-read.csv("Infectious Disease 2001-2014.csv")
cal_dataC<-filter(inf, County=="California", Disease=="Chlamydia", Sex=="Total")
cal_dataS<-filter(inf, County=="California", Disease=="Salmonellosis", Sex=="Total")
correlation <- cor(cal_dataC$Rate, cal_dataS$Rate)


ggplot(data = cal_dataC, aes(x = Rate, y = cal_dataS$Rate)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE,linetype= "dashed")+
  labs(title = "Relationship between Rates of Chlamydia and Salmonellosis in California",
       x = "Chlamydia Rate",
       y = "Salmonellosis Rate")
