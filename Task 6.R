library(dplyr)
library(tidyr)
options(scipen=999)

#load data
inf<-read.csv("Infectious Disease 2001-2014.csv")
#filter the data to only show san diego and san fransisco total rates of Dengue

sd_Dengue<-filter(inf, County=="San Diego", Disease=="Dengue", Sex=="Total")%>%
  pull(Rate)
sf_Dengue<-filter(inf, County=="San Francisco", Disease=="Dengue", Sex=="Total") %>%
  pull(Rate)


#chekc if data is normally dist
hist(sd_Dengue, main = "Dengue Rates in San Diego", xlab = "Rate", col = "lightblue")
shapiro.test(sd_Dengue)

#do mann whitney test to chck if stat different as its not norm dist

mann_whitney_test <- wilcox.test(sd_Dengue, sf_Dengue)
length (sf_Dengue)
length (sd_Dengue)
 print(mann_whitney_test)
 