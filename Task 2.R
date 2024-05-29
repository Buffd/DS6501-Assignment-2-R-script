library(dplyr)
options(scipen=999)

#load data
inf<-read.csv("Infectious Disease 2001-2014.csv")
#filter data to only have HIV disease and are female cases in 2005
fem_hiv05<-filter(inf, Disease=="HIV", Sex=="Female", Year=="2005")
#filter out highest rate
highest_rate <- fem_hiv05 %>% filter(Rate==max(Rate))
#show highest rate
highest_rate

#check correlation between number of females inf and pop
correlation <- cor(fem_hiv05$Rate, fem_hiv05$Population)
