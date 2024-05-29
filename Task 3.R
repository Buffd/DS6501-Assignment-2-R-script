library(dplyr)
options(scipen=999)

#load data
inf<-read.csv("Infectious Disease 2001-2014.csv")
#filter data to have atleast 10 cases of Malaria in 2010
mal_case10<-filter(inf, Disease=="Malaria", Count>=10, Year=="2010", Sex=="Total")
