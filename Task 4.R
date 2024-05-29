library(dplyr)
library(ggplot2)
options(scipen=999)

#load data
inf<-read.csv("Infectious Disease 2001-2014.csv")
tub<-filter(inf,Disease=="Tuberculosis", Year>=2010, Year<=2012, Sex=="Total")
tub0<-filter(inf,Disease=="Tuberculosis", Year>=2010, Year<=2012, Sex=="Total", Count==0)
tub10<-filter(inf,Disease=="Tuberculosis", Year>=2010, Year<=2012, Sex=="Total", Count>=10)


case0<-tub0%>%group_by(County) %>% filter(n_distinct(Year)==3)
unique<-unique(case0$County)

#calc total ratios for 0 cases and >=10
#total
tub_cases_summary <- tub %>%
  group_by(County) %>%
  summarise(total_cases = sum(Count))
tot_counties<-nrow(tub_cases_summary)
#0 cases
num_no_cases <- nrow(tub0)

#10 cases
num10 <- nrow(tub10)

#calc ratios
ratio_no_cases <- num_no_cases / tot_counties
ratio_at_least_10_cases <- num10 / tot_counties

#sum total num cases and av country per county between 2010-2012
tb_cases_population_summary <- tub %>%
  group_by(County) %>%
  summarise(total_cases = sum(Count), avg_population = mean(Population))
#calc correlation 
correlation <- cor(tb_cases_population_summary$total_cases, tb_cases_population_summary$avg_population)

#correlation results
cat("Correlation between total cases and population: ", correlation, "\n")

#plot correlations 
ggplot(tb_cases_population_summary, aes(x = avg_population, y = total_cases)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between Population and Tuberculosis Cases (2010-2012)",
       x = "Average Population",
       y = "Total Tuberculosis Cases")


