#Load Libraries

library(ggplot2)
library(dplyr)
options(scipen=999)

#load data
inf<-read.csv("Infectious Disease 2001-2014.csv")

#filter data to only have Amebiasis disease and both male and female cases
tot_am<-filter(inf, Disease=="Amebiasis", Sex=="Total")

#################################
#Task 1 a
# total cases
#####################################

#calc slopes to see if there is any trends
slope <- tot_am %>%  group_by(County) %>%
  summarise(slope = coef(lm(Rate ~ as.numeric(Year)))[2])
#check what the highest slope is
high_slope <- slope %>%  filter(slope == max(slope))
print(high_slope)
#Filter out highest slope
Mendocino<-filter(tot_am, County=="Mendocino")

#plot the county with the highest slope. 
Myscatterplot <- ggplot(data = Mendocino, aes(x=Year, y=Rate)) +
  geom_point(size=5, shape=16) +
  geom_smooth(method=lm, se=FALSE,linetype= "dashed")+
  labs(title="Trend of Amebaiasis cases in Mendocino", x="Year", y="Rate")
print(Myscatterplot)
###################################
#male cases
##################

#filter data to only have Amebiasis disease and male
Male_am<-filter(inf, Disease=="Amebiasis", Sex=="Male")
slope_M <- Male_am%>%  group_by(County) %>%
  summarise(slope = coef(lm(Rate ~ as.numeric(Year)))[2])
#check what the highest slope is
high_slope_M <- slope_M %>%  filter(slope == max(slope))
print(high_slope_M)
#######################
#female cases
#########################
#filter data to only have Amebiasis disease and both male and female cases
Fem_am<-filter(inf, Disease=="Amebiasis", Sex=="Female")

#calc slopes to see if there is any trends
slope_F <- Fem_am%>%  group_by(County) %>%
  summarise(slope = coef(lm(Rate ~ as.numeric(Year)))[2])
#check what the highest slope is
high_slope_F <- slope_F %>%  filter(slope == max(slope))
print(high_slope_F)
# Filter and plot data for county with highest slope (Male Cases)
County_high_slope_M <- high_slope_M$County
Male_high_slope <- filter(Male_am, County == County_high_slope_M)

Male_plot <- ggplot(data = Male_high_slope, aes(x = Year, y = Rate)) +
  geom_point(size = 5, shape = 16) +
  geom_smooth(method = lm, se = FALSE, linetype = "dashed") +
  labs(title = paste("Trend of Amebiasis cases in", County_high_slope_M, "for Males"), x = "Year", y = "Rate")
print(Male_plot)
# Filter and plot data for county with highest slope (Female Cases)
Mendocino_F<-filter(Fem_am, County=="Mendocino")

Female_plot <- ggplot(data = Mendocino_F, aes(x = Year, y = Rate)) +
  geom_point(size = 5, shape = 16) +
  geom_smooth(method = lm, se = FALSE, linetype = "dashed") +
  labs(title = paste("Trend of Amebiasis cases in Mendocino for Females"), x = "Year", y = "Rate")
print(Female_plot)
