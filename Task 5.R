library(dplyr)
library(tidyr)
library(ggplot2)
options(scipen=999)

#load data
inf<-read.csv("Infectious Disease 2001-2014.csv")
cal_dataC<-filter(inf, County=="California", Disease=="Chlamydia", Sex=="Total")
cal_dataS<-filter(inf, County=="California", Disease=="Salmonellosis", Sex=="Total")
correlation <- cor(cal_dataC$Rate, cal_dataS$Rate)
print(correlation)

#plot
ggplot(data = cal_dataC, aes(x = Rate, y = cal_dataS$Rate)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE,linetype= "dashed")+
  labs(title = "Relationship between Rates of Chlamydia and Salmonellosis in California",
       x = "Chlamydia Rate",
       y = "Salmonellosis Rate")

cal_data_combined <- cal_dataC %>%
  select(Year, Chlamydia_Rate = Rate) %>%
  inner_join(cal_dataS %>% select(Year, Salmonellosis_Rate = Rate), by = "Year")

# Identify outliers using a simple threshold method or interquartile range (IQR)
Q1 <- quantile(cal_data_combined$Chlamydia_Rate, 0.25)
Q3 <- quantile(cal_data_combined$Chlamydia_Rate, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR


# Filter out outliers
cal_data_no_outliers <- cal_data_combined %>%
  filter(Chlamydia_Rate >= lower_bound & Chlamydia_Rate <= upper_bound)

# Recalculate correlation wihtout outliers
correlation_no_outliers <- cor(cal_data_no_outliers$Chlamydia_Rate, cal_data_no_outliers$Salmonellosis_Rate)
print(correlation_no_outliers)
