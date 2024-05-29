library(dplyr)
library(tidyr)
library(dunn.test)
library(ggplot2)
options(scipen=999)

#load data
inf<-read.csv("Infectious Disease 2001-2014.csv")

C_data<-filter(inf, County %in% c("California", "Lake", "San Diego", "San Francisco"), Disease == "Cryptosporidiosis", Sex=="Total")
kruskal_test_result <- kruskal.test(Rate ~ County, data = C_data)
print(kruskal_test_result)

dunn_test_result <- dunn.test(C_data$Rate, g = C_data$County, method = "bonferroni")
print(dunn_test_result)

C_data$Year <- factor(C_data$Year)


ggplot(data = C_data, aes(x = Year, y = Rate, color = County, group=County)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Rate of Cryptosporidiosis", color = "County") +
  theme_minimal() +
  theme(legend.position = "right") +
  ggtitle("Changes in the Rate of Cryptosporidiosis in California, Lake, San Diego, and San Francisco")
