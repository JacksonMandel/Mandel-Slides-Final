
#Setup
library(tidyverse)
library(dplyr)
library(ggplot2)

#Download Data
fulldata <- readRDS("V-Dem-CPD-Party-V2.rds")

#Filter Data
filtereddata <- fulldata %>%
  select(v2paenname, country_name, year, v2xpa_popul, v2paseatshare)

#Clean Data & Remove Parties with less than 10% of seats in legislature
cleandata <- filtereddata %>%
  filter(!is.na(v2paseatshare) & !is.na(v2xpa_popul) & v2paseatshare >= 10)

#Regression
PopulismRegression <- lm(v2xpa_popul ~ year, data = cleandata)
print(PopulismRegression)
ggplot(cleandata, aes(x = year, y = v2xpa_popul)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Populism Level vs. Year", 
       x = "Year", 
       y = "Populism") +
  theme_minimal()

#Caluclating Country Averages
average_populism <- cleandata %>%
  group_by(country_name, year) %>%
  summarize(average_populism = mean(v2xpa_popul, na.rm = TRUE))%>%
  group_by(country_name) %>%
  filter(year == max(year)) %>%
  ungroup()

#Ranking countries by level of populist rhetoric in their political parties
ranked_populism <- average_populism %>%
  mutate(populism_rank = rank(-average_populism, ties.method = "first"))

#Boxplot of Country Averages
ggplot(ranked_populism, aes(y = average_populism)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Average Populism Rankings", 
       y = "Average Populism Score") +
  theme_minimal()

#Creating a binary variable for whether countries are in the Americas

americas_countries <- c("Dominican Republic", "Haiti", "Guyana", "Uruguay", 
                        "Jamaica", "El Salvador", "Panama", "Honduras", 
                        "Argentina", "Barbados", "Canada", "Colombia", 
                        "Trinidad and Tobago", "Ecuador", "Guatemala", 
                        "Paraguay", "Mexico", "Peru", "Nicaragua", "Brazil", 
                        "Costa Rica", "Venezuela", "Cuba", 
                        "United States of America")
americasdata <- ranked_populism %>%
  mutate(is_americas = ifelse(country_name %in% americas_countries, 
                              "Americas", "Other"))

#Americas or Not Boxplot
ggplot(americasdata, aes(x = factor(is_americas, labels = 
                                      c("Rest of World", "Americas")), 
                         y = average_populism)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Average Populism: Americas & Rest of World", x = "", 
       y = "Average Populism Score") +
  theme_minimal()

#Perform a t-test
t_test_result <- t.test(average_populism ~ is_americas, 
                        data = americasdata %>% 
                          filter(!is.na(average_populism)))
t_test_summary <- data.frame(
  "Statistic" = t_test_result$statistic,
  "p-value" = t_test_result$p.value,
  "Confidence Interval" = paste(round(t_test_result$conf.int[1], 2), "to", 
                                round(t_test_result$conf.int[2], 2)),
  "Mean Difference" = round(t_test_result$estimate[1] - 
                              t_test_result$estimate[2], 2)
)
print(t_test_summary)