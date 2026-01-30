# ================================
# Install & Load Required Packages
# ================================

install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("GGally")
install.packages("reshape2")

library(tidyverse)
library(lubridate)
library(ggplot2)
library(GGally)
library(reshape2)

# ================================
# Load Dataset
# ================================

corona <- read.csv("/Users/karandharamshi/Desktop/covid-19-master/data/countries-aggregated.csv")

head(corona)
dim(corona)

# ================================
# Data Overview
# ================================

summary(corona)

corona$total_count <- corona$Confirmed + corona$Recovered + corona$Deaths

# ================================
# Date Handling
# ================================

corona$Date <- as.Date(corona$Date)
min(corona$Date)
max(corona$Date)

# ================================
# Unique Countries
# ================================

length(unique(corona$Country))
unique(corona$Country)

# ================================
# Top Countries by Confirmed Cases
# ================================

top_countries <- corona %>%
  group_by(Country) %>%
  summarise(total_confirmed = max(Confirmed)) %>%
  arrange(desc(total_confirmed)) %>%
  head(10)

ggplot(top_countries, aes(x = reorder(Country, total_confirmed),
                          y = total_confirmed)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top Countries by Total Confirmed Cases",
       x = "Country",
       y = "Total Confirmed")

# ================================
# Pairwise Relationship Plot
# ================================

ggpairs(corona[, c("Confirmed", "Recovered", "Deaths")],
        title = "Covid data")

# ================================
# Linear Regression Model 1
# Deaths vs Confirmed
# ================================

fit1 <- lm(Deaths ~ Confirmed, data = corona)
summary(fit1)

# Residual Histogram
hist(fit1$residuals,
     breaks = 50,
     col = "gray",
     main = "Histogram for Model Residuals",
     xlab = "Residuals")

# Linear Fit Plot
ggplot(corona, aes(x = Confirmed, y = Deaths)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Model Fitted to Data")

# Prediction
predict(fit1, data.frame(Confirmed = 33962))

# ================================
# Linear Regression Model 2
# Deaths vs Confirmed + Recovered
# ================================

fit2 <- lm(Deaths ~ Confirmed + Recovered, data = corona)
summary(fit2)

predict(fit2,
        data.frame(Confirmed = 33962,
                   Recovered = 44351))

# ================================
# Linear Regression Model 3
# Deaths vs Confirmed * Recovered
# ================================

fit3 <- lm(Deaths ~ Confirmed * Recovered, data = corona)
summary(fit3)

predict(fit3,
        data.frame(Confirmed = 33962,
                   Recovered = 44351))
