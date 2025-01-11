library(dplyr)
library(caret)
library(randomForest)  # Or any other model you want to use
library(ggplot2)
library(factoextra)
library(dbscan)
library(fpc)
library(corrplot)
library(reshape2)
library(GGally)

cor(dataset1and3$Peak.CCU, dataset1and3$revenue, use = "complete.obs")

dataset1and3 <- dataset1and3 %>%
  select(-last_col())

twocol <- dataset1and3 %>% select(revenue,Peak.CCU,avgPlaytime,reviewScore,price,Positive,Negative,Achievements)

dataset1and3RAW <- read.csv("dataset1and3/dataset1and3RAW.csv")
write.csv(dataset1and3,"dataset1and3/dataset1and3.csv")

dataset1and3 <- dataset1and3 %>%
  mutate(Peak.CCU = handeledccu$Peak.CCU)

sum(dataset1and3$reviewScore == 0)

predictccu <- dataset1and3 %>% select(revenue$Peak.CCU)
predictccu <- dataset1and3 %>%
  select(revenue, Peak.CCU) %>%
  mutate(
    revenue = log1p(revenue),  # Log transformation on revenue
    Peak.CCU = ifelse(Peak.CCU > 0, log1p(Peak.CCU), 0)  # Log transformation on Peak.CCU only where > 0
  )

predictccumodel <- dataset1and3 %>% filter(Peak.CCU > 0) %>% select(revenue,Peak.CCU)

predictccumodel <- log1p(predictccumodel)
summary(predictccumodel$Peak.CCU)

sum(predictccumodel$Peak.CCU < 0)
str(predictccu)
#Coor Matrix
correlation_matrix <- cor(twocol, use = "complete.obs")
correlation_melted <- melt(correlation_matrix)

ggplot(data = correlation_melted, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +  # Add correlation values
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed() +
  labs(title = "Correlation Heatmap", x = "Variables", y = "Variables")


print(correlation_percentage)
sum(predictccu$Peak.CCU < 0)
#Model 
model <- lm(Peak.CCU ~ revenue, data = predictccumodel)
summary(model)

predictccu$Peak.CCU[predictccu$Peak.CCU == 0] <- predict(model, newdata = predictccu[predictccu$Peak.CCU == 0, ])
#Modeell
ggplot(dataset1and3, aes(x = log(revenue), y = log(Peak.CCU))) +
  geom_point(alpha = 0.5) +  # Add points with some transparency
  #geom_smooth(method = "lm",color = "blue", se = FALSE) +  # Add regression line
  labs(title = "Linear Regression of Peak.CCU vs. Revenue",
       x = "Revenue",
       y = "Peak CCU") +
  theme_minimal()  # Use a minimal theme for better appearance

handeledccu <- exp(predictccu)
