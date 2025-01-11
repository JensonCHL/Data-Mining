library(cluster)
library(fpc)
library(dplyr)
library(caret)
library(randomForest)  # Or any other model you want to use
library(ggplot2)
library(factoextra)
library(dbscan)
library(fpc)
library(corrplot)
library(reshape2)
library(GGally)  # For ggpairs function
library(e1071)
library(kernlab)

ggplot(dataset, aes(x = as.factor(Cluster_SPECTRAL), y = avgPlaytime)) +
  geom_boxplot() +
  labs(title = "Average Playtime by Spectral Clusters", x = "Cluster", y = "Average Playtime")

# Barplot for genre distribution
ggplot(dataset, aes(x = as.factor(Cluster_SPECTRAL), fill = Genres)) +
  geom_bar(position = "dodge") +
  labs(title = "Genre Distribution by Spectral Clusters", x = "Cluster", y = "Count")


ggplot(dataset, aes(x = log1p(Peak.CCU), y = log1p(copiesSold), color = as.factor(Cluster_SPECTRAL))) +
  geom_point(alpha = 0.6)

ggplot(dataset, aes(x = avgPlaytime, y = price, color = as.factor(Cluster_SPECTRAL))) +
  geom_point(alpha = 0.7) +
  labs(title = "Playtime vs. Price by Cluster", x = "Average Playtime", y = "Price") +
  theme_minimal()



# Bar plot for Categories distribution by cluster
category_percentages <- dataset %>%
  group_by(Cluster_SPECTRAL, Categories) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

ggplot(category_percentages, aes(x = Categories, y = Percentage, fill = as.factor(Cluster_SPECTRAL))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Category Distribution by Spectral Clustering",
    x = "Categories",
    y = "Percentage",
    fill = "Cluster"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Calculate genre distribution within clusters
genre_cluster_distribution <- dataset %>%
  group_by(Cluster_SPECTRAL, Genres) %>%
  summarize(Count = n(), .groups = "drop") %>%
  group_by(Cluster_SPECTRAL) %>%
  mutate(Percentage = Count / sum(Count) * 100)

print(genre_cluster_distribution, n = Inf)

ggplot(genre_cluster_distribution, aes(x = as.factor(Cluster_SPECTRAL), y = Percentage, fill = Genres)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  labs(
    title = "Genre Distribution Across Clusters",
    x = "Clusters",
    y = "Percentage",
    fill = "Genres"
  )

#Publisher Class Distribution
publisher_cluster_distribution <- dataset %>%
  group_by(Cluster_SPECTRAL, publisherClass) %>%
  summarize(Count = n(), .groups = "drop") %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

print(publisher_cluster_distribution, n = Inf)
ggplot(publisher_cluster_distribution, aes(x = as.factor(Cluster_SPECTRAL), 
                                           y = Percentage, 
                                           fill = publisherClass)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Publisher Class Distribution Across Clusters",
       x = "Clusters", y = "Percentage") +
  theme_minimal()



library(dplyr)

cluster_summary <- dataset %>%
  group_by(Cluster_SPECTRAL) %>%
  summarize(
    Avg_Revenue = mean(revenue, na.rm = TRUE),
    Median_Revenue = median(revenue, na.rm = TRUE),
    Total_Revenue = sum(revenue, na.rm = TRUE),
    Avg_Peak_CCU = mean(Peak.CCU, na.rm = TRUE),
    Median_Peak_CCU = median(Peak.CCU, na.rm = TRUE),
    Avg_Recommendations = mean(Recommendations, na.rm = TRUE),
    Median_Recommendations = median(Recommendations, na.rm = TRUE),
    Avg_Playtime = mean(avgPlaytime, na.rm = TRUE),
    Median_Playtime = median(avgPlaytime, na.rm = TRUE)
  )

print(cluster_summary)

# library(ggplot2)

ggplot(dataset, aes(x = Peak.CCU, y = revenue, color = as.factor(Cluster_SPECTRAL))) +
  geom_point(alpha = 0.7) +
  scale_y_log10() +  # Use log scale if revenue values vary widely
  labs(
    title = "Revenue vs Peak.CCU by Cluster",
    x = "Peak Concurrent Players",
    y = "Revenue",
    color = "Cluster"
  ) +
  theme_minimal()

ggplot(dataset, aes(x = Recommendations, y = revenue, color = as.factor(Cluster_SPECTRAL))) +
  geom_point(alpha = 0.7) +
  scale_y_log10() +
  labs(
    title = "Recommendations vs Revenue by Cluster",
    x = "Recommendations",
    y = "Revenue",
    color = "Cluster"
  ) +
  theme_minimal()





