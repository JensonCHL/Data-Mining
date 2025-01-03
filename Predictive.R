library(dplyr)
library(caret)
library(randomForest)  # Or any other model you want to use
library(ggplot2)
library(factoextra)


dataset3 <- read.csv("dataset3/cleaned_dataset3.1.csv")

dataset3<- dataset3 %>% filter(Genres != "")

dataset3 <- na.omit(dataset3)
sum(is.na(dataset3$Peak.CCU))

summary(dataset3$Peak.CCU)
dataset_filtered <- dataset3 %>% 
  filter(Average.playtime.forever > 0,Recommendations > 0 ) %>% 
  select(Average.playtime.forever, 
         Positive, 
         Negative, 
         Release.year, 
         rounded_price,
         English,
         Windows,
         Mac,
         Linux,
         Achievements,
         Recommendations,
         Categories,
         Genres)
dataset_filtered <- na.omit(dataset_filtered)
sum(is.na(dataset_filtered))
boxplot(dataset_filtered$Average.playtime.forever, main="Boxplot of Average Playtime Forever",
        ylab="Average Playtime (Forever1)", col="lightblue")

rm(list=ls())
# Calculate Q1 (25th percentile) and Q3 (75th percentile)

numerical_columns <- c("Average.playtime.forever", "Positive", "Negative", "Recommendations")
remove_outliers <- function(column_data) {
  Q1 <- quantile(column_data, 0.25, na.rm = TRUE)
  Q3 <- quantile(column_data, 0.75, na.rm = TRUE)
  IQR_value <- IQR(column_data, na.rm = TRUE)
  
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  return(column_data >= lower_bound & column_data <= upper_bound)
}
for (col in numerical_columns) {
  dataset_filtered <- dataset_filtered %>%
    filter(remove_outliers(.[[col]]))
}


Q1 <- quantile(dataset_filtered$Average.playtime.forever, 0.25)
Q3 <- quantile(dataset_filtered$Average.playtime.forever, 0.75)

# Calculate the Interquartile Range (IQR)
IQR_value <- IQR(dataset_filtered$Average.playtime.forever)

# Define the outlier limits
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Filter out the outliers
dataset_filtered <- dataset_filtered %>%
  filter(Average.playtime.forever >= lower_bound & Average.playtime.forever <= upper_bound)

# Visualize the boxplot without outliers
boxplot(dataset_filtered$Average.playtime.forever, main="Boxplot After Removing Outliers",
        ylab="Average Playtime (Forever)", col="lightgreen")


# Encode the column in Genre here with the number of unique genre
unique_genres <- unique(dataset_filtered$Genres)
unique_categories <- unique(dataset_filtered$Categories)
genre_mapping <- setNames(seq_along(unique_genres), unique_genres)
categories_mapping <- setNames(seq_along(unique_categories), unique_categories)
# Replace genres with corresponding integer values
dataset_filtered$Genres <- genre_mapping[dataset_filtered$Genres]
dataset_filtered$Categories <- categories_mapping[dataset_filtered$Categories]


#write.csv(dataset3, "ReadyModelDataset/ready_dataset3.1.csv", row.names = FALSE)
# Scale numerical features
#numerical_columns <- sapply(dataset_filtered, is.numeric)
#dataset_filtered[numerical_columns] <- scale(dataset_filtered[numerical_columns])

#MinMax Scalling 
min_max_scale <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
dataset_filtered[numerical_columns] <- lapply(dataset_filtered[numerical_columns], min_max_scale)


set.seed(123)  # For reproducibility
train_index <- createDataPartition(dataset_filtered$Average.playtime.forever, p = 0.8, list = FALSE)
train_data <- dataset_filtered[train_index, ]
test_data <- dataset_filtered[-train_index, ]

sum(is.na(test_data))
# Fit a Random Forest model
rf_model <- randomForest(Average.playtime.forever ~ ., data = train_data)
# Make predictions
predictions <- predict(rf_model, test_data)

# Print model summary
print(rf_model)
# Evaluate model performance
mse <- mean((predictions - test_data$Average.playtime.forever)^2)
cat("Mean Squared Error:", mse, "\n")

summary(dataset3$Average.playtime.forever)
hist(dataset3$Average.playtime.forever)
#Plot 
importance(rf_model)  # Get the importance of each feature
varImpPlot(rf_model)

ggplot(data = test_data, aes(x = predictions, y = test_data$Average.playtime.forever)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Random Forest: Predicted vs Actual",
       x = "Predicted Playtime Average Playtime",
       y = "Actual Playtime") +
  theme_minimal()

residuals <- test_data$Average.playtime.forever - predictions

ggplot(data = test_data, aes(x = residuals)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Residual Distribution", x = "Residuals", y = "Frequency") +
  theme_minimal()

#Kmeans
fviz_nbclust(dataset_filtered, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method")

km.out <- kmeans(dataset_filtered,centers = 2, nstart = 25)
print(km.out)

TryCluster_df <- as.data.frame(dataset_filtered)
TryCluster_df$Cluster <- as.factor(km.out$cluster)


fviz_cluster(list(data = TryCluster_df[, -ncol(TryCluster_df)], cluster = km.out$cluster)) +
  labs(title = "KMeans Clustering Results")

table(km.out$cluster, TryCluster_df$Cluster)


