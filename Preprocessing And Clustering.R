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
library(clusterCrit)
library(pracma)
library(gridExtra)



dataset_filtered <- read.csv("ReadyModelDataset/ready_dataset3.1.csv")
dataset <- read.csv("dataset1and3/dataset1and3.csv",fileEncoding = "UTF-8-BOM")

sum(is.na(dataset1and3))
cluster_df <- dataset1and3 %>% select(revenue,Estimated.owners,copiesSold,avgPlaytime,reviewScore,publisherClass,Peak.CCU,Windows,Mac,Linux,Metacritic.score, Genres,Categories,price,English,Recommendations,Achievements,Positive,Negative)

write.csv(TryCluster,"ReadyModelDataset/ClusterReady.csv")
#Pairplot
ggpairs(dataset_filtered)
summary(dataset1and3$Required.age)

TryCluster <- read.csv("ReadyModelDataset/ClusterReady.csv", fileEncoding = "UTF-8-BOM",check.names = FALSE)

#Coor
numeric_columns <- dataset_filtered[, sapply(dataset_filtered, is.numeric)]
correlation_matrix <- cor(numeric_columns, use = "complete.obs")
melted_correlation_matrix <- melt(correlation_matrix)
ggplot(data = melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +  # Create the heatmap
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +  # Add correlation values
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +  # Define color scale
  theme_minimal() +  # Use a minimal theme
  labs(title = "Correlation Matrix Heatmap", x = "Variables", y = "Variables") +  # Add labels
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))  # Rotate x-axis text



plot(cluster_df)
plot(dbscan_df)

set.seed(123)
rm(model)
#Changing to categorical Value
publisherClass_encoded_df <- as.data.frame(publisherClass_encoded)

cluster_df$Genres <- as.factor(cluster_df$Genres)
cluster_df$Genres <- as.numeric(cluster_df$Genres)
cluster_df$Categories <- as.factor(cluster_df$Categories)


cluster_df$Categories <- as.character(cluster_df$Categories)

genre_categorical <- table(cluster_df$Genres)



genre_categorical <- data.frame(
  original = levels(cluster_df$Genres),
  numeric = 1:length(levels(cluster_df$Genres))
)

#One Hot encoding 
publisherClass_encoded <- model.matrix(~ publisherClass - 1, data = cluster_df)

categories_encoded <- model.matrix(~ Categories - 1, data = cluster_df)

#
publisherClass_encoded_df <- as.data.frame(publisherClass_encoded)
write.csv(publisherClass_encoded_df,"OneHotData/publisherClass_encoded.csv")

categories_encoded_df <- as.data.frame(categories_encoded)

write.csv(categories_encoded_df,"OneHotData/categories_encoded.csv")

names(genres_encoded_df) <- gsub(" & ",".",names(genres_encoded_df))
names(genres_encoded_df) <- gsub(" ",".",names(genres_encoded_df))
names(categories_encoded_df) <- gsub("-",".",names(categories_encoded_df))
names(categories_encoded_df) <- gsub(" ",".",names(categories_encoded_df))


#Dataset
selected_columns <- cluster_df[, c("revenue","copiesSold", "avgPlaytime", "price", "Peak.CCU","English","Recommendations","reviewScore","gen")]

TryCluster <- cbind(selected_columns, categories_encoded_df, publisherClass_encoded_df)
plot(TryCluster)

TryCluster <- TryCluster_cpy
TryCluster[, 2:5] <- lapply(TryCluster[, 2:5], as.integer)

TryCluster$Peak.CCU <- as.integer(TryCluster$Peak.CCU)
TryCluster$Windows <- as.numeric(TryCluster$Windows)
TryCluster$Mac <- as.numeric(TryCluster$Mac)
TryCluster$Linux <- as.numeric(TryCluster$Linux)
TryCluster$reviewScore <- as.integer(TryCluster$reviewScore)
TryCluster$copiesSold <- as.integer(TryCluster$copiesSold)
TryCluster$avgPlaytime <- as.integer(TryCluster$avgPlaytime)
TryCluster$price <- as.integer(TryCluster$price)
TryCluster$English <- as.numeric(TryCluster$English)
TryCluster$Recommendations <- as.integer(TryCluster$Recommendations)
TryCluster$Genres <- as.integer(TryCluster$Genres)

TryCluster_cpy <- TryCluster

#Min Max Scalling
min_max_scale <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

int_columns <- sapply(TryCluster, is.integer)
TryCluster[, int_columns] <- lapply(TryCluster[, int_columns], function(x) min_max_scale(x))

#Scalee
binary_columns <- sapply(cluster_df, function(x) all(x %in% c(0, 1)))
numerical_columns <- sapply(cluster_df, is.numeric) & !binary_columns
cluster_df[numerical_columns] <- scale(cluster_df[numerical_columns])

#Log Transformation
int_columns <- sapply(TryCluster, is.integer)
TryCluster[, int_columns] <- lapply(TryCluster[, int_columns], function(x) log1p(x)) # Adding 1 to avoid log(0)
head(TryCluster)


#PCA
pca_result <- prcomp(TryCluster, center = TRUE, scale. = TRUE)
TryCluster_df_pca <- as.data.frame(pca_result$x)
write.csv(TryCluster_df_pca,"ReadyModelDataset/cluster_df_pca.csv")
  # A minimal theme for better aesthetics
# Plot the distances to the k-th nearest neighbor

kNNdistances <- kNNdist(TryCluster_df_pca, k = 3)  # Use k = minPts - 1, generally
kNNdist_df <- data.frame(kNNdistances = sort(kNNdistances))

ggplot(kNNdist_df, aes(x = 1:nrow(kNNdist_df), y = kNNdistances)) +
  geom_line() +
  geom_point() +
  ggtitle("k-NN Distance Plot for DBSCAN (k = 4)") +
  xlab("Points sorted by distance to 4th nearest neighbor") +
  ylab("Distance to 4th nearest neighbor") +
  geom_hline(yintercept = 1.43, color = "red", linetype = "dashed")


selected_columns <- cluster_df %>% select(copiesSold,avgPlaytime,price,Peak.CCU,Genres,reviewScore,English,Achievements,Recommendations)

TryCluster <- cbind(selected_columns, categories_encoded_df, publisherClass_encoded_df)

#DBscan
d<-dbscan::dbscan(TryCluster_df_pca,6.87,2)
fviz_cluster(list(data = TryCluster_df_pca, cluster = d$cluster), geom = "point", 
             ggtheme = theme_minimal()) +
  labs(title = "DBSCAN Clustering Visualization",
       subtitle = "Using copiesSold,avgPlaytime,price,Peak.CCU,Genres,reviewScore,English,Categories,Publisher Class")
filtered_data_df$Cluster <- as.factor(filtered_clusters_eval)

fviz_cluster(list(data = filtered_data_df[, 1:15], cluster = filtered_data_df$Cluster), 
             geom = "point", 
             ggtheme = theme_minimal()) +
  labs(title = "DBSCAN Clustering Visualization",
       subtitle = "Using copiesSold, avgPlaytime, price, Peak.CCU, Genres, reviewScore, English, Categories, Publisher Class")

dataset$Cluster_DBSCAN <- d$cluster
table(d$cluster)

#Evaluation
opt <- dbscan::optics(TryCluster_df_pca, eps = 6.87, minPts = 2)
plot(opt, main = "Reachability Plot DBSCAN", 
     xlab = "Data Points (Ordered)", 
     ylab = "Reachability Distance")
#Remove the noise
filtered_data_eval <- TryCluster_df_pca[d$cluster != 0, ]
filtered_clusters_eval <- d$cluster[d$cluster != 0]

# Combine data into a data frame
filtered_data_df <- as.data.frame(filtered_data_eval)
filtered_data_df$Cluster <- as.factor(filtered_clusters_eval)


# Calculate the reachability distances for each point (use core distances and distances between points)
reachability_distances <- dbscan_result$reachability



table(filtered_data_df$Cluster)



TryCluster_matrix <- as.matrix(TryCluster_df_pca)





print(dunn)
print(db_index)
 
OPTICS
OpticsCluster_pca <- TryCluster_df_pca
o <- optics(OpticsCluster_pca,100,2)
Opticsclusters <- extractDBSCAN(o, eps_cl = 6.8)$cluster


fviz_cluster(list(data = OpticsCluster_pca, cluster = Opticsclusters), geom = "point", 
             ggtheme = theme_minimal()) +
  labs(title = "Optics Clustering Visualization",
       subtitle = "Using copiesSold, avgPlaytime, price, Peak.CCU, Genres, reviewScore, English, Categories, Publisher Class")

table(Opticsclusters)

library(dbscan)
# Assuming you have the data and have applied OPTICS clustering

# Plotting the reachability plot
plot(o, 
     main = "Reachability Plot for OPTICS", 
     xlab = "Data Points (Ordered)", 
     ylab = "Reachability Distance")
dataset$Cluster_OPTICS <- Opticsclusters

# Filter out cluster 0 (noise) from the clustering result
Opticsclusters_no_noise <- Opticsclusters[Opticsclusters != 0]
OpticsCluster_pca_no_noise <- OpticsCluster_pca[Opticsclusters != 0, 1:15]

# Evaluate Dunn Index for Optics Clustering without noise
dunn_index_optics <- intCriteria(as.matrix(OpticsCluster_pca_no_noise), as.integer(Opticsclusters_no_noise), "Dunn")
print(paste("Dunn Index for Optics Clustering (without noise):", dunn_index_optics))

# Davies-Bouldin Index for Optics Clustering without noise
db_index_optics <- intCriteria(as.matrix(OpticsCluster_pca_no_noise), as.integer(Opticsclusters_no_noise), "Davies_Bouldin")
print(paste("Davies-Bouldin Index for Optics Clustering (without noise):", db_index_optics))

# Silhouette Coefficient for Optics Clustering without noise
library(cluster)
sil_optics <- silhouette(Opticsclusters_no_noise, dist(OpticsCluster_pca_no_noise))  # Use only PCA columns for silhouette
silhouette_avg_optics <- mean(sil_optics[, 3])  # Average silhouette width
cat("Silhouette Coefficient for Optics Clustering (without noise):", silhouette_avg_optics)

# Plot the Optics Clustering results without noise
library(factoextra)
fviz_cluster(list(data = OpticsCluster_pca_no_noise, cluster = Opticsclusters_no_noise), geom = "point", 
             ggtheme = theme_minimal()) +
  labs(title = "Optics Clustering Visualization (Without Noise)",
       subtitle = "Using copiesSold, avgPlaytime, price, Peak.CCU, Genres, reviewScore, English, Categories, Publisher Class")

# Table of Optics clusters to see the distribution without noise
table(Opticsclusters_no_noise)





#Spectral 
spectralCluster_pca <- as.matrix(TryCluster_df_pca)  



s <- specc(spectralCluster_pca, centers = 3, kernel = "polydot", kpar = list(degree = 3, scale = 1, offset = 1))
dim(spectralCluster_pca)  # Should show 1276 rows, number of columns

fviz_cluster(list(data = spectralCluster_pca, cluster = cluster_spectral), 
             geom = "point", 
             ggtheme = theme_minimal()) +
  labs(title = "Spectral Clustering Visualization",
       subtitle = "Using Selected Features")
library(stats)
distance_matrix <- dist(spectralCluster_pca)  # You can also use the distance in the transformed space

# Step 3: Create a heatmap of the distance matrix (this is similar to the reachability plot)
library(pheatmap)
pheatmap(as.matrix(distance_matrix), 
         cluster_rows = TRUE, 
         cluster_cols = TRUE, 
         main = "Distance Matrix Heatmap for Spectral Clustering",
         display_numbers = FALSE)



cluster_spectral <- s

table(cluster_spectral)



dataset$Cluster_SPECTRAL <- as.numeric(cluster_spectral)

summary(o)

#Evaluation Metrics
# Convert spectral clustering result to integer labels
cluster_spectral_int <- as.integer(cluster_spectral)

# Dunn Index
# Dunn Index for Spectral Clustering
dunn_index_spectral <- intCriteria(as.matrix(spectralCluster_pca), cluster_spectral_int, "Dunn")
print(paste("Dunn Index for Spectral Clustering:", dunn_index_spectral))

# Davies-Bouldin Index for Spectral Clustering
db_index_spectral <- intCriteria(as.matrix(spectralCluster_pca), cluster_spectral_int, "Davies_Bouldin")
print(paste("Davies-Bouldin Index for Spectral Clustering:", db_index_spectral))

# Silhouette Coefficient for Spectral Clustering
library(cluster)
silhouette_spectral <- silhouette(cluster_spectral_int, dist(spectralCluster_pca))
silhouette_avg <- mean(silhouette_spectral[, 3])  # Average silhouette width
print(paste("Silhouette Coefficient for Spectral Clustering:", silhouette_avg))



#Cara menjelaskan PCA


table(TryCluster$Mac)

num_clusters <- max(d$cluster)
colors <- c("red", "green", "blue", "purple")
plot_colors <- ifelse(d$cluster == 0, "red", colors[d$cluster])

summary(dataset1and3$User.score)



#Interpret
# Ensure that cluster_df_raw and d$cluster have the same number of rows
# Add cluster assignments to the original dataset
cluster_df_raw$Cluster <- d$cluster
library(factoextra)

# Visualize the clusters using fviz_cluster
fviz_cluster(list(data = cluster_df_pca, cluster = d$cluster), geom = "point") +
  labs(title = "DBSCAN Clustering Visualization",
       subtitle = "Using Average.playtime.forever, Genres, Categories, rounded_price, English, Recommendations")
# Plot the original data with clusters
plot(cluster_df_raw[, c("Average.playtime.forever", "rounded_price")], 
     col = plot_colors,  # Use the custom color mapping
     pch = 19,
     main = "Cluster Visualization on Original Data",
     xlab = "Average Playtime Forever", 
     ylab = "Categories")
legend("topright", legend = c("Noise", "Cluster 1", "Cluster 2", "Cluster 3"), 
       col = c("red", "green", "blue", "purple"), pch = 19)

plot(cluster_df_raw, 
     col = plot_colors,  # Use the custom color mapping
     pch = 19,
     main = "Cluster Visualization on Original Data")
     
legend("topright", legend = c("Noise", "Cluster 1", "Cluster 2", "Cluster 3"), 
       col = c("red", "green", "blue", "purple"), pch = 19)

unique(cluster_df_raw$Genres)
unique(cluster_df$Genres)


