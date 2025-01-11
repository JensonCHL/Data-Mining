library(DT)
library(knitr)
library(tidyverse)
library(dplyr)
library(magrittr)
library(ggplot2)
library(GGally)
library(scales)  # Load the package
library(lubridate)
library(factoextra)

erro

#Loading Dataset
dataset1 <- read.csv("dataset1/dataset1.csv")
dataset2 <- read.csv("dataset2/dataset2.csv")

dataset3 <- read.csv("dataset3/cleaned_dataset3.1.csv")
dataset3 <- dataset3 %>% arrange(AppID)
dataset3 <- dataset3 %>%
  filter(Name != "Counter-Strike")
dataset2and3 <- read.csv("dataset2and3/dataset2and3.csv")
dataset3raw <- read.csv("dataset3/games.csv",row.names = NULL)

#dataset3 drop col
dataset3 <- na.omit(dataset3)
dataset3 <- dataset3[!is.na(dataset3$Windows), ]
sum(is.na(dataset3$Release.date))
sum(is.na(dataset3$Release.year))

#Dataset3 preprocessing
dataset3 <- dataset3 %>%
  mutate(Release.year = year(dmy(Release.date)))

datasetYear <- dataset3 %>%
  mutate(Release.year = year(parse_date_time(Release.date, orders = "my")))

dataset3 <- dataset3 %>%
  mutate(Release.year = ifelse(grepl("^[A-Za-z]{3}-[0-9]{2}$", Release.date), 
                               year(parse_date_time(Release.date, orders = "my")), 
                               Release.year))
round_to_half <- function(x) {
  round(x * 2) / 2
}
dataset3 <- dataset3 %>%
  mutate(rounded_price = round_to_half(Price))
dataset3$Categories <- sapply(strsplit(dataset3$Categories, ","), `[`, 1)

dataset3 <- dataset3 %>%
  mutate(English = ifelse(grepl("English", Supported.languages, ignore.case = TRUE), 1, 0))

dataset3 <- dataset3 %>% select(-Score.rank)
unique(dataset3$Developers)

dataset3$Genres <- sapply(dataset3$Genres, function(x) {
  if (grepl(",", x)) {  # Check if there is a comma
    split_genres <- strsplit(x, ",")[[1]]  # Split the string by commas
    return(trimws(split_genres[1]))  # Return the first genre
  } else {
    return(x)  # If no comma, return the original value
  }
})

unique(dataset3$Genres)
sum(is.na(dataset3$Genres))

write.csv(dataset3, "dataset3/cleaned_dataset3.1.csv", row.names = FALSE)


# Apply the function to the Release.date column



failed_entries <- dataset3[is.na(year(dmy(dataset3$Release.date))), "Release.date"]
print(failed_entries)



unique(dataset3$DiscountDLC.count)
sum(is.na(dataset3$Achievements))
na_rows <- dataset3[is.na(dataset3$Achievements),]
head(dataset3$About.the.game)

#Shifting (Done)
dlc <- which(names(dataset3) == "DiscountDLC.count")
dataset3[, (dlc + 1):ncol(dataset3)] <- dataset3[, (dlc + 2):ncol(dataset3)]
dataset3[, ncol(dataset3)] <- NA
Notion

#Cleaning APP ID COlumn 
dataset3$AppID <- as.integer(dataset3$AppID)
sum(is.na(dataset3$AppID))
na_rows <- dataset3[is.na(dataset3$AppID), ]
dataset3<- dataset3[!is.na(dataset3$AppID), ]


#Cleaning 




#Removing Column
dataset3_cleaned <- dataset3 %>% select(-Header.image,-Website, -Support.url,-Support.email,-Metacritic.url, -Screenshots)
write.csv(dataset3_cleaned, "cleaned_dataset3.1.csv", row.names = FALSE)


#Combining data 




#Combine

#Modify Genre COlumn
dataset2and3$genres <- sapply(strsplit(dataset2and3$genres, ";"), `[`, 1)
dataset2and3$categories <- sapply(strsplit(dataset2and3$categories, ";"), `[`, 1)
dataset2and3 <- dataset2and3 %>% select(-Cluster)
unique(dataset2and3$steamspy_tags)
dataset2and3 <- dataset2and3 %>% select(-steamspy_tags)
dataset2and3 <- dataset2and3 %>%
  mutate(
    release_year = as.integer(substr(release_date, 1, 4)),  # Extract year
    release_month = as.integer(substr(release_date, 6, 7))  # Extract month
  )
#Average Price Plot
# Assuming dataset_filtered is your data frame with numeric columns
correlation_matrix <- cor(dataset_filtered)

# Display the correlation matrix
print(correlation_matrix)

# Optionally, visualize the correlation matrix using a heatmap
library(ggplot2)
library(reshape2)

# Melt the correlation matrix for ggplot
melted_correlation_matrix <- melt(correlation_matrix)

# Create the heatmap
ggplot(data = melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +  # Add this line to display numbers
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  labs(title = "Correlation Matrix Heatmap", x = "Variables", y = "Variables") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


write.csv(dataset2and3, "dataset2and3/dataset2and3.csv", row.names = FALSE)



  ggplot(dataset2and3, aes(x = reorder(genres, Average.playtime.forever), y = Average.playtime.forever)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Average Playtime by Genre", x = "Genre", y = "Average Playtime (Forever)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        plot.title = element_text(hjust = 0.5))


top_positive_ratings <- dataset2and3 %>%
  arrange(desc(positive_ratings)) %>%
  head(15)

ggplot(top_positive_ratings, aes(x = reorder(name, positive_ratings), y = positive_ratings)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Top 15 Games with the Most Positive Ratings(log scalled)",
       x = "Game Titles",
       y = "Positive Ratings") +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  theme_minimal()+
  coord_flip()

#One hot encoding
sum(is.na(dataset3$Genres))
genre_counts <- dataset3 %>%
  separate_rows(Genres, sep = ",") %>%  # Split genres by semicolon
  group_by(Genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

dataset3Genre <- dataset3 %>% select(Name)


for (genre in genre_counts$Genres) {
  dataset3Genre[[genre]] <- ifelse(grepl(genre, dataset3$Genres), 1, 0)
}

  #Model
cluster_dataset <- dataset3 %>% select(Average.playtime.forever
                                       ,Average.playtime.two.weeks,
                                       Positive,
                                       Negative,
                                       Metacritic.score,
                                       User.score,
                                       rounded_price,
                                       Required.age,
                                       Recommendations,
                                       Release.year,
                                       Windows,
                                       Mac,
                                       Linux
                                       )
genre_columns <- dataset3Genre %>%
  select(2:30) 
genre_columns <- genre_columns %>% scale(genre_columns)
cluster_dataset <- cluster_dataset



sum(is.na(TryCluster))
any(is.infinite(TryCluster))  # Check for Inf values
rm(enre_counts)

has_negative_values <- any(cluster_dataset$rounded_price < 0)

dataset_filtered <- cluster_dataset %>%
  filter(Average.playtime.forever > 0, 
         ) %>%
  select(Average.playtime.forever,Average.playtime.two.weeks,Recommendations,Metacritic.score,Required.age,Release.year)

dataset_filtered <- dataset_filtered %>% mutate(Average.playtime.forever = log(Average.playtime.forever))
dataset_filtered <- dataset_filtered %>% mutate(Recommendations = log(Recommendations))
dataset_filtered <- dataset_filtered %>% select(-log_Average.playtime.forever,-log_Recommendations)



TryCluster <- scale(dataset_filtered)
TryCluster <- dataset_filtered
TryCluster <- cluster_dataset %>%  select(Average.playtime.forever,Recommendations,(10:38))

TryCluster <- as.numeric(unlist(TryCluster[,3:31]))

TryCluster <- TryCluster %>% scale(TryCluster[,3:31])
TryCluster <- TryCluster %>% select(-Cluster)


sum(is.na(TryCluster))

unique(dataset3$Genres)

summary(cluster_dataset)

num_zero_scores <- sum(dataset3$Metacritic.score == 0, na.rm = TRUE)


dataset_transformed <- log1p(dataset2and3[, c("Average.playtime.forever","price","Recommendations")])
dataset_scaled <- scale(dataset_transformed)
pca_result <- prcomp(dataset_scaled, center = TRUE, scale. = TRUE)



pca_result <- prcomp(TryCluster, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca_result$x[, 1:2])  # Get the first two principal components
pca_data$Cluster <- TryCluster$Cluster
pca_data$Recommendations <- TryCluster$Recommendations

ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point() +
  
  labs(title = "PCA of Clustering with Action Genre Highlighted")

#Drop Null  Average column
#Split Genre 
split_genres <- strsplit(dataset2and3$genres, ";")
all_genres <- as.data.frame(unique(unlist(split_genres)))
all_genres <- trimws(all_genres)  # Trim whitespace

genre_counts <- table(all_genres)
genre_counts_df <- as.data.frame(genre_counts)
colnames(genre_counts_df) <- c("Genre", "Count")






#visualization 




ggplot(dataset_filtered,aes(x = log(Recommendations),y=Required.age))+
  geom_point()+
  labs(title = "Average Playtime and MetaCritic Score",
       x = "Recommendations",
       y = "MetaCritic Score")

ggplot(dataset_filtered, aes(x = log(Average.playtime.two.weeks), y = Release.year)) +
  geom_point() +
  labs(
    title = "Filtered Average Playtime vs. Positive Ratings (Log Scale)",
    x = "Average Playtime Forever",
    y = "Release Year"
  )
ggplot(dataset_filtered, aes(x = log(Average.playtime.forever), y = rounded_price)) +
  geom_point() +
  labs(
    title = "Filtered Average Playtime vs. Rounded Price (Log Scale)",
    x = "Average Playtime Forever",
    y = "Rounded Price"
  )
ggplot(TryCluster, aes(x = log(Average.playtime.forever), y = log(Recommendations))) +
  geom_point() +
  labs(
    title = "Filtered Average Playtime vs. Rounded Price (Log Scale)",
    x = "Average Playtime Forever",
    y = "Recommendations"
  )
unique(dataset_filtered$Recommendations)
sum(is.na(cluster_dataset))




ggpairs(dataset2and3[, c("Average.playtime.forever", "positive_ratings", "Recommendations")])


unique(dataset2and3$genres)
summary(dataset2and3$required_age)

nrow(filter(dataset2and3,required_age  == 0))

#Required Age column


#Displaying Dataset



#kable(head(datasetMain_Cleaned), format = "markdown")
#datatable(head(datasetMain_Cleaned))

