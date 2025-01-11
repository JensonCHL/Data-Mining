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

dataset1and3 <- read.csv("dataset1and3/dataset1and3.csv")

columns_to_remove <- c("X", "Release.date", "DiscountDLC.count", "About.the.game",
                       "Supported.languages", "Full.audio.languages", "Reviews",
                       "Notes", "Developers", "Publishers","AppID","Tags")

combined_dataset <- dataset1and3 %>%
  select(-one_of(columns_to_remove))
combined_dataset <- read.csv("dataset1and3/dataset1and3.csv")

#zero CCU
ZerCCU <- combined1and3 %>% filter(Peak.CCU == 0)

write.csv(combined_dataset,"dataset1and3/dataset1and3RAW.csv", row.names = FALSE)

unique(combined1and3$Genres)

summary(combined_dataset$revenue)

rm(list = ls())

combined_dataset1 <- combined_dataset <- select(name)

colnames(combined_dataset)
dataset1 <- read.csv("dataset1/dataset1.csv")

write.csv(dataset1,"dataset1/dataset1.csv")

#Rename
dataset3 <- dataset3 %>%
  rename(name = Name)

dataset1 <- dataset1 %>%
  mutate(name = tolower(name))

dataset3 <- dataset3 %>%
  mutate(name = tolower(name))


unique_chars <- unique(all_chars)

cat("Unusual characters found in the 'name' column: ", unusual_chars, "\n")

#Combining the dataset using Inner Join
combined_dataset <- inner_join(dataset1, dataset3, by = "name") 

sum(is.na(combined_dataset))
rm(combined_dataset)
summary(combined_dataset$avgPlaytime)

sum(is.na(combined_dataset$Peak.CCU))
sum(is.na(dataset3$Peak.CCU))

wwe_2k24_info <- dataset3 %>%
  filter(tolower(name) == "wwe 2k24")

combined1and3 <- combined1and3 %>%
  group_by(name) %>%
  filter(n() > 1) %>%
  summarize(count = n())

duplicates_dataset3 <- dataset3 %>%
  group_by(name) %>%
  filter(n() > 1) %>%
  summarize(count = n())

dataset3 <- dataset3 %>%
  distinct(name, .keep_all = TRUE)

summary(combined_dataset$About.the.game == "")

sum(is.na(dataset3))


sum(is.na(combined_dataset))
summary(combined_dataset$DiscountDLC.count)
unique(combined_dataset$Publishers)
#Boxplot
boxplot(combined_dataset$avgPlaytime, main = "Boxplot of AvgPlaytime", ylab = "Playtime (Hours)")

#Zscore
dataset1$z_score_playtime <- scale(dataset1$avgPlaytime)

#Visualize
hist(combined_dataset$price, breaks = 50, main = "Histogram of price", xlab = "price (Hours)")
plot(density(combined_dataset$avgPlaytime), main = "Density Plot of AvgPlaytime")

boxplot(dataset1$copiesSold,
        main = "Boxplot of copies sold",
        ylab = "CopiesSold"
        )
outliers <- boxplot.stats(dataset1$copiesSold)$out
print(outliers)  # List of outliers

genre_counts <- as.data.frame(table(combined_dataset$Metacritic.score))
colnames(genre_counts) <- c("Metacritic.score", "Count")

# Make sure to convert the appropriate columns to factor (if necessary) for a better visualization
genre_counts$Metacritic.score <- as.factor(genre_counts$Metacritic.score)

# Create a bar chart (corrected)
ggplot(genre_counts, aes(x = Metacritic.score, y = Count, fill = Metacritic.score)) +
  geom_bar(stat = "identity") + # Use identity to plot the counts
  theme_minimal() +
  labs(title = "Metacritic Scores Distribution",
       x = "Metacritic Score",
       y = "Count")


ggplot(dataset1and3, aes(x = log(revenue),y=log(Peak.CCU)))+
  geom_point(color="blue",alpha = 0.6 )+
  theme_minimal()+
  labs(title = "Scatter Plot",
       x = "AVG PLayTime",
       y = "Coppies SOld") +
  scale_y_continuous(labels = scales::comma) 
  
sum(is.na(combined_dataset$Peak.CCU))
sum(dataset$Peak.CCU == 0, na.rm = TRUE)
unique(combined_dataset$Tags)

zero <- combined_dataset %>% filter(combined_dataset$Peak.CCU == 0)

ggplot(dataset1and3,aes(x=log(revenue),y=log(Peak.CCU)))+
  geom_smooth(model="lm",color="red",se=FALSE)+
  geom_point()

sum(combined_dataset$Peak.CCU == 0)

# Scatter plot between `price` and `revenue`
ggplot(combined_dataset, aes(x = price, y = log1p(revenue))) +
  geom_point(aes(color = publisherClass)) + 
  theme_minimal() + 
  labs(title = "Price vs Revenue", x = "Price", y = "Revenue")

# Scatter plot between `avgPlaytime` and `reviewScore`
ggplot(combined_dataset, aes(x = log1p(avgPlaytime), y = reviewScore)) + 
  geom_point(aes(color = publisherClass)) + 
  theme_minimal() + 
  labs(title = "Average Playtime vs Review Score", x = "Avg Playtime", y = "Review Score")

ggplot(combined_dataset, aes(x = publisherClass, y = price)) + 
  geom_boxplot(fill = "lightblue") + 
  theme_minimal() + 
  labs(title = "Price Distribution by Publisher Class", x = "Publisher Class", y = "Price")

ggplot(combined_dataset, aes(x = Genres, y = reviewScore)) + 
  geom_boxplot(fill = "lightgreen") + 
  theme_minimal() + 
  labs(title = "Review Score by Genre", x = "Genre", y = "Review Score")

library(GGally)
ggpairs(numeric_features)

# Select numeric features for the pairwise plot
numeric_features <- combined_dataset %>% 
  select(price, revenue, avgPlaytime,Peak.CCU)

ggpairs(numeric_features, 
        aes(color = numeric_features$price),  # Color by Publisher Class (optional)
        title = "Pairwise Plot of Numeric Features")


