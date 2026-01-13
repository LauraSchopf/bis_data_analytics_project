library(here)
library(dplyr)
traveldata <- read.csv(here("data", "travel_platform.csv"))

# User-level table
user_features <- traveldata %>%
  group_by(user_id) %>%
  summarise(
    avg_distance = mean(orig_destination_distance, na.rm = TRUE),
    avg_adults = mean(srch_adults_cnt, na.rm = TRUE),
    avg_children = mean(srch_children_cnt, na.rm = TRUE),
    search_frequency = sum(cnt, na.rm = TRUE),
    booking_rate = mean(is_booking, na.rm = TRUE),
    mobile_share = mean(is_mobile, na.rm = TRUE),
    package_share = mean(is_package, na.rm = TRUE)
  ) %>%
  na.omit()

# Standardize and sample size 300

features_scaled <- scale(user_features[,-1])  # remove user_id

set.seed(123)
sample_300 <- features_scaled[sample(nrow(features_scaled), 300), ]

# Hierarchical clustering
dist_mat <- dist(sample_300)
hc <- hclust(dist_mat, method = "ward.D2")
plot(hc, main = "Hierarchical Clustering of Users (Ward, n=300)")

# Cutting dendrogram

k <- 4
clusters <- cutree(hc, k = k)
table(clusters)

# Adding cluster labels

sample_users <- user_features[sample(nrow(user_features), 300), ]
sample_users$cluster <- clusters

# Means per cluster 
aggregate(sample_users[,-1], by = list(cluster = sample_users$cluster), mean)







