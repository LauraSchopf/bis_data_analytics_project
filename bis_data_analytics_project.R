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
idx <- sample(nrow(user_features), 300)

sample_scaled <- features_scaled[idx, ]
sample_users  <- user_features[idx, ]  # same users

# Hierarchical clustering
dist_mat <- dist(sample_scaled, method = "euclidean")
hc <- hclust(dist_mat, method = "ward.D2")

plot(hc, labels = FALSE, hang = -1,
     main = "Hierarchical Clustering of Users (Ward, n=300)")

# Cutting dendrogram

k <- 4
clusters <- cutree(hc, k = k)
table(clusters)

# Adding cluster labels

sample_users$cluster <- factor(clusters)

aggregate(sample_users %>% select(-user_id),
          by = list(cluster = sample_users$cluster),
          mean)

#check cluster sizes
table(sample_users$cluster)


# full standardized feature matrix, no sampling
features_full_scaled <- scale(user_features[,-1])  # remove user_id

# run k-means with k=4
set.seed(123)

kmeans_4 <- kmeans(
  features_full_scaled,
  centers = 4,
  nstart = 25
)

# attach cluster labels
user_features$cluster_k4 <- factor(kmeans_4$cluster)

# Check cluster sizes
table(user_features$cluster_k4)

# see aggregate values for each cluster and variable
aggregate(
  user_features %>% select(-user_id),
  by = list(cluster = user_features$cluster_k4),
  mean
)

# evaluate cluster quality -> too computationally intensive, computer froze up
library(cluster)

#sil_k4 <- silhouette(
#  kmeans_4$cluster,
#  dist(features_full_scaled)
#)

#evaluating cluster quality on a random sample of the observations
set.seed(123)

eval_idx <- sample(nrow(features_full_scaled), 500)
features_eval <- features_full_scaled[eval_idx, ]
clusters_eval <- kmeans_4$cluster[eval_idx]

#compute silhouette on sample
sil_sample <- silhouette(
  clusters_eval,
  dist(features_eval)
)

mean(sil_sample[, 3])
#mean of sil_sample is 0.3237394

#WSS on full data
wss <- sapply(3:6, function(k) {
  kmeans(features_full_scaled, centers = k, nstart = 10)$tot.withinss
})

wss
# results of wss [1] 293356.7 257236.4 216643.5 199893.8

#creating some visuals

#silhouette analysis plot
#install.packages("factoextra")
library(factoextra)
library(FactoMineR)
# Visualize kmeans clustering


# Explicit PCA (already scaled, so scale.unit = FALSE)
pca_res <- PCA(features_full_scaled, scale.unit = FALSE, graph = FALSE)
pca_coords <- pca_res$ind$coord
#calculate axes


# Visualize clusters in PCA space
fviz_cluster(
  kmeans_4,
  data = pca_coords,
  geom = "point",
  ellipse.type = "norm",
  repel = TRUE
) +
  coord_cartesian(xlim = c(-4,6), ylim = c(-10,4)) +
  theme_minimal() +
  ggtitle("K-means Clusters (PCA projection)")


fviz_silhouette(sil_sample, label = FALSE, print.summary = TRUE)

#k=5 might provide better clusters according to WSS
# run k-means with k=5
set.seed(123)

kmeans_5 <- kmeans(
  features_full_scaled,
  centers = 5,
  nstart = 25
)

# attach cluster labels
user_features$cluster_k5 <- factor(kmeans_5$cluster)

# Check cluster sizes
table(user_features$cluster_k5)

# see aggregate values for each cluster and variable
aggregate(
  user_features %>% select(-user_id),
  by = list(cluster = user_features$cluster_k5),
  mean
)

#evaluating cluster quality on a random sample of the observations

clusters_eval5 <- kmeans_5$cluster[eval_idx]

#compute silhouette on sample
sil_sample5 <- silhouette(
  clusters_eval5,
  dist(features_eval)
)

mean(sil_sample5[, 3])
#silhouette is 0.335

# for k=5 Visualize clusters in PCA space
fviz_cluster(
  kmeans_5,
  data = pca_coords,
  geom = "point",
  ellipse.type = "norm",
  repel = TRUE
) +
  coord_cartesian(xlim = c(-4,6), ylim = c(-10,4)) +
  theme_minimal() +
  ggtitle("K-means Clusters for k=5 (PCA projection)")
