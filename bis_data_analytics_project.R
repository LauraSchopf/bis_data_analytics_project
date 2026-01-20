# Load required packages
library(here) # for reproducible file path
library(dplyr) # for data manipulation

# Load datset
traveldata <- read.csv(here("data", "travel_platform.csv"))
str(traveldata) # exploring structure of dataset

############################################################
## HIERARCHICAL CLUSTERING
############################################################

# Construct user-level behavioral features by aggregating search sessions
# Only continuous, meaningful variables are kept (no IDs, no categories)
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
  na.omit() # remove users with missing aggregated values

# Standardize features so each variable contributes equally to Euclidean distance
features_scaled <- scale(user_features[,-1])  # remove user_id column

# Hierarchical clustering is O(n^2), so we work on a random subsample
set.seed(123)
idx <- sample(nrow(user_features), 300)

sample_scaled <- features_scaled[idx, ]
sample_users  <- user_features[idx, ]   # same users, unscaled

# Compute Euclidean distance matrix
dist_mat <- dist(sample_scaled, method = "euclidean")

# Apply Ward's hierarchical clustering (minimizes within-cluster variance)
hc <- hclust(dist_mat, method = "ward.D2")

# Plot dendrogram
plot(hc, labels = FALSE, hang = -1,
     main = "Hierarchical Clustering of Users (Ward, n=300)")

# Cut the dendrogram into k clusters based on visual inspection
k <- 4
clusters <- cutree(hc, k = k)
table(clusters)

# Attach cluster labels to users

sample_users$cluster <- factor(clusters)

# Compute cluster centroids (mean behavior per cluster)

aggregate(sample_users %>% select(-user_id),
          by = list(cluster = sample_users$cluster),
          mean)

# Check cluster sizes
table(sample_users$cluster)

############################################################
## K-MEANS CLUSTERING (FULL DATA)
############################################################

# Standardize full dataset (no sampling)
features_full_scaled <- scale(user_features[,-1])  # remove user_id

# Run k-means with k = 4 (chosen from hierarchical dendrogram)

set.seed(123)

kmeans_4 <- kmeans(
  features_full_scaled,
  centers = 4,
  nstart = 25
)

# Attach cluster labels

user_features$cluster_k4 <- factor(kmeans_4$cluster)

# Inspect cluster sizes

table(user_features$cluster_k4)

# Compute cluster centroids and see aggregate values for each cluster and variable

aggregate(
  user_features %>% select(-user_id),
  by = list(cluster = user_features$cluster_k4),
  mean
)

############################################################
## CLUSTER VALIDATION (SILHOUETTE ON SUBSAMPLE)
############################################################

library(cluster)

# evaluate cluster quality -> too computationally intensive, computer froze up
# sil_k4 <- silhouette(
#  kmeans_4$cluster,
#  dist(features_full_scaled)
#)

## Silhouette on full data is too heavy -> use random sample

set.seed(123)

eval_idx <- sample(nrow(features_full_scaled), 500)
features_eval <- features_full_scaled[eval_idx, ]
clusters_eval <- kmeans_4$cluster[eval_idx]

#compute silhouette on sample
sil_sample <- silhouette(
  clusters_eval,
  dist(features_eval)
)

mean(sil_sample[, 3]) # average silhouette width
# Mean of sil_sample is 0.3237394

############################################################
## ELBOW METHOD (WITHIN-CLUSTER SUM OF SQUARES)
############################################################


# WSS on full data
wss <- sapply(3:6, function(k) {
  kmeans(features_full_scaled, centers = k, nstart = 10)$tot.withinss
})

wss
# results of wss [1] 293356.7 257236.4 216643.5 199893.8
# decreasing WSS suggests k = 4 or 5

############################################################
## PCA VISUALIZATION OF CLUSTERS
############################################################

# Silhouette analysis plot
install.packages("factoextra")
install.packages("emmeans")
install.packages("FactoMineR")

library(factoextra)
library(FactoMineR)

# Visualize kmeans clustering

# PCA for 2D visualization of high-dimensional clusters
# Explicit PCA (already scaled, so scale.unit = FALSE)

pca_res <- PCA(features_full_scaled, scale.unit = FALSE, graph = FALSE)
pca_coords <- pca_res$ind$coord
# Calculate axes


# Plot k=4 clusters in PCA space

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


# Silhouette plot

fviz_silhouette(sil_sample, label = FALSE, print.summary = TRUE)

############################################################
## K = 5 COMPARISON
############################################################

# k=5 might provide better clusters according to WSS
# run k-means with k=5
set.seed(123)

kmeans_5 <- kmeans(
  features_full_scaled,
  centers = 5,
  nstart = 25
)

# Attach cluster labels
user_features$cluster_k5 <- factor(kmeans_5$cluster)

# Check cluster sizes
table(user_features$cluster_k5)

# Cluster centroids: see aggregate values for each cluster and variable
aggregate(
  user_features %>% select(-user_id),
  by = list(cluster = user_features$cluster_k5),
  mean
)

# Silhouette for k=5 (same sample)

clusters_eval5 <- kmeans_5$cluster[eval_idx]

sil_sample5 <- silhouette(
  clusters_eval5,
  dist(features_eval)
)

mean(sil_sample5[, 3])
# Silhouette is 0.335

# PCA visualization for k=5
p <- fviz_cluster(
  kmeans_5,
  data = pca_coords,
  geom = "point",
  ellipse.type = "norm",
  repel = TRUE
) +
  coord_cartesian(xlim = c(-4,6), ylim = c(-10,4)) +
  theme_minimal() +
  ggtitle("K-means Clusters for k=5 (PCA projection)")

print(p)


