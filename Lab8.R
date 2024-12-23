# Load necessary libraries
library(ggplot2)
library(factoextra)

# Step 1: Load the dataset
data <- read.csv("C:/Users/ravik/OneDrive/Desktop/Mall_Customers.csv")

# Step 2: Preprocessing
clustering_data <- data[, c("Annual.Income", "Spending.Score")]
colnames(clustering_data) <- c("Annual_Income", "Spending_Score")
scaled_data <- scale(clustering_data)

# Step 3: Determine optimal clusters using the Elbow method
set.seed(42)
wcss <- sapply(1:10, function(i) kmeans(scaled_data, centers = i, nstart = 10)$tot.withinss)
plot(1:10, wcss, type = "b", pch = 19, xlab = "Number of Clusters", ylab = "WCSS", main = "Elbow Method")

# Step 4: Apply K-Means Clustering
optimal_clusters <- 5
kmeans_result <- kmeans(scaled_data, centers = optimal_clusters, nstart = 10)
data$Cluster <- kmeans_result$cluster

# Step 5: Visualize Clusters with PCA
pca_result <- prcomp(scaled_data)
pca_data <- data.frame(pca_result$x[, 1:2], Cluster = as.factor(kmeans_result$cluster))
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) + 
  geom_point(size = 3) + 
  labs(title = "Customer Segmentation using PCA", x = "PC1", y = "PC2") + 
  theme_minimal()

# Step 6: Save the segmented dataset
write.csv(data, "C:\\path_to_save\\segmented_customers.csv", row.names = FALSE)
