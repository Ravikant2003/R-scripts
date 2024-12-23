# Step 1: Load libraries
library(ggplot2)    # For visualization
library(cluster)    # For clustering

# Step 2: Load data
data <- read.csv("C:/Users/ravik/OneDrive/Desktop/network_traffic.csv")

# Step 3: Preprocess data
# Select relevant columns and remove missing values
selected_data <- data[, c("Duration", "Source_Bytes", "Destination_Bytes")]

# Standardize the data
scaled_data <- scale(selected_data)

# Step 4: Apply K-Means Clustering
set.seed(42)  # Ensure reproducibility
kmeans_model <- kmeans(scaled_data, centers = 3, nstart = 10)

# Add cluster labels to the data
data$Cluster <- kmeans_model$cluster

# Step 5: Calculate distances from cluster centroids
centroids <- kmeans_model$centers
distances <- apply(scaled_data, 1, function(row) min(sqrt(rowSums((t(centroids) - row)^2))))

# Step 6: Identify anomalies (top 5% based on distance)
threshold <- quantile(distances, 0.95)
data$Anomaly <- ifelse(distances > threshold, "Yes", "No")

# Step 7: Visualize anomalies
ggplot(data, aes(x = Source_Bytes, y = Destination_Bytes, color = Anomaly)) +
  geom_point() +
  labs(title = "Anomaly Detection", x = "Source Bytes", y = "Destination Bytes")

# Step 8: Save results
write.csv(data, "C:/Users/ravik/OneDrive/Desktop/network_anomalies.csv", row.names = FALSE)
