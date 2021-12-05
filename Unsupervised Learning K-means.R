library(tidyverse)
library(caret)
library(e1071)
library(mlbench)

# unsupervised learning
# K-Means clustering
head(iris)

# remove species column
species <- iris$Species

iris_data <- iris %>%
  select(-Species)

#model fitting
set.seed(27)
km_result <- kmeans(iris_data, centers = 3)

#visualize clusters
plot(x = iris_data$Petal.Length,
     y = iris_data$Petal.Width,
     pch = km_result$cluster,
     col = km_result$cluster)

#cluster centers
km_result$centers[, 3]
km_result$centers[, 4]

#centroid(center of cluster)
points(x = km_result$centers[, 3],
       y = km_result$centers[, 4],
       pch = 8,
       cex = 3)

#add new column to original data
iris_data$cluster <- km_result$cluster

#cluster profile
iris_data %>%
  group_by(cluster) %>%
  summarise(avg_sp_length = mean(Sepal.Length),
            avg_sp_width = mean(Sepal.Width),
            n = n()) %>%
  mutate(pct = n/sum(n))