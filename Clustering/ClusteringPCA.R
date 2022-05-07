# Install the ms excel reading package.
if (!require("readxl"))
  install.packages("readxl")

# Install the ggfortify package if its not installed already.
if (!require("ggfortify"))
  install.packages("ggfortify")

# Load the library
library("readxl")
library("ggfortify")
library(scales)
library(cluster)
library(NbClust)
library(stats)
library(factoextra)
library(caret)
library(tidyverse)

# Load the data.
white_wine_data <- read_excel("Whitewine_v2.xlsx")

# Remove the outliers using the interquartile range.
remove_outliers <- function(dataset, columns = names(dataset))
{
  no_outliers <- dataset
  for (column in columns) {
    column_data <- no_outliers[[column]]
    
    no_outliers <- subset(
      no_outliers,
      column_data > (quantile(column_data, 0.25) - 1.5 * IQR(column_data)) &
        column_data < (quantile(column_data, 0.75) + 1.5 * IQR(column_data))
    )
  }
  
  return(no_outliers)
}

column_names = c(
  "fixed acidity",
  "volatile acidity",
  "citric acid",
  "residual sugar",
  "chlorides",
  "free sulfur dioxide",
  "total sulfur dioxide",
  "density",
  "pH",
  "sulphates",
  "alcohol"
)

# First attempt to remove most of the outliers.
clean_white_wine_data <-
  remove_outliers(white_wine_data,
                  column_names)

# Second and final attempt to remove the remaining outliers.
clean_white_wine_data <-
  remove_outliers(clean_white_wine_data,
                  column_names)

# Scale the clean white wine data.
scaled_clean_wine_data <-
  as.data.frame(scale(clean_white_wine_data[1:11]))
boxplot(scaled_clean_wine_data)

# Apply PCA
pca <- prcomp(scaled_clean_wine_data, center = TRUE, scale. = TRUE)
summary(pca)

wine_pca_plot <- autoplot(pca, data = scaled_clean_wine_data)
plot(wine_pca_plot)

# Create the new data sets using the PCA.
scaled_clean_wine_data <- scaled_clean_wine_data[1:9]

# Find the optimal cluster count using the elbow method (manual).
wss <- 0

for (i in 1:10) {
  km.out <- kmeans(scaled_clean_wine_data, centers = i)
  wss[i] <- km.out$tot.withinss
}
wss

plot(1:10, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

# Find the optimal cluster count using the elbow method (automated).
fviz_nbclust(scaled_clean_wine_data, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow method")

# Find the optimal cluster count using the silhouette method (manual).
silhoutte_function <- map_dbl(2:10,  function(k){
  model <- pam(x = scaled_clean_wine_data, k = k)
  model$silinfo$avg.width
})

silhouette_data <- data.frame(
  k = 2:10,
  sil_width = silhoutte_function
)

ggplot(silhouette_data, aes(x = k, y = silhoutte_function)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 2:10)

# Find the optimal cluster count using the silhouette method (automated).
fviz_nbclust(scaled_clean_wine_data, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

starting_point_cout <- 25
#################### Perform K-Means with 2 clusters ####################
set.seed(123)
result <-
  kmeans(scaled_clean_wine_data, 2, nstart = starting_point_cout)
result

# Create the confusion matrix to evaluate the outputs.
confusionMatrix(as.factor(predict(result, scaled_clean_wine_data)),
                as.factor(clean_white_wine_data$quality - 4))

# Show the results.
fviz_cluster(result,
             scaled_clean_wine_data,
             geom = "point",
             ellipse.type = "convex")

#################### Perform K-Means with 3 clusters ####################
set.seed(123)
result <-
  kmeans(scaled_clean_wine_data, 3, nstart = starting_point_cout)
result

# Create the confusion matrix to evaluate the outputs.
confusionMatrix(as.factor(predict(result, scaled_clean_wine_data)),
                as.factor(clean_white_wine_data$quality - 4))

# Show the results.
fviz_cluster(result,
             scaled_clean_wine_data,
             geom = "point",
             ellipse.type = "convex")

#################### Perform K-Means with 4 clusters ####################
set.seed(123)
result <-
  kmeans(scaled_clean_wine_data, 4, nstart = starting_point_cout)
result

# Create the confusion matrix to evaluate the outputs.
confusionMatrix(as.factor(predict(result, scaled_clean_wine_data)),
                as.factor(clean_white_wine_data$quality - 4))

# Show the results.
fviz_cluster(result,
             scaled_clean_wine_data,
             geom = "point",
             ellipse.type = "convex")