library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(plyr)
library(caret)

data <- datacluster
str(data)
data$X.1 <- NULL
data$X <- NULL

data$clusternum <- NULL
data$kmeans.cluster <- NULL
data$HealthSurvey.X_state <- NULL
data$HealthSurvey.sex.1 <- NULL
data$HealthSurvey.sex <- NULL
data$HealthSurvey.income2 <- NULL
data$HealthSurvey.physhlth <- NULL
data$HealthSurvey.menthlth <- NULL
str(data)

dummy <- dummyVars("~.", data=data)
data <- data.frame(predict(dummy, newdata = data))
data <- scale(data)
### sampling for pca

### data.sample.1000 <- sample(1:nrow(data),1000)
### data.sample.1000 <- data[data.sample.1000,]

### pca <- prcomp(data.sample.1000, center = TRUE,scale. = TRUE)
#summary(pca)
#str(pca)
#install_github("vqv/ggbiplot")
#library(ggbiplot)
#ggbiplot(pca, groups= kmeans$fac.cluster)



#### ELBOW METHOD ####

library("purrr")
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(data, k, nstart = 15 )$tot.withinss
}

wss_values <- map_dbl(k.values, wss)
# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


### silhouette method ###


# extract wss for 2-15 clusters
#avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}


# Compute and plot wss for k = 2 to k = 15
#k.values <- 2:15

# extract avg silhouette for 2-15 clusters
#avg_sil_values <- map_dbl(k.values, avg_sil)


# plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

##### do not works!

##### kmenas with 6

kmeans2 <- kmeans(data, centers = 6, nstart =25)
print(kmeans)
View(kmeans$cluster)


fviz_cluster(kmeans, data = data, geom=c("point"), ellipse.alpha  = 0.05)

write.csv(new_cluster, "6cluster_data.csv")
