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


install.packages("rgl")
library(rgl)
plot3d(pca$x[,1:3], col=kmeans$fac.cluster)
kmeans$fac.cluster <- as.factor(kmeans$cluster)


?ggbiplot

set.seed(123)
library("purrr")
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(data, k, nstart = 15 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:25

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

fviz_nbclust(data, kmeans, method = "wss")

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

##### kmenas with 6

kmeans <- kmeans(data, centers = 6, nstart =25)
print(kmeans)
View(kmeans$cluster)

## sample for plot
data.sample.index <- sample(1:nrow(data),1000)
data.sample <- data[data.sample.index,]

kmenas.sam
kmeans.sample <- kmeans$cluster[data.sample.index]

fviz_cluster(kmeans, data = data, geom=c("point"), ellipse.alpha  = 0.05)
write.csv(datast, "clusterdf.csv")
qplot(datast$physhlth, datast$menthlth, colour = datast$`kmeans$cluster`, 
      data = datast)
plot(datast$`kmeans$cluster`, datast$`kmeans$cluster`)


datacluster$X.1 <- NULL
datacluster$kmeans.cluster <- NULL
datacluster$HealthSurvey.sex.1 <- NULL
datacluster$X <- NULL
datacluster$clusternum <- NULL
new_cluster <- cbind(datacluster, kmeans$cluster)



write.csv(new_cluster, "6cluster_data.csv")
