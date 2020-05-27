

### SPLIT
data_train <- sample(1:nrow(data),0.10*nrow(data))
train <- data[data_train,]

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(train[,2:105], train[,105], sizes=c(10,20,30,40), rfeControl=control)


### BORUTA METHOD


data <- HealthSurvey

str(data)

##REMOVE NA INTEGER
int <- data[,sapply(data, is.integer)]
int[is.na(int)] <- 0

##Remove NA FACTOR
fact <- data[,sapply(data, is.factor)]
fct_lgl <- sapply(fact,is.factor)
fact[fct_lgl] <- lapply(
  fact[fct_lgl], 
  function(x) {
    x <- addNA(x)
    levels(x)[nlevels(x)] <- "zero"
    x})

str(fact)

data <- cbind(int, fact)
str(data)

# load the library
library(mlbench)
library(caret)

# ensure the results are repeatable
set.seed(100)
### Columns (more than 53 levels)
state <- data$X_state
data$X_state <- NULL
data$exract11 <- NULL
data$exract21 <- NULL

data$weight2 <- as.numeric(data$weight2)



### SPLIT
data_train <- sample(1:nrow(data),0.50*nrow(data))
train <- data[data_train,]



#### BORUTA
install.packages("Boruta")
library(Boruta)
set.seed(100)
boruta.train <- Boruta(train$genhlth~., data = train, doTrace = 2)
print(boruta.train)

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)


final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)
getSelectedAttributes(final.boruta, withTentative = F)
boruta.df <- attStats(final.boruta)
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
