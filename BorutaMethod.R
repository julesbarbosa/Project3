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
