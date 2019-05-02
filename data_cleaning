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
set.seed(100)
### dataset 

data <- cbind(int, fact)
str(data)
