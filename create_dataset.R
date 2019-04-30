data <- brfss2013[,c(1:102)]

##deleting main data
data <- data[,-c(2:6)]
data <- data[,-c(3:5)]
data <- data[,-c(6:7)]


data$pre_diabetic <- brfss2013$prediab1
brfss2013$diabage2 <- as.numeric(brfss2013$diabage2)
data$have_diabets <- ifelse(brfss2013$diabage2 > 1, 1,0)
### HEALTH CARE
data$have_medicare <- brfss2013$medicare
data$have_bills <- brfss2013$medbills
data$ssbsugar <-  brfss2013$ssbsugar
data$ssbfrut2 <-  brfss2013$ssbfrut2

### ASTHMA
brfss2013$asthmage[is.na(brfss2013$asthmage)]<-0
table(brfss2013$asthmage)
data$have_asthma <- ifelse(brfss2013$asthmage > 0, 1,0)
table(data$have_asthma)

### Woman health
data$hpvadvc2 <- brfss2013$hpvadvc2

data$hadmam <- brfss2013$hadmam
data$hadpap2 <- brfss2013$hadpap2
### mental Health

data$misdeprd <- brfss2013$misdeprd
data$mistmnt <- brfss2013$mistmnt
 ## Money

data$scntmony <- brfss2013$scntmony
data$scntmeal <- brfss2013$scntmeal
data$scntlwk1 <- brfss2013$scntlwk1
data$lsatisfy <- brfss2013$lsatisfy
### Only completed interview
data <- subset(data, data$dispcode == "Completed interview")

data <- data[,-c(2)]


write.csv(data, "HealthSurvey.csv")


