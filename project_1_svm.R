library("e1071")

### Normalization and data preparation

load("data_newFeatures_z.RData")

groups.svm <- factor(c(rep("other", 12), rep("other", 6), rep("good", 12)), levels=c("good", "other"))

data.newFeatures.z <- data.frame(calc_zscore_in_columns(t(data.newFeatures)), stringsAsFactors = F)

groups.df <- data.frame(Index = seq(1:30),
                        Group = groups.svm,
                        Sample = rownames(data.newFeatures.z))
data.newFeatures.z$groups <- groups.svm

### Tuning SVM on whole data
svm.tuned <- tune.svm(groups ~ ., data = data.newFeatures.z, gamma = 2^(-15:3), cost = 2^(-5:15), kernel = "radial")

### Leave-3-out cross-validation
randomIndex <- sample(seq(1:30))

rm(decVals.table)
for (i in seq(1, 30, 5)) {
  
  block <- c(i:(i+4))
  indeces.test <- randomIndex[block]
  
  data.train <- data.newFeatures.z[-indeces.test, ]
  data.test <- data.newFeatures.z[indeces.test, ]
  
  svm.model <- svm(groups ~ ., data=data.train, cost=svm.tuned$best.parameters$cost, 
                   gamma=svm.tuned$best.parameters$gamma, kernel = "radial", decision.values = T)
  
  decVals.prediction <- predict(svm.model, data.test, decision.values = T)
  decVals.vals <- attributes(decVals.prediction)$decision.values
  
  if (i == 1) {
    decVals.table <- decVals.vals
  } else {
    decVals.table <- rbind(decVals.table, decVals.vals)
  }
  
}

save.image(file="project_1_svm.RData")