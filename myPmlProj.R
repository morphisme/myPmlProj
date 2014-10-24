# Practical Machine Learning, Course Project
library(caret)
# files are supposed to be in current directory
train <- read.csv("pml-training.csv", stringsAsFactors=FALSE)
train <- train[, -1]
n <- dim(train)[1]
p <- dim(train)[2]
labels <- factor(train[, p])
train <- train[, -p]
test <- read.csv("pml-testing.csv", stringsAsFactors=FALSE)
test <- test[, -1]
n2 <- dim(test)[1]
test <- test[, -p]

# there are some "" and NA to transform in the data:
tout <- rbind(train, test)
for (j in 1:(p-1)){
   if (class(train[, j])=="numeric"){
      m <- mean(tout[, j], na.rm=TRUE)
      train[is.na(train[, j]), j] <- m
      test[is.na(test[, j]), j] <- m
   } else {
      if (class(train[, j])=="integer"){
         m <- floor(mean(tout[, j], na.rm=TRUE) + 0.5)
         train[is.na(train[, j]), j] <- m
         test[is.na(test[, j]), j] <- m
      } else {
         if (class(train[, j])=="character"){
            tout[is.na(tout[, j]), j] <- "zzz"
            tout[tout[, j]=="", j] <- "zz"
            train[is.na(train[, j]), j] <- "zzz"
            train[train[, j]=="", j] <- "zz"
            test[is.na(test[, j]), j] <- "zzz"
            test[test[, j]=="", j] <- "zz"
            tout[, j] <- factor(tout[, j])
            niv <- levels(tout[, j])
            if (length(niv) > 53){  # max categorical predictors that randomForest can handle
               train[, j] <- as.numeric(tout[1:n, j])
               test[, j] <- as.numeric(tout[1:n2 +n, j])
            } else {
               train[, j] <- factor(train[, j], levels=niv)
               test[, j] <- factor(test[, j], levels=niv)
            }
         }
      }
   }
}

set.seed(777777777) # to be able to redo the same processing
index <- createDataPartition(labels, p=0.6, list=FALSE)
train1 <- train[index, ]
labels1 <- factor(labels[index], levels=levels(labels))
test1 <- train[-index, ]
labels1t <- factor(labels[-index], levels=levels(labels))

mon.cRf <- train(x=train1, y=labels1) # default method = randomForest
guess1 <- predict(mon.cRf, newdata=test1)
sum((labels1t != guess1)*1) # only 9 differences (out of 7846 rows) between predicted and observed classes!

guess <- predict(mon.cRf, newdata=test)

# output using provided code
pml_write_files <- function(x){
  n <- length(x)
  for(i in 1:n){
    filename <- paste0("problem_id_", i, ".txt")
    write.table(x[i], file=filename, quote=FALSE, row.names=FALSE, col.names=FALSE)
  }
}
pml_write_files(guess)
