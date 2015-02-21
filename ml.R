setwd("~/GitHub/datasciencecoursera/Machine Learning")

library(randomForest)
library(caret)
library(e1071)

train  <- read.csv("pml-training.csv", header=TRUE, na.strings = c("", " ","NA"))
test  <- read.csv("pml-testing.csv", header=TRUE, na.strings = c("", " ","NA"))

#get rid of columns where at least half the rows in the training set are NAs
train2 <- train[,colSums(is.na(train)) < .5*nrow(train)]
test2 <- test[,colSums(is.na(train)) < .5*nrow(train)]

train3 <- train2[,-c(1:7)]
test3 <- test2[,-c(1:7)]

#make all values numeric, except predictor value
for (i in 1:52){
   train3[,i] <- as.numeric(train3[,i])
   test3[,i] <- as.numeric(test3[,i])   
}


# segment the training set into subsets: training and test
set.seed(321)
subTrain <- createDataPartition(y = train3$classe,
                               p = .8,
                               list = FALSE)

training <- train3[subTrain,]
testing <- train3[-subTrain,]

#model
rf_model <- randomForest(classe~.,data=training, ntree=250, importance=TRUE)
predTrain <- predict(rf_model,testing); predAcc <- predTrain==testing$classe
table(predTrain,testing$classe)
prop.table(table(predAcc))
#99.5% accurate!

#an alternate model
rf_model2 <- train(classe~., data=training, method='rf', metric='Accuracy',
                  trControl=trainControl(method="cv", number=4, classProbs=T))
predTrain2 <- predict(rf_model2,testing); predAcc2 <- predTrain2==testing$classe
table(predTrain2,testing$classe)
#slightly less accurate!


predTest <- predict(rf_model,newdata=test3)
table(predTest)

pml_write_files = function(x){
   n = length(x)
   for(i in 1:n){
      filename = paste0("problem_id_",i,".txt")
      write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
   }
}

pml_write_files(predTest)
