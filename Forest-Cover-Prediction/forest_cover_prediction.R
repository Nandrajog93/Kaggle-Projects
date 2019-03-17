
library(caret)
library(randomForest)
library(ranger)
library(e1071)
# Forest cover prediction -------------------------------------------------

data_forest <- read.csv(file = "train.csv", header=TRUE, sep=",")
names = colnames(data_forest)[12:56]
for( i in 1:12){
  data_forest[,i] = scale(data_forest[,i])
}
for( i in 12:56){
  data_forest[,i]  =as.factor(data_forest[,i])
}
#feature columns 


set.seed(107)
inTrain = createDataPartition(y = data_forest$Cover_Type,
                              p = .75,
                              list = FALSE)
head(inTrain)
training = data_forest[ inTrain, ]
testing = data_forest[-inTrain, ]


# Random Forest -----------------------------------------------------------

rf = randomForest(Cover_Type ~ ., data = training,
                  importance = T)

yhat.rf = predict(rf,newdata=testing[,-56])

round( mean( yhat.rf != testing$Cover_Type )*100, 3)
confusionMatrix(yhat.rf,testing$Cover_Type)

# SVM ---------------------------------------------------------------------

svmfit=svm(as.factor(Cover_Type)~ ., data = training, kernel="radial", cost=10,scale=FALSE, cross = 10)
yhat.rf = predict(svmfit,newdata=testing[,-56])
round( mean( yhat.rf != testing$Cover_Type )*100, 3)
confusionMatrix(yhat.rf,testing$Cover_Type)

svm_tune <- tune(svm, as.factor(Cover_Type)~ ., data = training, kernel="radial", ranges=list(cost=2^(-1:5)
                                                                                            , gamma=c(.5,1,2)))

bestmod=svm_tune$best.model
summary(bestmod)
bestmod$cost
bestmod$kernel
bestmod$degree
bestmod$gamma



# Something with Ranger ---------------------------------------------------
library(ranger)
rang3 <- ranger(as.factor(Cover_Type) ~ .,
                write.forest=TRUE,
                data=training, num.trees = 2000) 
yhat.rf.ranger= predict(rang3,data=testing[,-56])
round( mean( yhat.rf.ranger$predictions != testing$Cover_Type )*100, 3)
confusionMatrix(yhat.rf.ranger$predictions,testing$Cover_Type)

