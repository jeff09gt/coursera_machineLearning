

install.packages("randomForest", repos = "http://cran.rstudio.com", type = "source")


library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rattle)
library(randomForest)


if(!file.exists("pml-training.csv")){
  myURL = paste("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",sep = "")
  
  dir = "pml-training.csv"
  download.file(myURL, dir, mode="wb")  
}


if(!file.exists("pml-testing.csv")){
  myURL = paste("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",sep = "")
  
  dir = "pml-testing.csv"
  download.file(myURL, dir, mode="wb")    
}


trainSet <- read.csv("pml-training.csv", na.strings = c("NA", "#DIV/0!", ""))
testSet<-read.csv("pml-testing.csv",na.strings=c("NA","#DIV/0!"))


NA_Count = sapply(1:dim(trainSet)[2],function(x)sum(is.na(trainSet[,x])))
NA_list = which(NA_Count>0)

trainSet = trainSet[,-NA_list]
trainSet = trainSet[,-c(1:7)]
trainSet$classe = factor(trainSet$classe)

NA_Count1 = sapply(1:dim(testSet)[2],function(x)sum(is.na(testSet[,x])))
NA_list1 = which(NA_Count1>0)
testSet = testSet[,-NA_list]
testSet = testSet[,-c(1:7)]


dim(trainSet)

dim(testSet)


inTrain=createDataPartition(y=trainSet$classe, p=0.6, list=FALSE)
training <-trainSet[inTrain,]
testing <- trainSet[-inTrain,]

dim(training)

dim(testing)

try_fit01 <- train(classe ~ .,method='rpart',data=training)
fancyRpartPlot(try_fit01$finalModel) 


fit01_pred=predict(try_fit01,newdata=testing)
z=confusionMatrix(fit01_pred,testing$classe)
z$table

z$overall[1]

try_fit02=randomForest(classe~., data=training, method='class')
fit02_pred = predict(try_fit02,testing,type='class') 
qplot(roll_belt, magnet_dumbbell_y, colour=classe, data=training)  

z2=confusionMatrix(fit02_pred,testing$classe)
z2$table

z2$overall[1]



fit03_pred =  predict(try_fit02,testSet,type='class')
nofiles = length(fit03_pred)
for (i in 1:nofiles){
  filename =  paste0("problem_id",i,".txt")
  write.table(fit03_pred[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
}
fit03_pred




