install.packages("rpart",dependencies=TRUE,repos="http://cran.rstudio.com/")
install.packages("e1071",dependencies=TRUE,repos="http://cran.rstudio.com/")
install.packages("randomForest",dependencies=TRUE,repos="http://cran.rstudio.com/")
install.packages("stargazer",dependencies=TRUE,repos="http://cran.rstudio.com/")
install.packages("ipred",dependencies=TRUE,repos="http://cran.rstudio.com/")
install.packages("adabag",dependencies=TRUE,repos="http://cran.rstudio.com/")
install.packages("VGAM")


rm(list=ls())
library(rpart)
library(e1071)
library(class)
library(randomForest)
library(ipred)
library(adabag)
library(VGAM)

#Data Set
given_data <- read.csv("processed.cleveland.data.txt",header = FALSE,na.strings=c(""))
#Adding column Names to the dataset
colnames(given_data) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")

#Converting test variable to a factor in R
given_data$num <- factor(given_data$num)


KNNcall<-function(given_data)
{

  ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
  model <- train(num ~.,  data=given_data, method="knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 5, tuneGrid = data.frame(k = 4))
  pred = predict(model, given_data, na.action = na.pass)
      
  cm <- confusionMatrix(data=pred, given_data$num)
  #print(cm)
      
  knn_accuracy <- round(cm$overall[1], 3)
  knn_precision <- round(cm$byClass[11], 3)
      
  print(mean(knn_accuracy))
  print(knn_precision)
}

BaggCall<-function(given_data)
{
  n <- names(given_data)
  f	<- as.formula(paste("num	~",	paste(n[!n	%in%	"num"],	collapse	=	"	+	")))
  
  model <- bagging.cv(num ~.,  data=given_data, mfinal=10, v=50, control=rpart.control(cp=0.03))
  bag_accuracy <- 1 - model$error
    
  #calculating precision
  v1 <- model$confusion[1] + model$confusion[2] + model$confusion[3] + model$confusion[4] + model$confusion[5]
  bag_precision <- model$confusion[1] / v1
    
  print(bag_accuracy)
  print(bag_precision)
}

BoostCall<-function(given_data)
{
  n <- names(given_data)
  f	<- as.formula(paste("num	~",	paste(n[!n	%in%	"num"],	collapse	=	"	+	")))
  
  model <- boosting.cv(num ~.,  data=given_data, mfinal=10, v=50, control=rpart.control(cp=0.03))
  boo_accuracy <- 1 - model$error
    
  #calculating precision
  v1 <- model$confusion[1] + model$confusion[2] + model$confusion[3] + model$confusion[4] + model$confusion[5]
  boo_precision <- model$confusion[1] / v1
  
  print(boo_accuracy)
  print(boo_precision)
}

#KNN
averageKNN<-KNNcall(given_data)

#Bagging
AverageBagging<-BaggCall(given_data)

#Boosting
AverageBoosting<-BoostCall(given_data)

##Logistic regression
#Average of 10 samplings.
#converting the data to numeric due to error in scaling - Error in colMeans(x, na.rm = TRUE) : 'x' must be numeric
given_Data$age<-as.numeric(given_Data$age)
given_Data$sex<-as.numeric(given_Data$sex)
given_Data$cp<-as.numeric(given_Data$cp)
given_Data$trestbps<-as.numeric(given_Data$trestbps)
given_Data$chol<-as.numeric(given_Data$chol)
given_Data$fbs<-as.numeric(given_Data$fbs)
given_Data$restecg<-as.numeric(given_Data$restecg)
given_Data$thalach<-as.numeric(given_Data$thalach)
given_Data$exang<-as.numeric(given_Data$exang)
given_Data$oldpeak<-as.numeric(given_Data$oldpeak)
given_Data$slope<-as.numeric(given_Data$slope)
given_Data$ca<-as.numeric(given_Data$ca)
given_Data$thal<-as.numeric(given_Data$thal)
given_Data$num<-as.numeric(given_Data$num)

accuracy<-0
precision<-0

for(i in 1:10)
{
  
  instances<-sample(1:nrow(given_Data),size = 0.95*nrow(given_Data))
  
  trainingDataSet<-given_Data[instances,]
  testDataSet<-given_Data[-instances,]
  
  #Building the model using training data
  LRmodel <- vglm(num~., family=multinomial, data=trainingDataSet, control = vglm.control( maxit = 10, stepsize = 1.5))
  LRModel.prob <- predict(LRmodel, testDataSet[,1:13], type="response")
  LRModel.pred <- apply(LRModel.prob, 1, which.max)
  
  LRModel.pred[which(LRModel.pred=="1")] <- levels(as.factor(testDataSet$num))[1]
  LRModel.pred[which(LRModel.pred=="2")] <- levels(as.factor(testDataSet$num))[2]
  LRModel.pred[which(LRModel.pred=="3")] <- levels(as.factor(testDataSet$num))[3]
  LRModel.pred[which(LRModel.pred=="4")] <- levels(as.factor(testDataSet$num))[4]
  LRModel.pred[which(LRModel.pred=="5")] <- levels(as.factor(testDataSet$num))[5]
  
  confusion<-table(LRModel.pred, testDataSet$num)
  acc<-sum(diag(confusion))/sum(confusion)
  acc<-acc*100
  accuracy<-accuracy+acc
  
}

accuracy<-accuracy/10
print(accuracy)
precision<-sum(diag(confusion))/rowSums(confusion)
precision[1]

#Random Forest
result <- rfcv(given_Data[,-14], given_Data$num,cv.fold=35,scale="log", 
               step=0.3,recursive = FALSE);
accuracy <- 100 - result$error.cv;
print("Accuracy :")
print(accuracy)
pred1 <- c('a','b');

for(i in 1:nrow(given_Data))
{
  pred1[i] <- result$predicted[[1]][i]
  
  if (pred1[i] < 0.5 && pred1[i] >= 0)
  {  
    pred1[i] <- 0;
  }
  
  if (pred1[i] < 1.5 && pred1[i] >= 0.5)
  {  
    pred1[i] <- 1;
  }
  
  if (pred1[i] < 2.5 && pred1[i] >= 1.5)
  {  
    pred1[i] <- 2;
  }
  
  if (pred1[i] < 3.5 && pred1[i] >= 2.5 )
  {  
    pred1[i] <- 3;
  }
  
  if (pred1[i] < 4.5 && pred1[i] >= 3.5)
  {  
    pred1[i] <- 4;
  }
}

print("Area under roc Curve:")
multiclass.roc(as.numeric(given_Data$num), as.numeric(pred1))

