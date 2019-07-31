libraries <- function(){
  library(rpart)
  install.packages('rattle')
  install.packages('rpart.plot')
  install.packages('RColorBrewer')
  library(rattle)
  library (rpart.plot)
  library(RColorBrewer)
  library(class)
  install.packages("kknn")
  library(kknn)
}

#testData  <- read.csv("test.csv")
#trainData <- read.csv("train.csv")
allData = read.csv("train.csv")

testingStuff <- function(){
  #Merge Condition1+Condition2
  #Merge BldgType+HouseStyle
  #Merge OverallQual+OverallCond
  #Merge YearBuilt+YearRemodAdd
  #Merge Exterior1st+Exterior2nd
  #Merge MasVnrType+MasVnrArea
  #Merge ExterQual+ExterCond
  #Merge BsmtQual+BsmtCond
  #Merge BsmtFinType1+BsmtFinType2+TotalBsmtSF
  #Merge Heating+HeatingQC
  allData <- subset(allData, select=c(MSZoning,Neighborhood,Condition1,Condition2,BldgType,HouseStyle,OverallQual,
                                      OverallCond,YearBuilt,YearRemodAdd,Exterior1st,Exterior2nd,MasVnrType,MasVnrArea,
                                      ExterQual,ExterCond,BsmtQual,BsmtCond,BsmtFinType1,BsmtFinType2,TotalBsmtSF,
                                      Heating,HeatingQC))
  
  boxplot(allData$SalePrice~allData$Electrical)
  boxplot(allData$SalePrice~allData$Alley)
  
  
  
  par(mfrow=c(1,2))
  boxplot(allData$SalePrice[is.na(allData$BsmtFinType2)])
  boxplot(allData$SalePrice~allData$BsmtFinType2)
  sum(is.na(allData$Electrical)==TRUE)
  #
}

cleanData <- function(){
  allData = read.csv("train.csv")
  allData$ExterQual<- factor(allData$ExterQual, levels = c("Po","Fa","TA","Gd","Ex"), 
                              labels = c(1,2,3,4,5),
                              ordered = TRUE)
  allData$ExterCond<- factor(allData$ExterCond, levels = c("Po","Fa","TA","Gd","Ex"), 
                             labels = c(1,2,3,4,5),
                             ordered = TRUE)
  allData$BsmtQual<- factor(allData$BsmtQual, levels = c("Po","Fa","TA","Gd","Ex"), 
                             labels = c(1,2,3,4,5),
                             ordered = TRUE)
  allData$BsmtCond<- factor(allData$BsmtCond, levels = c("Po","Fa","TA","Gd","Ex"), 
                            labels = c(1,2,3,4,5),
                            ordered = TRUE)
  allData$BsmtExposure<- factor(allData$BsmtExposure, levels = c("No","Mn","Av","Gd"), 
                            labels = c(1,2,3,4),
                            ordered = TRUE)
  allData$BsmtFinType1<- factor(allData$BsmtFinType1, levels = c("Unf","Lwq","Rec","BLQ","ALQ","GLQ"), 
                                labels = c(1,2,3,4,5,6),
                                ordered = TRUE)
  allData$BsmtFinType2<- factor(allData$BsmtFinType2, levels = c("Unf","Lwq","Rec","BLQ","ALQ","GLQ"), 
                                labels = c(1,2,3,4,5,6),
                                ordered = TRUE)
  allData$HeatingQC<- factor(allData$HeatingQC, levels = c("Po","Fa","TA","Gd","Ex"), 
                                labels = c(1,2,3,4,5),
                                ordered = TRUE)
  allData$CentralAir<- factor(allData$CentralAir, levels = c("N","Y"), 
                             labels = c(0,1),
                             ordered = TRUE)
  allData$KitchenQual<- factor(allData$KitchenQual, levels = c("Po","Fa","TA","Gd","Ex"), 
                             labels = c(1,2,3,4,5),
                             ordered = TRUE)
  allData$Functional<- factor(allData$Functional, levels = c("Sal","Sev","Maj2","Maj1","Mod","Min2","Min1","Typ"), 
                               labels = c(1,2,3,4,5,6,7,8),
                               ordered = TRUE)
  allData$FireplaceQu<- factor(allData$FireplaceQu, levels = c("Po","Fa","TA","Gd","Ex"), 
                               labels = c(1,2,3,4,5),
                               ordered = TRUE)
  allData$GarageFinish<- factor(allData$GarageFinish, levels = c("Unf","RFn","Fin"), 
                               labels = c(1,2,3),
                               ordered = TRUE)
  allData$GarageQual<- factor(allData$GarageQual, levels = c("Po","Fa","TA","Gd","Ex"), 
                               labels = c(1,2,3,4,5),
                               ordered = TRUE)
  allData$GarageCond<- factor(allData$GarageCond, levels = c("Po","Fa","TA","Gd","Ex"), 
                              labels = c(1,2,3,4,5),
                              ordered = TRUE)
  allData$PavedDrive<- factor(allData$PavedDrive, levels = c("N","P","Y"), 
                                labels = c(1,2,3),
                                ordered = TRUE)
  allData$PoolQC<- factor(allData$PoolQC, levels = c("Po","Fa","TA","Gd","Ex"), 
                              labels = c(1,2,3,4,5),
                              ordered = TRUE)
  allData$Fence<- factor(allData$Fence, levels = c("MnWw","MnPrv","GdWo","GdPrv"), 
                          labels = c(1,2,3,4),
                          ordered = TRUE)
  boxplot(allData$SalePrice ~ allData$ExterQual)
  summary(lm(as.numeric(allData$ExterQual) ~ allData$SalePrice, data = allData))

}


ExportRPFVals <- function(){
  names <- c()
  rVals <- c()
  pVals <- c()
  fVals <- c()
  for(i in seq(2,length(allData)-1)){
    mdl <- lm(as.numeric(allData[,i]) ~ allData$SalePrice, data = allData)
    summ <- summary(mdl)
    names <- c(names, colnames(allData)[i])
    rVals <- c(rVals, as.numeric(summ$r.squared))
    pVals <- c(pVals, as.numeric(anova(mdl)$`Pr(>F)`[1]))
    fVals <- c(fVals, as.numeric(summ$fstatistic[1]))
  }
  
  d <- data.frame(names,rVals,pVals,fVals)
  write.csv(d, file="databaseRPFVals.csv")
}

Important <- function(){
  boxplot(trainData$SalePrice~trainData$OverallQual)    #18
  boxplot(trainData$SalePrice~trainData$Utilities)      #10
  boxplot(trainData$SalePrice~trainData$Neighborhood)   #13
  boxplot(trainData$SalePrice~trainData$YearBuilt)      #20
  boxplot(trainData$SalePrice~trainData$MSZoning)       #3
  boxplot(trainData$SalePrice~trainData$Condition1)     #14
  boxplot(trainData$SalePrice~trainData$Condition2)     #15
  boxplot(trainData$SalePrice~trainData$HouseStyle)     #17
  boxplot(trainData$SalePrice~trainData$OverallCond)    #19
  boxplot(trainData$SalePrice~trainData$ExterQual)      #28
  boxplot(trainData$SalePrice~trainData$BsmtQual)       #31
  boxplot(trainData$SalePrice~trainData$BsmtCond)       #32
  boxplot(trainData$SalePrice~trainData$BsmtExposure)   #33
  boxplot(trainData$SalePrice~trainData$TotalBsmtSF)
  boxplot(trainData$SalePrice~trainData$HeatingQC)
  boxplot(trainData$SalePrice~trainData$CentralAir)
  boxplot(trainData$SalePrice~trainData$Electrical)
  plot(trainData$SalePrice~trainData$GrLivArea)
  boxplot(trainData$SalePrice~trainData$FullBath)
  boxplot(trainData$SalePrice~trainData$BedroomAbvGr)
  boxplot(trainData$SalePrice~trainData$KitchenQual)
  boxplot(trainData$SalePrice~trainData$TotRmsAbvGrd)
  boxplot(trainData$SalePrice~trainData$Functional)
  boxplot(trainData$SalePrice~trainData$Fireplaces)
  boxplot(trainData$SalePrice~trainData$FireplaceQu)
  boxplot(trainData$SalePrice~trainData$GarageYrBlt)
  boxplot(trainData$SalePrice~trainData$PoolQC)
  boxplot(trainData$SalePrice~trainData$MiscFeature)
  
  hist(trainData$SalePrice)
}

firstPredictAttempt <- function(){
  allData = read.csv("train.csv")
  sampleRows <- sample(1:1423, 350)
  testingData <- allData[sampleRows, ]
  trainingData <- allData[-sampleRows, ]
  fit <- rpart(SalePrice~OverallQual+GrLivArea+ExterQual+KitchenQual+BsmtQual+GarageCars+GarageArea+TotalBsmtSF+X1stFlrSF+FullBath, data = trainingData)
  fancyRpartPlot(fit)
  prediction <- predict(fit,testingData, interval="confidence")
  accuarcy <- sum(round(prediction,digit = -3)==round(allData[1095:1460,81],digit=-3))/length(prediction)
  print(accuarcy)
  
  par(mfrow=c(2,1))
  plot(prediction)
}
#Spoiler alert: rerally bad
SecondPredictAttempt <- function(){
  
  fit <- rpart(SalePrice~OverallQual+GrLivArea+GarageCars+ExterQual+GarageArea+TotalBsmtSF+BsmtQual, data = trainData)
  fancyRpartPlot(fit)
  prediction <- predict(fit,testData, interval="confidence")
  accuarcy <- sum(round(prediction,digit = -3)==round(allData[1095:1460,81],digit=-3))/length(prediction)
  print(accuarcy)
  
  par(mfrow=c(2,1))
  plot(prediction)
}

#Feature Scalling function
FeatureScalling <- function(x) {
  x <- as.numeric(x)
  return((x-min(x))/(max(x)-min(x))) 
}
#Inverse of Feature Scalling function
InverseFeatureScalling <- function(x) {
  mx <- 755000
  mn <- 34900
  return(x*(mx-mn)+mn)
}

ThirdPredictAttempt <- function(){
  allData = read.csv("train.csv")
  allData <- subset(allData, select=c(SalePrice,OverallQual,GrLivArea,ExterQual,KitchenQual,BsmtQual,GarageCars,GarageArea,TotalBsmtSF,X1stFlrSF,FullBath))
  allData <- na.omit(allData)
  allData_Normalised <- as.data.frame(lapply(allData[,2:length(allData)], FeatureScalling))
  trainlen <- floor(length(allData[,1])*3/4)
  trainData <- allData_Normalised[1:trainlen,]
  testData <- allData_Normalised[trainlen:length(allData),]
  #Compute k-value to use with the classifier. Rule of thumb is square root of n of observations
  k_value <- floor(sqrt(length(trainData[,1])))
  #Now use KNN Algorithm in class package to classify data
  predictions <- knn(trainData, testData, allData[1:trainlen,1], k=k_value)
  #Reference data into dataframe
  reference <- allData[trainlen:length(allData),1]
  #Compare results
  #Performance shown as TP,FP/FN,TN
  #table(predictions,reference)
  accuracy <- sum(round(as.numeric(as.character(predictions)),digit = -4)==round(as.numeric(reference),digit=-4))/length(predictions)
  all.equal(as.numeric(as.character(predictions)),as.numeric(reference))
  print(accuracy)
  mdl <- lm(round(as.numeric(as.character(predictions)),digit = -4) ~ round(as.numeric(reference),digit=-4))
  summary(mdl)
  
#  accuracySum <- c()
  for(i in seq(1,length(predictions))){
    tempPred <- as.numeric(predictions[i])
    tempRef <- reference[i]
    if(tempPred>tempRef){
      tempPred <- tempRef-(tempPred-tempRef)
    }
    accuracySum <- c(accuracySum , (tempPred/tempRef))
  }
  accuracy <- mean(accuracySum)
}

FourthPredictAttempt <- function(){
  allData = read.csv("train.csv")
  allData <- subset(allData, select=c(SalePrice,OverallQual,GrLivArea,ExterQual,KitchenQual,BsmtQual,GarageCars,GarageArea,TotalBsmtSF,X1stFlrSF,FullBath))
  allData <- na.omit(allData)
  allData_Normalised <- cbind(allData[,1], as.data.frame(lapply(allData[,-1], FeatureScalling)))
  colnames(allData_Normalised) <- c("SalePrice", colnames(allData_Normalised[,-1]))
  #trainlen <- floor(length(allData[,1])*3/4)
  #trainData <- allData_Normalised[1:trainlen,]
  #testData <- allData_Normalised[trainlen:length(allData),]
  #Compute k-value to use with the classifier. Rule of thumb is square root of n of observations
  k_value <- floor(sqrt(length(allData_Normalised[,1])))
  #Now use KNN Algorithm in class package to classify data
  
  sampleRows <- sample(1:1423, 350)
  testingData <- allData_Normalised[sampleRows, ]
  trainingData <- allData_Normalised[-sampleRows, ]
  
  model <- train.kknn(formula = SalePrice~., data=trainingData, kmax = k_value, kernel = "optimal")
  predicty <- predict(model, testingData[, -1]) 
  
  #table(testingData[, 1], predicty)
  #accuracy <- sum(round(testingData[, 1],digit = -4)==round(predicty,digit = -4))/length(predicty)
  #accuracy
  
  
  mdl <- lm(predicty ~ testingData[, 1])
  plot(predicty,testingData[,1])
  abline(mdl)
  #par(mfrow=c(1,2))
  #plot(predicty)
  #plot(testingData[,1])
  x = c(0,0.3,0.8)
  y = c(0,0.3,0.8)
  par(new=T)
  plot(x,y,type="l")
  summary(mdl)
  all.equal(testingData[, 1],predicty)
  summary(predicty)
  summary(testingData[,1])
}

#BEST ATTEMPT KKNN (Weighted knn)
FiftPredictAttempt <- function(){
  #Import Data
  allData = read.csv("train.csv")
  #Normalise Data
  allData <- subset(allData, select=c(SalePrice,OverallQual,GrLivArea,ExterQual,KitchenQual,BsmtQual,GarageCars,GarageArea,TotalBsmtSF,X1stFlrSF,FullBath,TotRmsAbvGrd,YearBuilt))
  allData <- na.omit(allData)
  allData_Normalised <- cbind(allData[,1], as.data.frame(lapply(allData[,-1], FeatureScalling)))
  colnames(allData_Normalised) <- c("SalePrice", colnames(allData_Normalised[,-1]))
  #Compute k-value to use with the classifier. Rule of thumb is square root of n of observations
  k_value <- floor(sqrt(length(allData_Normalised[,1])))
  #Split data into testing and training data
  sampleRows <- sample(1:1423, 350)
  testingData <- allData_Normalised[sampleRows, ]
  trainingData <- allData_Normalised[-sampleRows, ]
  #Train weighted KNN Algorithm and test against testing results
  model <- train.kknn(formula = SalePrice~., data=trainingData, kmax = k_value, kernel = "optimal")
  predicty <- predict(model, testingData[, -1])
  #Linear model of prediction vs real result
  mdl <- lm(predicty ~ testingData[, 1])
  #Plot prediction against real result with a trendline and a y=x line for comparison
  plot(predicty,testingData[,1])
  abline(mdl)
  x = c(0,0.3,0.8)
  y = c(0,0.3,0.8)
  par(new=T)
  plot(x,y,type="l")
  #Print summary for model, prediction and real data. And find Mean Relative Difference between prediction and real data
  summary(mdl)
  all.equal(testingData[, 1],predicty)
  summary(predicty)
  summary(testingData[,1])
}



notUsed <- function(){
  #cut(trainData$SalePrice,breaks=c(0,50000,100000,200000,300000,400000,500000,600000,700000,Inf), labels = c("0-50k","50k-100k","100k-200k","200k-300k","300k-400k","400k-500k","500k-600k","600k-700k","700k-800k"))
  
  # trainData$SalePrice[trainData$SalePrice <= 50000] <- 50
  # trainData$SalePrice[trainData$SalePrice > 50000 & trainData$SalePrice <= 100000] <- 100
  # trainData$SalePrice[trainData$SalePrice > 100000 & trainData$SalePrice <= 200000] <- 200
  # trainData$SalePrice[trainData$SalePrice > 200000 & trainData$SalePrice <= 300000] <- 300
  # trainData$SalePrice[trainData$SalePrice > 300000 & trainData$SalePrice <= 400000] <- 400
  # trainData$SalePrice[trainData$SalePrice > 400000 & trainData$SalePrice <= 500000] <- 500
  # trainData$SalePrice[trainData$SalePrice > 500000 & trainData$SalePrice <= 600000] <- 600
  # trainData$SalePrice[trainData$SalePrice > 600000 & trainData$SalePrice <= 700000] <- 700
  # trainData$SalePrice[trainData$SalePrice > 700000 & trainData$SalePrice <= 800000] <- 800
  
  #plot(trainData$SalePrice[order(trainData$SalePrice)],trainData$Id, main = "", type="n")
  #lines(trainData$SalePrice[order(trainData$SalePrice)],trainData$Id)
}

hist(trainData$YearBuilt)


boxplot(trainData$SalePrice~trainData$SaleCondition)
