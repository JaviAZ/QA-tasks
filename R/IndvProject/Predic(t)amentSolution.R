importLibraries <- function (){
  install.packages('rattle')
  install.packages('rpart.plot')
  install.packages('RColorBrewer')
  install.packages('kknn')
  library(rpart)
  library(rattle)
  library (rpart.plot)
  library(RColorBrewer)
  library(class)
  library(kknn)
}
options(scipen=999)

allData = read.csv("train.csv")

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

#Feature Scalling function
FeatureScalling <- function(x) {
  x <- as.numeric(x)
  return((x-min(x))/(max(x)-min(x))) 
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
  print(summary(mdl))
  print(all.equal(testingData[, 1],predicty))
  print(summary(predicty))
  print(summary(testingData[,1]))
}

cleanData()
ExportRPFVals()
FiftPredictAttempt()
