library(class)
library(ggplot2)

RawBCD <- read.table("BreastCancerData.data", sep=",")
colnames(RawBCD) <- c("id", "result", "meanRadius", "meanTexture", "meanPerimeter","meanArea",
                      "meanSmoothness","meanCompactness","meanConcativity","meanConcavePoints",
                      "meanSymmetry","meanFractalDimension","seRadius","seTexture","sePerimeter",
                      "seArea","seSmoothness","seCompactness","seConcativity",
                      "seConcavePoints","seSymmetry","seFractalDimension","worstRadius",
                      "worstTexture","meanPerimeter","worstArea","worstSmoothness","worstCompactness",
                      "worstConcativity","worstConcavePoints","worstSymmetry","worstFractalDimension")

#ID is not relevant so get rid off it
BCD_Noid <- RawBCD[,-1]

#Results are not relevant
BCD_NoResults <- BCD_Noid[,-1]

#Feature Scalling function
FeatureScalling <- function(x) { ((x-min(x))/(max(x)-min(x))) }

#Data normalised according to function
BCD_Normalised <- as.data.frame(lapply(BCD_NoResults, FeatureScalling))

#Split data for training and testing doing 75:25 (ish) split
BCD_Train <- BCD_Normalised[1:451,]
BCD_Test  <- BCD_Normalised[452:569,]

#Compute k-value to use with the classifier. Rule of thumb is square root of n of observations
k_value <- floor(sqrt(length(BCD_Train[,1])))

#Instantiate variables for loop
bestErr <- 200
bestK <- -1
bestTable <- c()

#Reference data into dataframe
BCD_Reference <- BCD_Noid[452:569,1]

#Loop iterating through all possible k odd values
for (k in seq(1,451, by = 2)){
  #Now use KNN Algorithm in class package to classify data
  BCD_Predictions <- knn(BCD_Train, BCD_Test, BCD_Noid[1:451,1], k=k)
  
  #Compare results
  #Performance shown as TP,FP/FN,TN
  tempTable <- table(BCD_Predictions,BCD_Reference)
  tempErr <- tempTable[1,2] + tempTable[2,1]
  if(tempErr < bestErr){
    print(bestErr)
    bestErr <- tempErr
    bestTable = tempTable
    bestK <- k
  }
}
#Print best K value and the prediction table it makes
print(bestK)
print(bestTable)
