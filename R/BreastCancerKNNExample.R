library(class)
library(ggplot2)

RawBCD <- read.table("BreastCancerData.data", sep=",")
colnames(RawBCD) <- c("id", "result", "meanRadius", "meanTexture", "meanPerimeter","meanArea",
                      "meanSmoothness","meanCompactness","meanConcativity","meanConcavePoints",
                      "meanSymmetry","meanFractalDimension","seRadius","seTexture","meanPerimeter",
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
BCD_Train <- BCD_Normalised[1:450,]
BCD_Test  <- BCD_Normalised[451:569,]

#Compute k-value to use with the classifier. Rule of thumb is square root of n of observations
k_value <- floor(sqrt(length(BCD_Train[,1])))

#Now use KNN Algorithm in class package to classify data
BCD_Predictions <- knn(BCD_Train, BCD_Test, BCD_Noid[1:450,1], k=k_value)


#Reference data into dataframe
BCD_Reference <- BCD_Noid[451:569,1]

#Compare results
#Performance shown as TP,FP/FN,TN
table(BCD_Predictions,BCD_Reference)







