importLibrariesAndDB <- function (){
  #setwd("C:/Users/Admin/Desktop/QAExercises/R/IndvProject")
  #install.packages('rattle')
  #install.packages('rpart.plot')
  #install.packages('RColorBrewer')
  #install.packages('kknn')
  #install.packages("RMariaDB")
  #install.packages("shiny")
  library(rpart)
  library(rattle)
  library (rpart.plot)
  library(RColorBrewer)
  library(class)
  library(kknn)
  library(RMariaDB)
  library(shiny)
  localuserpassword <- "root"
  PredictamentDB <- dbConnect(RMariaDB::MariaDB(), user='root', password=localuserpassword, dbname='predictamentdb', host='localhost')
  dbListTables(PredictamentDB)
  query <- "SELECT * FROM train_data;"
  allData <- dbFetch(dbSendQuery(PredictamentDB, query))
  dbDisconnect(PredictamentDB)
  return(allData)
}

options(scipen=999)

cleanData <- function(){
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
  return(allData)
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
  #Normalise Data
  allData <- subset(allData, select=c(SalePrice,OverallQual,GrLivArea,ExterQual,KitchenQual,BsmtQual,GarageCars,TotalBsmtSF,X1stFlrSF,FullBath,TotRmsAbvGrd,YearBuilt))
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

allData <- importLibrariesAndDB()
allData <- cleanData()
FiftPredictAttempt()

#Design UI
ui <- fluidPage(
  titlePanel("Hello Shiny!"),
  fluidRow(
    column(2,
      selectInput(inputId = "OverallQual", label = "Overal Quality", choices = c("Excellent","Good","Average","Fair","Poor"))),
    column(2,
      selectInput(inputId = "ExterQual", label = "Exterior Quality", choices = c("Excellent","Good","Average","Fair","Poor"))),
    column(3,
      selectInput(inputId = "KitchenQual", label = "Kitchen Quality", choices = c("Excellent","Good","Average","Fair","Poor"))),
      selectInput(inputId = "BsmtQual", label = "Basement Quality", choices = c("Excellent","Good","Typical","Average","Fair","Poor","No Basement")),
      textInput(inputId = "GrLivArea", label = "Above ground living area square feet", value = 0),
      textInput(inputId = "TotalBsmtSF", label = "Basement area square feet", value = 0),
      textInput(inputId = "X1stFlrSF", label = "1st floor area square feet", value = 0),
      textInput(inputId = "YearBuilt", label = "Original construction date", value = 0),
      sliderInput(inputId = "GarageCars", label = "Garage car capacity",min = 0, max = 10, value = 0),
      sliderInput(inputId = "FullBath", label = "Bathrooms above ground",min = 0, max = 10, value = 0),
      sliderInput(inputId = "TotRmsAbvGrd", label = "Rooms above ground (not including bathrooms",min = 0, max = 20, value = 0)
    ),
    mainPanel(
      textOutput(outputId = "Predicted"),
      plotOutput(outputId =  "Ploty")
    )
)

cleverCleverLogic <- function(input){
  bedsValue <- input$beds*100000
  toiletsValue <- input$toilets*50000
  storiesValue <- (as.numeric(input$stories)-1)*50000
  value <- (bedsValue + toiletsValue + storiesValue)
  return(value)
}

server <- function(input,output){
  output$Predicted <- renderText({
    paste("Predicted Value of \U00A3", 300)
  })
}

shinyApp(ui = ui, server = server)
