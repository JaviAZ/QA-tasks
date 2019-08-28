#' Install and import necessary libraries and import dataset from SQL
#'
#' This function contains a list of necessary libraries for the application to run
#' it checks if the libraries are already installed, if they arents it proceeds to installation.
#' Then it imports each library, connects to a local database to extract the data for the data set
#' and returns the dataset with the train data.
#' @param NA
#' no parameters needed
#' @return The training dataset
#' @examples
#' trainData <- importLibrariesAndDB()
#' @export
importLibrariesAndDB <- function (){
  required_packages <- c('rpart.plot','RColorBrewer','kknn','RMariaDB','shiny','plyr') #Every package your script needs
  new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])] #Get all the ones not already installed
  if(length(new_packages >= 1)){
    install.packages(new_packages) #Install all packages not already installed
  }
  library(rpart)
  library (rpart.plot)
  library(RColorBrewer)
  library(class)
  library(kknn)
  library(RMariaDB)
  library(shiny)
  library (plyr)
  #Variables for connection to database
  dbName <- "predictamentdb"
  tableName <- "train_data"
  localusername <- "root"
  localuserpassword <- "root" #Change to "password" for GCP
  #Connecting to the database to get the data frame
  PredictamentDB <- dbConnect(RMariaDB::MariaDB(), user=localusername, password=localuserpassword, dbname=dbName, host='localhost')
  dbListTables(PredictamentDB)
  query <- paste("SELECT * FROM ", tableName,";", sep="")
  allData <- dbFetch(dbSendQuery(PredictamentDB, query))
  dbDisconnect(PredictamentDB)
  return(allData)
}

#To stop program from printing numbers in scientific notation
options(scipen=999)

#' Clean data in certain columns
#'
#' This function will do slightly different things depending on the parameters, if it is called with the
#' UI input, it will normalise the "user friendly" words into string factors and then change this factors
#' into numeric values. Otherwise, it will be called with the whole train data which will normalise the
#' string factors into numeric values.
#' @param dataset
#' Train data or GUI input
#' @return Cleaned dataset
#' @examples
#' trainClean <- cleanData(trainData)
#' @export
cleanData <- function(dat){
  if(length(dat) == 81){
    dat$CentralAir<- factor(dat$CentralAir, levels = c("N","Y"), labels = c(0,1), ordered = TRUE)
    dat$PavedDrive<- factor(dat$PavedDrive, levels = c("N","P","Y"), labels = c(1,2,3), ordered = TRUE)
    dat$ExterCond<- factor(dat$ExterCond, levels = c("Po","Fa","TA","Gd","Ex"), labels = c(1,2,3,4,5), ordered = TRUE)
    dat$BsmtCond<- factor(dat$BsmtCond, levels = c("Po","Fa","TA","Gd","Ex"), labels = c(1,2,3,4,5), ordered = TRUE)
    dat$HeatingQC<- factor(dat$HeatingQC, levels = c("Po","Fa","TA","Gd","Ex"), labels = c(1,2,3,4,5), ordered = TRUE)
    dat$GarageQual<- factor(dat$GarageQual, levels = c("Po","Fa","TA","Gd","Ex"), labels = c(1,2,3,4,5), ordered = TRUE)
    dat$GarageCond<- factor(dat$GarageCond, levels = c("Po","Fa","TA","Gd","Ex"), labels = c(1,2,3,4,5), ordered = TRUE)
    dat$FireplaceQu<- factor(dat$FireplaceQu, levels = c("Po","Fa","TA","Gd","Ex"), labels = c(1,2,3,4,5), ordered = TRUE)
    dat$PoolQC<- factor(dat$PoolQC, levels = c("Po","Fa","TA","Gd","Ex"), labels = c(1,2,3,4,5), ordered = TRUE)
    dat$BsmtFinType1<- factor(dat$BsmtFinType1, levels = c("Unf","Lwq","Rec","BLQ","ALQ","GLQ"), labels = c(1,2,3,4,5,6), ordered = TRUE)
    dat$BsmtFinType2<- factor(dat$BsmtFinType2, levels = c("Unf","Lwq","Rec","BLQ","ALQ","GLQ"), labels = c(1,2,3,4,5,6), ordered = TRUE)
    dat$BsmtExposure<- factor(dat$BsmtExposure, levels = c("No","Mn","Av","Gd"), labels = c(1,2,3,4), ordered = TRUE)
    dat$GarageFinish<- factor(dat$GarageFinish, levels = c("Unf","RFn","Fin"), labels = c(1,2,3), ordered = TRUE)
    dat$Fence<- factor(dat$Fence, levels = c("MnWw","MnPrv","GdWo","GdPrv"), labels = c(1,2,3,4), ordered = TRUE)
    dat$Functional<- factor(dat$Functional, levels = c("Sal","Sev","Maj2","Maj1","Mod","Min2","Min1","Typ"), labels = c(1,2,3,4,5,6,7,8), ordered = TRUE)
  }else{
    if(dat$ExterQual == "Excellent") dat$ExterQual <- "Ex"
    if(dat$ExterQual == "Good") dat$ExterQual <- "Gd"
    if(dat$ExterQual == "Average") dat$ExterQual <- "TA"
    if(dat$ExterQual == "Fair") dat$ExterQual <- "Fa"
    if(dat$ExterQual == "Poor") dat$ExterQual <- "Po"
    if(dat$BsmtQual == "Excellent") dat$BsmtQual <- "Ex"
    if(dat$BsmtQual == "Good") dat$BsmtQual <- "Gd"
    if(dat$BsmtQual == "Typical") dat$BsmtQual <- "TA"
    if(dat$BsmtQual == "Fair") dat$BsmtQual <- "Fa"
    if(dat$BsmtQual == "Poor") dat$BsmtQual <- "Po"
    if(dat$BsmtQual == "No Basement") dat$BsmtQual <- "TA"
    if(dat$KitchenQual == "Excellent") dat$KitchenQual <- "Ex"
    if(dat$KitchenQual == "Good") dat$KitchenQual <- "Gd"
    if(dat$KitchenQual == "Average") dat$KitchenQual <- "TA"
    if(dat$KitchenQual == "Fair") dat$KitchenQual <- "Fa"
    if(dat$KitchenQual == "Poor") dat$KitchenQual <- "Po"
  }
  dat$ExterQual<- factor(dat$ExterQual, levels = c("Po","Fa","TA","Gd","Ex"), labels = c(1,2,3,4,5), ordered = TRUE)
  dat$BsmtQual<- factor(dat$BsmtQual, levels = c("Po","Fa","TA","Gd","Ex"), labels = c(1,2,3,4,5), ordered = TRUE)
  dat$KitchenQual<- factor(dat$KitchenQual, levels = c("Po","Fa","TA","Gd","Ex"), labels = c(1,2,3,4,5), ordered = TRUE)


  return(dat)
}

#' Export R values, P values and F values for each column into a CSV
#'
#' This functionCalculates R values, P values and F values for each column and export these into a CSV
#' to be able to compare which columns affect the sale price the most to then be used in the prediction
#' @param NA
#' no parameters needed
#' @return No value returned
#' @examples
#' ExportRPFVals()
#' @export
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

#' Feature Scalling function
#'
#' This function takes in string or numeric values and formats them into a number between 0 and 1
#' @param DatasetColumn
#' Takes in a column using lapply
#' @return List of numeric values between 0-1
#' @example
#' lapply(allData[,2:length(allData)], FeatureScalling)
#' @export
FeatureScalling <- function(x) {
  x <- as.numeric(x)
  return((x-min(x))/(max(x)-min(x)))
}

#' Normalise train dataset to use only relevant columns and rows
#'
#' This function creates a subset of the data frame with pre-selected columns and removes rows with
#' NA values
#' @param Dataset
#' Takes in the train dataset
#' @return Normalised dataset
#' @examples
#' dataNormalised <- normaliseData(trainData)
#' @export
normaliseData <- function(dat){
  #Normalise Data
  Dat_Normalised <- subset(dat, select=c(SalePrice,OverallQual,GrLivArea,ExterQual,KitchenQual,BsmtQual,GarageCars,TotalBsmtSF,X1stFlrSF,FullBath,TotRmsAbvGrd,YearBuilt))
  Dat_Normalised <- na.omit(Dat_Normalised)
  #Dat_Normalised <- cbind(Dat_Normalised[,1], as.data.frame(lapply(Dat_Normalised[,-1], FeatureScalling)))
  #colnames(Dat_Normalised) <- c("SalePrice", colnames(Dat_Normalised[,-1]))
  return(Dat_Normalised)
}

#' Weighted K-Nearest Neighbor model
#'
#' This function calculates a k value and trains a weighted knn model with the train data
#' @param NA
#' no parameters needed
#' @return Weighted K-Nearest Neighbor model
#' @examples
#' knnModel <- weightedKNNModel()
#' @export
weightedKNNModel <- function(){
  #Compute k-value to use with the classifier. Rule of thumb is square root of n of observations
  k_value <- floor(sqrt(length(allData_Normalised[,1])))
  trainingData <- allData_Normalised[,]
  model <- train.kknn(formula = SalePrice~., data=trainingData, kmax = k_value, kernel = "optimal")
  return(model)
}

#' Weighted K-Nearest Neighbor prediction
#'
#' This function calculates a sale price prediction based on a previously trained K-Nearest Neighbormodel and
#' the user input
#' @param list
#' List with user inputs from UI
#' @return Predicted sale price rounded to the nearest thousand
#' @examples
#' weightedKNNPredict(input)
#' @export
weightedKNNPredict <- function(input){
  #inp <- list("OverallQual"=6,"GrLivArea"=1800,"ExterQual"="Excellent","KitchenQual"="Good","BsmtQual"="Good","GarageCars"=2,
  #            "TotalBsmtSF"=800,"X1stFlrSF"=800,"FullBath"=2,"TotRmsAbvGrd"=5,"YearBuilt"=1997)
  inp <- list("OverallQual"=input$OverallQual,"GrLivArea"=as.numeric(input$GrLivArea),"ExterQual"=input$ExterQual,
              "KitchenQual"=input$KitchenQual,"BsmtQual"=input$BsmtQual,"GarageCars"=as.numeric(input$GarageCars),
              "TotalBsmtSF"=as.numeric(input$TotalBsmtSF),"X1stFlrSF"=as.numeric(input$X1stFlrSF),
              "FullBath"=as.numeric(input$FullBath),"TotRmsAbvGrd"=as.numeric(input$TotRmsAbvGrd),
              "YearBuilt"=as.numeric(input$YearBuilt))

  #Clean input
  testingData <- data.frame(inp)
  testingData <- cleanData(testingData)
  #Train weighted KNN Algorithm and test against testing results
  predicty <- predict(knnModel, testingData)
  return(round(predicty,-3))
}

#Design UI
ui <- shiny::fluidPage(
  shiny::titlePanel("Iowa House Prices Prediction System"),
  shiny::sidebarPanel(
    shiny::fluidRow(
      shiny::column(2,
                    shiny::sliderInput(inputId = "OverallQual", label = "Overal Quality",min = 0, max = 10, value = 5)),
      shiny:: column(2,
                     shiny::selectInput(inputId = "ExterQual", label = "Exterior Quality", choices = c("Excellent","Good","Average","Fair","Poor"))),
      shiny::column(3,
                    shiny::selectInput(inputId = "KitchenQual", label = "Kitchen Quality", choices = c("Excellent","Good","Average","Fair","Poor"))),
      shiny::column(3,
                    shiny::selectInput(inputId = "BsmtQual", label = "Basement Quality", choices = c("Excellent","Good","Typical","Fair","Poor","No Basement")))
    ),
    shiny::fluidRow(
      shiny::column(3,
                    shiny::textInput(inputId = "GrLivArea", label = "Above ground living area square feet", value = 2423)),
      shiny::column(3,
                    shiny::textInput(inputId = "TotalBsmtSF", label = "Basement area square feet", value = 1234)),
      shiny::column(3,
                    shiny::textInput(inputId = "X1stFlrSF", label = "1st floor area square feet", value = 1342)),
      shiny::column(3,
                    shiny::textInput(inputId = "YearBuilt", label = "Original construction date", value = 1997))
    ),
    shiny::fluidRow(
      shiny::column(3,
                    shiny::sliderInput(inputId = "GarageCars", label = "Garage car capacity",min = 0, max = 10, value = 2)),
      shiny::column(3,
                    shiny::sliderInput(inputId = "FullBath", label = "Bathrooms above ground",min = 0, max = 10, value = 2)),
      shiny::column(3,
                    shiny::sliderInput(inputId = "TotRmsAbvGrd", label = "Rooms above ground (not including bathrooms",min = 1, max = 20, value = 0))
    ),
    width = 20),
  shiny::mainPanel(
    shiny::textOutput(outputId = "Predicted"),
    shiny::submitButton("Check price")
  )
)

server <- function(input,output){
  output$Predicted <- renderText({
    paste("Predicted Value of \U0024", weightedKNNPredict(input))
  })
}

allData <- importLibrariesAndDB()
allData <- cleanData(allData)
allData_Normalised <- normaliseData(allData)
knnModel <- weightedKNNModel()
shinyApp(ui = ui, server = server)
