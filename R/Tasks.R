condi <- function(x, y){
  if(x == 0){
    return (y)
  }
  if(y == 0){
    return (x)
  }
  if(y != 0 && x != 0){
    return (x+y)
  }
}
recur <- function(counter){
  print(condi(3,counter))
  counter = counter + 1
  if(counter < 10){
    recur(counter)
  }
}

blackJack <- function(x,y){
  if(x>=y && x<=21 || x<y && y>21 && x<=21){
    return (x)
  }else if(x<y && y<=21 || x>y && x>21 && y<=21){
    return (y)
  }else{
    return (0)
  }
}

uniqueSum <- function(x,y,z){
  total = 0
  if (x!=y && x!=z){
    total = total + x
  }
  if (y!=x && y!=z){
    total= total + y
  }
  if (z!=x && z!=y){
    total= total + z
 }
  return (total)
}

tooHot <- function(temp, isSummer){
  if(temp>=60 && temp<=90 || isSummer && temp>=60 && temp<=100){
    return (TRUE)
  }else{
    return (FALSE)
  }
}

leapYear <- function(year){
  if(year%%4==0){
    if(year%%100==0){
      if(year%%400==0){
        return (T)
      }else{
        return (F)
      }
    }else{
      return (T)
    }
  }else{
    return (F)
  }
}

workingWithFiles <- function(){
  evens = c(2,4,6,8)
  write.csv(evens, "evenNums.csv")
  odds <- read.csv("evenNums.csv")[,2]+1
  write.csv(odds,"oddNums.csv")
}

plotting <- function(){
  par(mfrow=c(2,1))
  boxplot(iris$Sepal.Width~iris$Species)
  plot(iris$Petal.Length, iris$Petal.Width, pch = c(15,16,17)[iris$Species], col = c("black", "red", "blue")[iris$Species])
  legend("topleft", legend = unique(iris$Species), pch = c(15,16,17), col = c("black", "red", "blue"))
}

mean_checker <- function(v1,v2){
  m1 = mean(v1)
  m2 = mean(v2)
  if(m1 > m2){
    return (1)
  }else if(m1 < m2){
    return (2)
  }else{
    return (3)
  }
}
co2Data <- function(){
  typeof(CO2["Plant"])
  meanUpTake <- mean(CO2$uptake)
  boxplot(CO2$uptake~CO2$Type)
  quebec_CO2 <- CO2[CO2["Type"] == "Quebec",]
  mississippi_CO2 <- CO2[CO2["Type"] == "Mississippi",]
  c("Quebec","Mississippi", "Both are equal")[mean_checker(quebec_CO2$uptake,mississippi_CO2$uptake)]
}

orchardSpraysData <- function(){
  max_decrease = OrchardSprays[OrchardSprays["decrease"] == max(OrchardSprays$decrease)][4]
  boxplot(OrchardSprays$decrease~OrchardSprays$treatment)
}

chickWeightData <- function(){
  chickAt21 <- ChickWeight[ChickWeight$Time == 21 ,]
  chickAt0 <- ChickWeight[ChickWeight$Time == 0 ,]
  chickAt0 <- chickAt0[-c(8,15,16,18,44),]
  totalDiff <- chickAt21$weight - chickAt0$weight
  diffWeight <- data.frame(totalDiff,chickAt21$Chick,chickAt21$Diet)
  boxplot(diffWeight$totalDiff~diffWeight$chickAt21.Diet)
  plot(ChickWeight$Time~ChickWeight$weight, pch = c(15,16,17,18)[ChickWeight$Diet], col=c("black","red","blue","green")[ChickWeight$Diet], type = "n")
  abline(lm(Time~weight, data = ChickWeight[ChickWeight$Diet == 1,]), col = "black", lwd = 4)
  abline(lm(Time~weight, data = ChickWeight[ChickWeight$Diet == 2,]), col = "red", lwd = 4)
  abline(lm(Time~weight, data = ChickWeight[ChickWeight$Diet == 3,]), col = "blue", lwd = 4)
  abline(lm(Time~weight, data = ChickWeight[ChickWeight$Diet == 4,]), col = "green", lwd = 4)
  legend("bottomright", legend = unique(ChickWeight$Diet),pch = "-", col=c("black","red","blue","green"))
}

primeNumbers <- function(){
  A <- 2
  n <- 3000000
  for (i in 2:n){
    prime = TRUE
    for(j in A){
      if(i%%j == 0){
        prime = FALSE
        break
      }
      if(j > sqrt(n)){
        break
      }
    }
    if(prime){
      A <- c(A, i)
    }
  }
  print(length(A))
}

salaryPredsTrain <- function(){
  #http://cseweb.ucsd.edu/classes/sp15/cse190-c/reports/sp15/048.pdf
  #https://www.rdocumentation.org/packages/arules/versions/1.6-3/topics/Adult
  train <- read.csv("censusData_train.csv", header = FALSE)
  colnames(train) <- c("Age", "Workclass", "FinalWeight", "Education", "EducationNum", "MaritalStatus", "Occupation", "Relationship", "Race", "Sex","CapitalGain", "CapitalLoss", "HoursPerWeek","NativeCountry", "Salary")
  
 
  train$Workclass <-  factor(train$Workclass, levels = c(unique(as.character(train$Workclass)),"Unemployed"))
  train$Workclass[train$Workclass %in% c("?", "Never-worked", "Without-pay")] <-"Unemployed"
  train$Workclass <-  factor(train$Workclass, levels = c(unique(as.character(train$Workclass))))
  ages <- cut(train$Age, breaks = c(0,20,29, 36, 49,59,Inf), labels = c("17-20","21-29","30-36","37-49","50-59","60-90"))
  
  #train$Age[train$Age > 0  && train$Age < 20] <- 17.20
  #train$Age[as.numeric(train$Age) > 19 && as.numeric(train$Age) < 30] <- 21.29
  #train$Age[as.numeric(train$Age) > 29 && as.numeric(train$Age) < 37] <- 30.36
  #train$Age[as.numeric(train$Age) > 36 && as.numeric(train$Age) < 50] <- 37.49
  #train$Age[as.numeric(train$Age) > 49 && as.numeric(train$Age) < 60] <- 50.59
  #train$Age[as.numeric(train$Age) > 59 && as.numeric(train$Age) < 99] <- 60.90
  #train$Age <- factor(train$Age, levels = c(unique(as.numeric(train$Age))))
  
  
  #Pre-highschool("Preschool","1st-4th", "5th-6th", "7th-8th", "9th", "10th", "11th")<highschool("12th", "HS-grad")<Prof-school("Prof-school","Assoc-acdm","Assoc-voc")<Grad("Some-college","Bachelors")<Masters<Doctorate
  train$Education <-  factor(train$Education, levels = c(unique(as.character(train$Education)),1,2,3,4,5,6,7))
  train$Education[train$Education %in% c("Preschool","1st-4th", "5th-6th", "7th-8th", "9th", "10th", "11th")] <- 1
  train$Education[train$Education %in% c("12th", "HS-grad")] <- 2
  train$Education[train$Education %in% c("Prof-school")] <- 3
  train$Education[train$Education %in% c("Assoc-acdm","Assoc-voc")] <- 4
  train$Education[train$Education %in% c("Some-college","Bachelors")] <- 5
  train$Education[train$Education %in% c("Masters")] <- 6
  train$Education[train$Education %in% c("Doctorate")] <- 7
  train$Education <-  factor(train$Education, levels = c(unique(as.character(train$Education))))
  educSorted <- train
  educSorted$Education <- factor(train$Education, levels = c(unique(as.numeric(train$Education))), ordered = TRUE)
 
  
  par(mfrow = c(2,2))
  plot(educSorted$Salary~educSorted$Education, main = "Salary VS Education", ylab = "Salary", xlab = "Education")
  plot(train$Salary~ages, main = "Salary VS Ages", ylab = "Salary", xlab = "Ages")
  plot(train$Salary~train$Sex, main = "Salary VS Sex", ylab = "Salary", xlab = "Sex")
  sexVSsalary  <- prop.table(table(train$Sex,train$Salary),1)
  agesVSsalary <- prop.table(table(ages,train$Salary),1)
  educVSsalary <- prop.table(table(educSorted$Education,educSorted$Salary),1)
  
  
  sexAbove50 <- c(sexVSsalary [,2])
  agesAbove50<- c(agesVSsalary[,2])
  educAbove50<- c(educVSsalary[,2])
  
  return (train)
 
}
salaryPredsTest<- function(train){
  test <- read.csv("censusData_trainCopy.csv", header = FALSE)
  colnames(test) <- c("Age", "Workclass", "FinalWeight", "Education", "EducationNum", "MaritalStatus", "Occupation", "Relationship", "Race", "Sex","CapitalGain", "CapitalLoss", "HoursPerWeek","NativeCountry")
  
  test$Workclass <-  factor(test$Workclass, levels = c(unique(as.character(test$Workclass)),"Unemployed"))
  test$Workclass[test$Workclass %in% c("?", "Never-worked", "Without-pay")] <-"Unemployed"
  test$Workclass <-  factor(test$Workclass, levels = c(unique(as.character(test$Workclass))))
  
  test$Education <- factor(test$Education, levels = c(unique(as.character(test$Education)),1,2,3,4,5,6,7))
  test$Education[test$Education %in% c("Preschool","1st-4th", "5th-6th", "7th-8th", "9th", "10th", "11th")] <- 1
  test$Education[test$Education %in% c("12th", "HS-grad")] <- 2
  test$Education[test$Education %in% c("Prof-school")] <- 3
  test$Education[test$Education %in% c("Assoc-acdm","Assoc-voc")] <- 4
  test$Education[test$Education %in% c("Some-college","Bachelors")] <- 5
  test$Education[test$Education %in% c("Masters")] <- 6
  test$Education[test$Education %in% c("Doctorate")] <- 7
  test$Education <- factor(test$Education, levels = c(unique(as.character(test$Education))))
  
  fit <- rpart(Salary~Age+Education+Workclass+CapitalGain+CapitalLoss+HoursPerWeek, data = train)
  par(mfrow = c(1,1))
  fancyRpartPlot(fit)
  prediction <- predict(fit,test,type='class')
  accuarcy <- sum(prediction==train$Salary)/length(prediction)
  print(accuarcy)
}








