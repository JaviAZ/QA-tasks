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
  indvChicks <- unique(ChickWeight$Chick)
  diffWeight1 = ChickWeight[ChickWeight$Time == 21 ,]
  diffWeight2 = ChickWeight[ChickWeight$Time == 0 ,]
  
}
