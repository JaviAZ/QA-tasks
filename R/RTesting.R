myvar <- 3
myvar
str(myvar)
myBool = TRUE
vec = range(1:100)
vec
strings <- c("hello", "world", "today")
numbers <- c(1,2,3)
bools<- c(TRUE, FALSE, FALSE)
myDataFrame <- data.frame(strings, numbers, bools)
myDataFrame
names(myDataFrame) <- c("Strings", "Numbers", "Booleans")
myDataFrame
myDataFrame[myDataFrame$Booleans == FALSE,]
summary(myDataFrame)
square <- function(x){
  squared <- x*x
  return (squared)
}
square(12)
par(2,2)
plot(iris$Petal.Length, iris$Sepal.Length, main = "title", pch = c(2,3,4)[iris$Species], cex = 2, col = c("red", "blue", "black"), legend = levels(iris$Species))
hist(iris$Sepal.Length)
plot(iris$Petal.Width, iris$Sepal.Width)
boxplot(iris$Petal.Length)