View(diamonds)

train <- diamonds[1:30000,]
test <- diamonds[31000:53940,-1]
test_carat <- unlist(diamonds[31000:53940,1])

model <- lm(carat ~., data=diamonds)

summary(model)

predictions <- predict(model, test)

plot(sample(test_carat, 100))
plot(sample(predictions,100))

rmse <- function(predicted,test_carat){
  error = predicted - test_carat
  return(sqrt(mean(error*error)))
}

rmse <- rmse(predictions, test_carat)

mean(test_carat)

