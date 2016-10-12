## ------------------------------------------------------------------------
library(MASS)
library(caret)

train_idx <- createDataPartition(Boston$crim, p=0.80, list=FALSE)

training <- Boston[train_idx,]
testing <- Boston[-train_idx,]

lmfit <- train(crim ~ ., data=training, method="lm", preProc=c("center", "scale"))

lmfit.test_predict <- predict(lmfit, newdata=testing)
testing_mse <- mean((testing$crim - lmfit.test_predict)^2)
testing_mse

lmfit.train_predict <- predict(lmfit, newdata=training)
training_mse <- mean((training$crim - lmfit.train_predict)^2)
training_mse


