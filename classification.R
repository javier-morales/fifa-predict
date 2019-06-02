
source('preprocessing.R')

library(e1071)


## Utils 

conf.matrix <- function(mod, data, target) {
  pred <- predict(mod, data)
  cm <- table(pred, target)
  return(list(Confusion.matrix = cm, accuracy = sum(diag(cm))/sum(cm)))
}


## -- SVM (radial) --

library(caret)
library(kernlab)

trC <- trainControl(method = "repeatedcv", number = 3, repeats = 3, allowParallel = T)

model3x3CV.sigma <- train(train.s, train.data$Role, method = "svmRadial",
                    trControl = trC, tuneGrid = expand.grid(.sigma = c(.01, .015, 0.2), .C = 1))

(bestSigma <- model3x3CV.sigma$bestTune$sigma)

model3x3CV.C <- train(train.s, train.data$Role, method = "svmRadial",
                    trControl = trC, tuneGrid = expand.grid(.sigma = bestSigma, .C = 2^seq(-2, 2)))

(bestCost <- model3x3CV.C$bestTune$C)

mod.svmRadial <- model3x3CV.C$finalModel

conf.matrix(mod.svmRadial, train.s, train.data$Role)
conf.matrix(mod.svmRadial, test.s, test.data$Role)



# -- SVM (poly) --

model3x3CV.poly <- train(train.s, train.data$Role, method = "svmPoly",
                          trControl = trC, tuneGrid = expand.grid(.degree = 2, 
                                                                  .scale = 1, 
                                                                  .C = 0))


mod.svmPoly <- model3x3CV.poly$finalModel

conf.matrix(mod.svmRadial, train.s, train.data$Role)
conf.matrix(mod.svmRadial, test.s, test.data$Role)



# -- kNN --

mod.knn <- train(train.data, train.data$Role, method = "knn",
                 trControl = trC, tuneGrid = expand.grid(.k = 1:8))

