
source('preprocessing.R')

library(e1071)


## Utils 

conf.matrix <- function(mod, data, target, type) {
  pred <- if (missing(type)) predict(mod, data)
          else predict(mod, data, type)
  cm <- table(pred, target)
  return(list(Confusion.matrix = cm, accuracy = sum(diag(cm))/sum(cm)))
}



## -- LDA --

set.seed(270217)
mod.lda <- lda(train.data$Role ~ ., data = as.data.frame(train.m))

p.tr <- predict(mod.lda, newdata = train.data)
(cm <- table(train.data$Role, p.tr$class))
sum(diag(cm)) / sum(cm)

p.te <- predict(mod.lda, newdata = test.data)
(cm <- table(test.data$Role, p.te$class))
sum(diag(cm)) / sum(cm)


## -- SVM (radial) --

library(caret)
library(kernlab)

trC <- trainControl(method = "repeatedcv", number = 3, repeats = 3, allowParallel = T)

set.seed(270217)
model3x3CV.sigma <- train(train.s, train.data$Role, method = "svmRadial",
                    trControl = trC, tuneGrid = expand.grid(.sigma = c(.01, .015, 0.2), .C = 1))

(bestSigma <- model3x3CV.sigma$bestTune$sigma)

set.seed(270217)
model3x3CV.C <- train(train.s, train.data$Role, method = "svmRadial",
                    trControl = trC, tuneGrid = expand.grid(.sigma = bestSigma, .C = 2^seq(-2, 2)))

(bestCost <- model3x3CV.C$bestTune$C)

mod.svmRadial <- model3x3CV.C$finalModel

conf.matrix(mod.svmRadial, train.s, train.data$Role)
conf.matrix(mod.svmRadial, test.s, test.data$Role)



# -- SVM (poly) --

set.seed(270217)
model3x3CV.poly <- tune(svm, train.s, train.data$Role,
                        ranges = list(degree = 1:5, coef0 = 1, cost = 1),
                        kernel = "polynomial",
                        tunecontrol = tune.control(sampling = "cross", nrepeat = 3, cross = 3))

mod.svmPoly <- model3x3CV.poly$best.model

conf.matrix(mod.svmPoly, train.s, train.data$Role)
conf.matrix(mod.svmPoly, test.s, test.data$Role)



# -- kNN --

set.seed(270217)
mod.knn <- train(train.s, train.data$Role, method = "knn",
                 trControl = trC, tuneGrid = expand.grid(.k = 1:7))
plot(mod.knn)

set.seed(270217)
mod.knn <- train(train.s, train.data$Role, method = "knn",
                 trControl = trC, tuneGrid = expand.grid(.k = 3))

model.knn <- mod.knn$finalModel

set.seed(270217) # Ties are solved randomly
conf.matrix(model.knn, train.s, train.data$Role, "class")
conf.matrix(model.knn, test.s, test.data$Role, "class")


# -- Decision tree --

library(rpart)

mod.tree <- rpart (Role ~ ., data=train.data)

summary(mod.tree)
mod.tree

# install.packages("rpart.plot")
rpart.plot::rpart.plot(mod.tree)

conf.matrix(mod.tree, train.data, train.data$Role, "vector")
conf.matrix(mod.tree, test.data, test.data$Role, "vector")

predict(mod.tree, type = "vector")


#  -- Random forest --
set.seed(270217)
mod.rf <- randomForest(Role ~ ., data=train.data, ntree = 1000)

conf.matrix(mod.rf, train.data, train.data$Role)
conf.matrix(mod.rf, test.data, test.data$Role)


