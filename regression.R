source("preprocessing.R")



# NRMSE (the lower the better)
nrmse <- function(obs, pred) {
  return(sqrt(sum((obs - pred)^2)/(length(obs)-1))/sd(obs))
}


# -- LASSO --

library(glmnet)

# Target: Value
x <- train.m[,-4]
t <- train.m[, 4]

set.seed(270217)
mod.lasso <- cv.glmnet(x, t, nfolds = 10)

coef(mod.lasso)

#png("lassolambda.png", units = "cm", width = 10, height = 8, res = 300)
plot(mod.lasso)

p.tr <- (predict(mod.lasso, newx = x, s = "lambda.min"))
p.te <- (predict(mod.lasso, newx = test.m[,-4], s = "lambda.min"))

nrmse(t, p.tr)  # equivalent to 0.80 R^2
nrmse(test.m[,4], p.te) # 0.78 R^2

head(p.te) # negative predictions! We don't want this


# -- RIDGE --

library(MASS)

set.seed(270217)
models.ridge <- lm.ridge(Value ~ Overall+Potential+Wage+International.Reputation+
                         Skill.Moves+Dribbles, data = train.data, lambda = seq(0,10,0.1))


#png("ridgelambda.png", units = "cm", width = 10, height = 8, res = 300)
plot(seq(0,10,0.1), models.ridge$GCV, main="GCV of Ridge Regression", type="l", 
     xlab=expression(lambda), ylab="GCV")

(lambda.ridge <- seq(0,10,0.1)[which.min(models.ridge$GCV)])  # optimal lambda
abline(v=lambda.ridge,lty=2)

mod.ridge <- lm.ridge(Value ~ Overall+Potential+Wage+International.Reputation+
                      Skill.Moves+Dribbles, data = train.data, lambda = lambda.ridge)

predict.ridge <- function(model, data) {
  X <- scale(as.matrix(data[,names(model$coef)]))
  w <- model$coef
  return(X %*% w)
}

p.tr <- predict.ridge(mod.ridge, train.data)
p.te <- predict.ridge(mod.ridge, test.data)

nrmse(train.data$Value, p.tr)  # worse than lasso, and same problem: negative predictions!
nrmse(test.data$Value, p.te)


# -- GLM (log link) --

trC <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

set.seed(270217)
model.10CV.glm <- train(Value ~ Overall+Potential+Wage+International.Reputation+
                 Skill.Moves+Dribbles, data = train.data, method = "glm",
                 family = gaussian(link = "log"))

mod.glm <- model.10CV.glm$finalModel

p.tr <- predict(mod.glm, train.data, type = 'response')
p.te <- predict(mod.glm, test.data, type = 'response')

nrmse(train.data$Value, p.tr) # R^2 0.95
nrmse(test.data$Value, p.te)  # R^2 0.941 -> good!

head(p.te) # Now there are only positive response values!
plot(test.data$Value, p.te)
abline(0, 1, col = "red")

# -- Neural Network --

library(caret) # for cross-validation

trC <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(270217)
model.5x3CVsize <- train(Value ~ ., data = train.s, method = 'nnet', linout = T,
                       tuneGrid = expand.grid(.size = seq(1, 20, 2), .decay = 0), trControl = trC)

plot(model.5x3CVsize)
nnet.bestSize <- model.5x3CVsize$bestTune$size

model.5x3CVdecay <- train(Value ~ ., data = train.s, method = 'nnet', linout = T,
                         tuneGrid = expand.grid(.size = nnet.bestSize, .decay = 10^seq(-2, 2, 0.5)), trControl = trC)

plot(model.5x3CVdecay)
mod.nnet <- model.5x3CVdecay$finalModel

p.tr <- predict(mod.nnet, train.s)
p.te <- predict(mod.nnet, test.s)

nrmse(train.s[,"Value"], p.tr)  # R^2 0.98
nrmse(test.s[,"Value"], p.te)  # R^2 0.97 -> good!

plot(test.s[, "Value"], p.te)
abline(0,1,col="red")


# More simple neural network

set.seed(270217)
model.5x3CVsize.simple <- train(Value ~ Overall+Potential+Wage+International.Reputation+
                              Skill.Moves+Dribbles, data = train.s, method = 'nnet',
                              linout = T, tuneGrid = expand.grid(.size = seq(1, 20, 2), .decay = 0),
                              trControl = trC)

plot(model.5x3CVsize.simple)

nnet.simple.bestSize <- model.5x3CVsize.simple$bestTune$size

model.5x3CVdecay.simple <- train(Value ~ Overall+Potential+Wage+International.Reputation+
                                  Skill.Moves+Dribbles, data = train.s, method = 'nnet',
                                linout = T, tuneGrid = expand.grid(
                                          .size = nnet.simple.bestSize,
                                          .decay = 10^seq(-2, 2, 0.5)),
                                trControl = trC)

plot(model.5x3CVdecay.simple)

mod.nnet.simple <- model.5x3CVdecay.simple$finalModel

p.tr <- predict(mod.nnet.simple, train.s)
p.te <- predict(mod.nnet.simple, test.s)

nrmse(train.s[,"Value"], p.tr) # R^2 0.970
nrmse(test.s[,"Value"], p.te) # R^2 0.962 -> good! and with less variables than previous net

mod.nnet$n
mod.nnet.simple$n # hidden layer is also slightly smaller

plot(test.s[, "Value"], p.te)
abline(0,1,col="red")

