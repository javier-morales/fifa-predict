source("preprocessing.R")


# -- LASSO --

library(glmnet)

# Target: Value
x <- train.m[,-4]
t <- train.m[, 4]

mod.lasso <- cv.glmnet(x, t, nfolds = 10)

coef(mod.lasso)

# NRMSE (the lower the better)
nrmse <- function(obs, pred) {
  return(sqrt(sum((obs - pred)^2)/(length(obs)-1))/sd(obs))
}

p.tr <- (predict(mod.lasso, newx = x, s = "lambda.min"))
p.te <- (predict(mod.lasso, newx = test.m[,-4], s = "lambda.min"))

nrmse(t, p.tr)  # equivalent to 0.80 R^2
nrmse(test.m[,4], p.te) # 0.78 R^2


# -- RIDGE --

library(MASS)


models.ridge <- lm.ridge(Value ~ Age+Overall+Potential+Wage+International.Reputation+Skill.Moves+Pace+Shooting+Passing
                      + Dribbles + Defending+ Physicality, data = train.data, lambda = seq(0,10,0.1))

plot(seq(0,10,0.1), models.ridge$GCV, main="GCV of Ridge Regression", type="l", 
     xlab=expression(lambda), ylab="GCV")

(lambda.ridge <- seq(0,10,0.1)[which.min(models.ridge$GCV)])  # optimal lambda
abline(v=lambda.ridge,lty=2)

mod.ridge <- lm.ridge(Value ~ Age+Overall+Potential+Wage+International.Reputation+Skill.Moves+Pace+Shooting+Passing
                      + Dribbles + Defending+ Physicality, data = train.data, lambda = lambda.ridge)

predict.ridge <- function(model, data) {
  X <- scale(as.matrix(data[,names(model$coef)]))
  w <- model$coef
  return(X %*% w)
}

p.tr <- predict.ridge(mod.ridge, train.data)
p.te <- predict.ridge(mod.ridge, test.data)

nrmse(train.data$Value, p.tr)  # worse than lasso
nrmse(test.data$Value, p.te)


# -- Gamma GLM --

mod.poisson <- glm(Value ~ ., family = poisson, data = train.data)
p.tr <- predict(mod.poisson, train.data, type = 'response')
p.te <- predict(mod.poisson, test.data, type = 'response')

nrmse(train.data$Value, p.tr)
nrmse(test.data$Value, p.te)


mod.linear <- lm(log(Value) ~ ., data = train.data)
p.tr <- exp(predict(mod.linear, train.data))
p.te <- exp(predict(mod.linear, test.data))

nrmse(train.data$Value, p.tr)
nrmse(test.data$Value, p.te)


# -- Neural Network --


library(caret)

trC <- trainControl(method = "repeatedcv", number = 3, repeats = 3)

model.10x10CV <- train(Value ~ ., data = train.s, method = 'nnet', linout = T,
                       tuneGrid = expand.grid(.size = seq(1, 20, 2), .decay = 0), trControl = trC)

plot(model.10x10CV)

mod.nnet <- model.10x10CV$finalModel

p.tr <- predict(mod.nnet, train.s)
p.te <- predict(mod.nnet, test.s)

nrmse(train.s[,"Value"], p.tr)  # R^2 0.988
nrmse(test.s[,"Value"], p.te)  # R^2 0.981


