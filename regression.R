source("preprocessing.R")

library(caret)
library(glmnet)


# -- LASSO --

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




