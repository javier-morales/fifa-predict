
source('preprocessing.R')

library(e1071)

library(caret)


## Utils 

conf.matrix <- function(mod, data, target) {
  pred <- predict(mod, data)
  cm <- table(pred, target)
  return(list(Confusion.matrix = cm, accuracy = sum(diag(cm))/sum(cm)))
}


## -- SVM (radial) --

bound <- round(nrow(train.data)/5)
cv.test <- train.data[1:bound,]
cv.train <- train.data[(bound+1):nrow(train.data),]

# CV of cost parameter
C <- 10^seq(-2, 3)
acc.C <- rep(NA, length(C))
for (i in 1:length(C)) {
  mod.svm <- svm(Role ~ Value+Age+Overall+Pace+Shooting,
                 data = cv.train,
                 kernel = "radial", cost = C[i])
  acc.C[i] <- conf.matrix(mod.svm, cv.test, cv.test$Role)$accuracy
  cat("Model", i, "done ( cost =", C[i], ")\n")
}
plot(1:length(acc.C), acc.C, type="l", xaxt= "n", xlab = "Cost", ylab = "Accuracy")
axis(1,at=1:length(acc.C),labels=C)
(best.cost <- C[which.max(acc.C)])
abline(v = which.max(acc.C), col = "blue", lty = 2)


# CV of gamma
gamma <- 2^seq(-3, 4)
acc.g <- rep(NA, length(gamma))
for (i in 1:length(gamma)) {
  mod.svm <- svm(Role ~ Value+Age+Overall+Pace+Shooting, data = train.data,
                 kernel = "radial", cost = 1, gamma = gamma[i])
  acc.g[i] <- conf.matrix(mod.svm, train.data, train.data$Role)$accuracy
  cat("Model", i, "done ( gamma =", gamma[i], ")\n")
}
plot(1:length(acc.g), acc.g, type="l", xaxt= "n", xlab = "Gamma", ylab = "Accuracy")
axis(1,at=1:length(acc.g),labels=gamma)
(best.gamma <- gamma[which.max(acc.g)])
abline(v = which.max(acc.g), col = "blue", lty = 2)



# Best model SVM (radial)

t <- train.data$Role

svm.mod <- svm(Role~Value+Age+Overall+Pace+Shooting,
               data = train.data, kernel = "radial",
               cost = best.cost, gamma = best.gamma)

conf.matrix(svm.mod, test.data, test.data$Role)


