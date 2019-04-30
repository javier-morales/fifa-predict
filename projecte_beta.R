library(dplyr)
library(tidyr)
library(readr)
library(cclust)
library(stringr)

data <- read.csv("fifa.csv")


# ---------------------
# --- PREPROCESSING ---
# ---------------------

# We first remove useless variables.
# Goalkeepers (NAs) and their stats are also removed


#WATCH OUT WITH X...  !!!!
data <- data %>%
  select(-one_of("X", "ID", "Photo", "Flag", "Club.Logo",
                 "Real.Face", "Jersey.Number", "Joined", "Release.Clause",
                 "Loaned.From", "Contract.Valid.Until","Body.Type")) %>%
  filter(complete.cases(data)) %>%
  filter(Position != "GK") %>%
  select(-one_of("GKDiving", "GKHandling", "GKKicking", "GKPositioning", "GKReflexes")) 

# Next we change units, parse numbers and separate variables that merged

data$M.value <- sapply(data$Value, str_sub, start=-1) == 'M'
data$M.wage <- sapply(data$Wage, str_sub, start=-1) == 'M'

data <- data %>%
  separate(Height, c('ft', 'inch'), "'", convert = TRUE) %>%
  separate(Work.Rate, c('WR.Attack', 'WR.Defense'), "/ ") %>%
  mutate(Value = parse_number(Value),
         Wage = parse_number(Wage),
         Weight = parse_number(Weight) * 0.453592,  # lbs to kg
         Height = ft*30.48 + inch*2.54
  ) %>%
  select(-one_of("ft", "inch")) %>%
  mutate_at(vars(LS:RB), parse_number) %>%
  mutate_at(vars(Weak.Foot, Skill.Moves), parse_number) %>%
  mutate_at(vars(WR.Attack, WR.Defense), factor, ordered = TRUE)

data$Wage <- ifelse(data$M.wage, data$Wage * 1000000, data$Wage * 1000)
data$Value <- ifelse(data$M.value, data$Value * 1000000, data$Value * 1000)
data$M.value <- NULL
data$M.wage <- NULL

levels(data$WR.Attack) <- c("Low", "Medium", "High")  # Correct order
levels(data$WR.Defense) <- c("Low", "Medium", "High")

# Now we will group some factors and variables together.
# We are going to create the 'Role' variable using 'Position'. It will
# only have three levels: ATT, MID and DEF.

data$Role <- data$Position
levels(data$Role) <- c("", "ATT", "DEF", "MID", "ATT", "MID",
            "DEF", "ATT", "DEF", "DEF", "MID", "MID",
            "ATT", "MID", "ATT", "ATT", "DEF", "MID",
            "DEF", "DEF", "MID", "MID", "ATT", "MID",
            "ATT", "ATT", "DEF", "ATT")
data$Position <- NULL
data$Role <- droplevels(data$Role)

# We are going to group  all the different habilitiy variables (shooting, dribbling,...) into
# more simple groups, according to FIFA 19 game.

#the dataset source (sofifa.com): Attacking, Skill, 
# Movement, Power, Mentality and Defending.

data$Pace <- rowMeans(data[,c("Acceleration","SprintSpeed")])
data$Shooting <- rowMeans(data[, c("Positioning", "Finishing", "ShotPower",
                                   "LongShots", "Volleys","Penalties")])
data$Passing <- rowMeans(data[,c("Vision", "Crossing", "FKAccuracy",
                                 "ShortPassing", "LongPassing","Curve")])
data$Dribbles <- rowMeans(data[, c("Agility", "Balance", "Reactions", "BallControl",
                                    "Dribbling","Composure")])
data$Defending <- rowMeans(data[, c("Interceptions", "HeadingAccuracy","Marking",
                                    "StandingTackle", "SlidingTackle")])
data$Physicality <- rowMeans(data[, c("Jumping", "Stamina", "Strength","Aggression")])

data <- select(data, -(Crossing:SlidingTackle)) # We remove them!

# We do the same thing for the position score of the players, the same
# way we did when making the 'Role' variable.

data$ATT <- rowMeans(data[, c("CAM", "CF", "LAM", "LF","LS",
                              "LW", "RF", "RS", "RW", "ST")])
data$MID <- rowMeans(data[, c("CDM", "CM", "LCM", "LDM","LM",
                              "RAM", "RCM", "RDM", "RM")])
data$DEF <- rowMeans(data[, c("CB", "LB", "LCB","LWB",
                              "RB", "RCB", "RWB")])

data <- select(data, -(LS:RB)) # We remove them!


#-------------------
# FEATURE SELECTION
#------------------

# We do the covariance matrix

#Cov_M <- cov(data.m)

# We will remove the following variables because they are highly correlated
# and they are combinations of other variables
# Special, Overall
# ATT, MID, DEF

#overall <- data$Overall

data$Special <- NULL
data$ATT <- NULL
data$MID <- NULL
data$DEF <- NULL


# -----------------------
# --- RESAMPLING DATA ---
# -----------------------


set.seed(123)
# 1. shuffle data
data.2 <- data[sample(nrow(data)),]

# 2. split training and test data (20% test data)
bound <- floor((nrow(data)/5))

test <- data.2[1:bound,]
train <- data.2[(bound+1):nrow(data),]


# -------------------
# --- DATA MATRIX ---
# -------------------

numerical <- !sapply(data, is.factor)
data.m <- sapply(data[,numerical], as.numeric)

numerical <- !sapply(train, is.factor)
train.m <- sapply(train[,numerical], as.numeric)

numerical <- !sapply(test, is.factor)
test.m <- sapply(test[,numerical], as.numeric)


# --------------
# ---- PCA -----
# --------------

comp <- princomp(scale(data.m))
screeplot(comp)


kmeans.3 <- cclust(data.m[,c(1,2,3,4)], centers = 3, method = "kmeans")


par(mfrow = c(1, 2))
plot(comp$scores[,1], comp$scores[,2], col = kmeans.3$cluster, 
     main = "K-means", xlab = "Comp1", ylab = "Comp2")
points(comp$scores[1:10,1], comp$scores[1:10,2], pch = 8, col = 'Yellow')
text(comp$scores[1:3,1], comp$scores[1:3,2], labels = data$Name[1:3])
text(comp$scores[16120:16122,1], comp$scores[16120:16122,2], labels = data$Name[16120:16122], col = "Blue")
#text(comp$scores[c(234,237),1], comp$scores[c(234,237),2], labels = data$Name[c(234,237)], col = "Blue")
plot(comp$scores[,1], comp$scores[,2], col = cut(data$Overall, breaks = 3), main = "Overall",
     xlab = "Comp1", ylab = "Comp2")
points(comp$scores[1:10,1], comp$scores[1:10,2], pch = 8, col = 'Yellow')
text(comp$scores[1:3,1], comp$scores[1:3,2], labels = data$Name[1:3])
text(comp$scores[16120:16122,1], comp$scores[16120:16122,2], labels = data$Name[16120:16122], col = "Blue")
#text(comp$scores[c(234,237),1], comp$scores[c(234,237),2], labels = data$Name[c(234,237)], col = "Blue")
par(mfrow = c(1, 1))


##CALINSKI - HARABASZ

data.k <- data.m[1:2500,]

do.kmeans <- function (whatK)
{
  r <- cclust (data.k,whatK,iter.max=100,method="kmeans",dist="euclidean")
  (clustIndex(r,data.k, index="calinski"))
}

max (replicate (100, do.kmeans(5)))

# So it is not a matter of wrong initialization, this is really the best 5-means can do here

res <- rep(NA,10)
for (K in 2:10){
  res[K] <- max (replicate (100, do.kmeans(K)))
}
plot(res, type="l")


#Boxplots
par(mfrow=c(2,3))
vars <- c(18,19,20,21,22,23)
for (i in vars) {
  boxplot(data[,i]~data$Role, main = colnames(data)[i], col = c(2,3,4))
}

#transformation for the Value?
par(mfrow=c(1,2))

eps <- 100
boxplot(data$Value, main = "Value")
boxplot(log(data$Value+eps), main = "Log-Value")

boxplot(data$Value~data$Role, main = "Value")
boxplot(log(data$Value)~data$Role, main = "Log-Value", col = c(2,3,4))

par(mfrow=c(1,1))


##Value transformated
eps <- 10
Value.trans <- log(data$Value+eps)

library(ggplot2)

plot(data$Nationality)
plot(data$Club)

plot(data$Preferred.Foot~data$Role, main = "Preferred Foot vS Role", xlab = "Role", ylab = "Preferred Foot")

par(mfrow=c(1,3))
boxplot(data$Overall, main = "Overall")
boxplot(data$Potential, main = "Potential")
boxplot(data$Value, main = "Value")
#-----------------
#----DATA VISUALIZATION II--#
#-----------------

#library(graphics)
#Star.var <- data.m[,11:16]
#Star.Best <- data.m[1:5,11:16]

#stars(Star.Best, scale = T)



# --------------
# -- LASSO -----
# --------------
library(glmnet)

# Train model to predict value
x <- train.m[,-4]
t <- train.m[, 4]

mod.lasso <- cv.glmnet(x, t, nfolds = 10)

coef(mod.lasso)

p.tr <- (predict(mod.lasso, newx = x, s = "lambda.min"))
p.te <- (predict(mod.lasso, newx = test.m[,-4], s = "lambda.min"))

# R squared gives us an NRMSE of 0.2007968 with the test data
(NRMSE.train <- (1 - sum((p.tr - mean(train.m[, 4]))^2) / sum((train.m[, 4] - mean(train.m[, 4]))^2)))

# R squared gives us an NRMSE of 0.1541901 with the test data
(NRMSE.test <- (1 - sum((p.te - mean(test.m[, 4]))^2) / sum((test.m[, 4] - mean(test.m[, 4]))^2)))



#--------------------------------
#-------GLM - Poisson------------
#--------------------------------

glm.mod <- glm(Value~Age+Overall+Potential+Wage+International.Reputation+Skill.Moves+Pace+Shooting+Passing
               + Dribbles + Defending+ Physicality, data = train, family = poisson)

p.tr <- predict(glm.mod, type = 'response')
p.te <- predict(glm.mod, newx = test, type = 'response')

# R squared gives us an NRMSE of 0.2007968 with the test data
(NRMSE.train <- 1 - sum((p.tr - mean(train.m[, 4]))^2) / sum((train.m[, 4] - mean(train.m[, 4]))^2))

# R squared gives us an NRMSE of 0.1541901 with the test data
(NRMSE.test <- 1 - sum((p.te - mean(test.m[, 4]))^2) / sum((test.m[, 4] - mean(test.m[, 4]))^2))




#-----------
#----LM-----
#-----------

library(caret)
K <- 10; TIMES <- 10
trc <- trainControl (method="repeatedcv", number=K, repeats=TIMES)

# STANDARD REGRESSION
model.std.10x10CV <- train (Value~Age+Overall+Potential+Wage+International.Reputation+Skill.Moves+Pace+Shooting+Passing
                            + Dribbles + Defending+ Physicality , data = train, trControl=trc, method='lm')

normalization.train <- (length(train$Value)-1)*var(train$Value)
(NMSE.std.train <- crossprod(predict (model.std.10x10CV) - train$Value) / normalization.train)



# --------------
# -- RIDGE -----
# --------------

library(MASS)

model.ridge <- lm.ridge(Value~Age+Overall+Potential+Wage+International.Reputation+Skill.Moves+Pace+Shooting+Passing
                      + Dribbles + Defending+ Physicality, data = train, lambda = seq(0,10,0.1))

coef(model.ridge)


# Generalized Cross Validation plot
plot(seq(0,10,0.1), model.ridge$GCV, type  ="l", main = "GCV", xlab=expression(lambda),ylab="GCV")
abline(v = seq(0,10,0.1)[which.min(model.ridge$GCV)], lty = 2)

#####FINAL MODEL WITH LAMBDA######

(lambda.ridge <- seq(0,10,0.1)[which.min(model.ridge$GCV)])

##PLOTS FOR RIDGE REGRESSION##

abline (v=lambda.ridge,lty=2)

#THE FINAL MODEL

ridge.final <- lm.ridge(Value~Age+Overall+Potential+Wage+International.Reputation+Skill.Moves+Pace+Shooting+Passing
                        + Dribbles + Defending+ Physicality, data = train, lambda = lambda.ridge)

# -----------
# Neural NET-
# -----------
library(nnet)

x <- train.m[,-3]
t <- train.m[, 3]

mod.nnet <- nnet(Value ~ ., data = scale(data.m), subset = bound, size = 10, linout = T)

p.tr <- predict(mod.nnet, newx = x)
p.te <- predict(mod.nnet, newx = test.m[,-c(2, 3, 4)])

# R squared gives us an NRMSE of 0.17715 with the test data
(NRMSE.train <- sqrt(1 - sum((p.tr - mean(train.m[, 3]))^2) / sum((train.m[, 3] - mean(train.m[, 3]))^2)))

# R squared gives us an NRMSE of 0.17715 with the test data
(NRMSE.test <- sqrt(1 - sum((p.te - mean(test.m[, 3]))^2) / sum((test.m[, 3] - mean(test.m[, 3]))^2)))




# --------------------------
# -                        -
# -     CLASSIFICATION     -
# -                        -
# --------------------------

# --------
# -- LDA -
# --------

data.lda <- data[,-match(c("Club", "Nationality", "Name", "Preferred.Foot", "WR.Attack", "WR.Defense"), colnames(data))]
  
mod.lda <- lda(Role ~ ., data = data.lda)

summary(mod.lda)

p.te <- predict(mod.lda, newdata = test)
cm <- table(test$Role, p.te$class)
sum(diag(cm)) / sum(cm)


# Plot lda (tipus PCA)
data.lda <- data.lda[,-match(c("Role"), colnames(data.lda))]
loadings <- as.matrix(data.lda) %*% as.matrix(mod.lda$scaling)

plot(loadings[,1], loadings[,2], type="n", xlab="LD1", ylab="LD2")
points(loadings[,1], loadings[,2], pch = 4, col = rainbow(3)[unclass(data$Role)])
legend("topright", c("ATT", "DEF", "MID"), fill = rainbow(3))


# --------
# - KNN
# -
library(class)

k.train <- train
k.test <- test

k.train$Club <- NULL
k.test$Club <- NULL


# CV
set.seed(123)

# 2. split training and test data (20% test data)
bound <- floor((nrow(train)/5))

validation <- train[1:bound,]
train.cv <- train[(bound+1):nrow(train),]


numerical <- !sapply(validation, is.factor)
validation.m <- sapply(validation[,numerical], as.numeric)
numerical <- !sapply(train.cv, is.factor)
train.cv.m <- sapply(train.cv[,numerical], as.numeric)

#kNN.e <- rep(NA,5)
e <- vector(mode = "numeric", length = 5)


#The value of error vs. k is an oscillation, so it depends in every relization, see some plots for different k's.
keys <- seq(1,10,1)

for (i in keys) {
  predicted <- knn(train.cv.m, validation.m, train.cv$Role, k = i)
  CM <- table(predicted, validation$Role)*100
  e[i] <- sum(diag(CM))/sum(CM)
}

plot(keys, e, type = 'l', xlab = "k", ylab="% of success", main = "% of success")

(Errors <- data.frame(keys,e))


final.knn <- knn(train.cv.m, validation.m, train.cv$Role, k = 1)

cm <- table(validation$Role,as.vector(final.knn))
sum(diag(cm)) / sum(cm)

#SOME PLOTS

nicecolors <- c('black','red','blue')
grid.size <- 20000
XLIM <- range(train.cv.m[,1])
grid.x <- seq(XLIM[1], XLIM[2], len=grid.size)

YLIM <- range(train.cv.m[,2])
grid.y <- seq(YLIM[1], YLIM[2], len=grid.size)

#PCA for Train data
train.cv.PCA <- princomp(scale(train.cv.m))
train.cv.R2 <- cbind(train.cv.PCA$scores[,1], train.cv.PCA$scores[,2])

#PCA for Validation data
validation.cv.PCA <- princomp(scale(validation.m))
validation.R2 <- cbind(validation.cv.PCA$scores[,1], validation.cv.PCA$scores[,2])

############
## TRY TO DO THE PCA FOR THE COMPLETE DATA AND THEN SEPARATE IT BETWEEN TRAIN AND VALIDATION


visualize.1NN <- function ()
{
  par(mfrow=c(1,1))
  
  predicted <- knn (train.cv.m, validation.m, train.cv$Role, k=1)
  
  # These are the predictions
  plot(train.cv.R2, xlab="X1", ylab="X2", type="n")
  points(validation.R2, col=nicecolors[as.numeric(predicted)], pch=16)
  #contour(grid.x, grid.y, matrix(as.numeric(predicted),grid.size,grid.size), 
         # levels=c(1,2,3), add=TRUE, drawlabels=FALSE)
  #NO EXECUTIS CONTOUR!!! PETA EL PC.
  # Add training points, for reference
  #points(train.cv.R2, col=rainbow(3), pch=4)
  title("1-NN classification")
}

visualize.1NN ()


############################################
############################################
#-MODELS FOR POTENTIAL-#####################
############################################
############################################


#############
###-LASSO-###
#############
# --
# Lasso predict potential
# --
#library(glmnet)

x <- train.m[,-3]
t <- train.m[, 3]

mod.lasso <- cv.glmnet(x, t, nfolds = 10)

coef(mod.lasso)

p.tr <- predict(mod.lasso, newx = x, s = "lambda.min")
p.te <- predict(mod.lasso, newx = test.m[,-3], s = "lambda.min")

# R squared gives us an NRMSE of 0.2007968 with the test data
(NRMSE.train <- (1 - sum((p.tr - mean(train.m[, 3]))^2) / sum((train.m[, 3] - mean(train.m[, 3]))^2)))

# R squared gives us an NRMSE of 0.1541901 with the test data
(NRMSE.test <- (1 - sum((p.te - mean(test.m[, 3]))^2) / sum((test.m[, 3] - mean(test.m[, 3]))^2)))

#16 % on training error
#15 % on testing error


##########
###-LM-###
##########
K <- 10; TIMES <- 10
trc <- trainControl (method="repeatedcv", number=K, repeats=TIMES)

# STANDARD REGRESSION
model.std.10x10CV <- train (Potential~Value+Age+Overall+Wage+International.Reputation+Skill.Moves+Pace+Shooting+Passing
                            + Dribbles + Defending+ Physicality , data = train, trControl=trc, method='lm')

normalization.train <- (length(train$Potential)-1)*var(train$Potential)
(NMSE.std.train <- crossprod(predict (model.std.10x10CV) - train$Potential) / normalization.train)

(NMSE.std.train)^2

#TRAIN error of 16,19 % or 2,6 %

##### LINEAR MODEL USING NON-ZERO PARAMETRES OF LASSO

lm.pot <- lm(Potential~Age+Overall+International.Reputation + Weight + Pace + Physicality, data = train)
summary(lm.pot)

#R-squared of 83,73 %
#Almost the same training error but with less variables

plot(mod.lasso)

par(mfrow = c(1,1))
plot(coef(mod.lasso))



#############
####Ridge####
#############

model.ridge <- lm.ridge(Potential~Age+Overall+Value+Wage+International.Reputation+Skill.Moves+Pace+Shooting+Passing
                      + Dribbles + Defending+ Physicality, data = train, lambda = seq(0,10,0.1))

coef(model.ridge)


# Generalized Cross Validation plot
#plot(seq(0,5,0.5), mod.ridge$GCV, type  ="l", main = "GCV", xlab="Lambda",ylab="GCV")
#abline(v = seq(0,5,0.5)[which.min(mod.ridge$GCV)], lty = 2)


plot(seq(0,10,0.1), model.ridge$GCV, main="GCV of Ridge Regression", type="l", 
     xlab=expression(lambda), ylab="GCV")

# The optimal lambda is given by
 (lambda.ridge <- seq(0,10,0.1)[which.min(model.ridge$GCV)])
abline (v=lambda.ridge,lty=2)

# We can plot the coefficients and see how they vary as a function of lambda:
#colors <- rainbow(12)

#matplot (seq(0,10,0.1), coef(model.ridge)[,-1], type="l",xlab=expression(lambda), 
 #        ylab=expression(hat(beta)), col=colors, lty=1, lwd=2, main="Ridge coefficients")
#abline (v=lambda.ridge, lty=2)
#abline (h=0, lty=2)
#arrows (5.5,0.45,5,0.35, length = 0.15)
#text (rep(10, 9), coef(model.ridge)[length(seq(0,10,0.1)),-1], colnames(train)[-9], pos=4, col=colors)
#text(5.5, 0.4, adj=c(0,-1), "best lambda", col="black", cex=0.75)

## So we refit our final ridge regression model using the best lambda:
model.ridgereg.FINAL <- lm.ridge(lpsa ~ ., data=train, lambda = lambda.ridge)
(beta.ridgereg.FINAL <- coef(model.ridgereg.FINAL))

model.ridge.Final <- lm.ridge(Potential~Age+Overall+Value+Wage+International.Reputation+Skill.Moves+Pace+Shooting+Passing
                              + Dribbles + Defending+ Physicality, data = train, lambda = lambda.ridge )







