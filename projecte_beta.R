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
data$Dribbling <- rowMeans(data[, c("Agility", "Balance", "Reactions", "BallControl",
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

Cov_M <- cov(data.m)

# We will remove the following variables because they are highly correlated
# and they are combinations of other variables
# Special, Overall
# ATT, MID, DEF

overall <- data$Overall

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


kmeans.3 <- cclust(data.m, centers = 3, method = "kmeans")


par(mfrow = c(1, 2))
plot(comp$scores[,1], comp$scores[,2], col = kmeans.3$cluster, 
     main = "K-means", xlab = "Comp1", ylab = "Comp2")
plot(comp$scores[,1], comp$scores[,2], col = data$Role, main = "Role",
     xlab = "Comp1", ylab = "Comp2")
par(mfrow = c(1, 1))



# --------------
# -- LASSO -----
# --------------
library(glmnet)

# Train model to predict value
x <- train.m[,-4]
t <- train.m[, 4]

mod.lasso <- cv.glmnet(x, t, nfolds = 10)

coef(mod.lasso)

p.tr <- predict(mod.lasso, newx = x, s = "lambda.min")
p.te <- predict(mod.lasso, newx = test.m[,-4], s = "lambda.min")

# R squared gives us an NRMSE of 0.2007968 with the test data
(NRMSE.train <- 1 - sum((p.tr - mean(train.m[, 4]))^2) / sum((train.m[, 4] - mean(train.m[, 4]))^2))

# R squared gives us an NRMSE of 0.1541901 with the test data
(NRMSE.test <- 1 - sum((p.te - mean(test.m[, 4]))^2) / sum((test.m[, 4] - mean(test.m[, 4]))^2))


# --
# Lasso predict potential
# --
library(glmnet)

x <- train.m[,-3]
t <- train.m[, 3]

mod.lasso <- cv.glmnet(x, t, nfolds = 10)

coef(mod.lasso)

p.tr <- predict(mod.lasso, newx = x, s = "lambda.min")
p.te <- predict(mod.lasso, newx = test.m[,-3], s = "lambda.min")

# R squared gives us an NRMSE of 0.2007968 with the test data
(NRMSE.train <- 1 - sum((p.tr - mean(train.m[, 3]))^2) / sum((train.m[, 3] - mean(train.m[, 3]))^2))

# R squared gives us an NRMSE of 0.1541901 with the test data
(NRMSE.test <- 1 - sum((p.te - mean(test.m[, 3]))^2) / sum((test.m[, 3] - mean(test.m[, 3]))^2))



# --------------
# -- RIDGE -----
# --------------

library(MASS)

mod.ridge <- lm.ridge(Value ~ . -Club -Nationality -Name -Preferred.Foot -Role, data = train, lambda = seq(0,5,0.5))

coef(mod.ridge)


# Generalized Cross Validation plot
plot(seq(0,5,0.5), mod.ridge$GCV, type  ="l")
abline(v = seq(0,5,0.5)[which.min(mod.ridge$GCV)], lty = 2)

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
(NRMSE.train <- 1 - sum((p.tr - mean(train.m[, 3]))^2) / sum((train.m[, 3] - mean(train.m[, 3]))^2))

# R squared gives us an NRMSE of 0.17715 with the test data
(NRMSE.test <- 1 - sum((p.te - mean(test.m[, 3]))^2) / sum((test.m[, 3] - mean(test.m[, 3]))^2))




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

kNNs <- vector(mode = "list")
for (k in c(1,3,5,7,10)) {
  predicted <- knn(train.cv.m, validation.m, train.cv$Role, k = k)
  kNNs[k] <- predicted
}






