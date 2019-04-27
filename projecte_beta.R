library(dplyr)
library(tidyr)
library(readr)
library(cclust)

data <- read.csv("fifa.csv")


# ---------------------
# --- PREPROCESSING ---
# ---------------------

# We first remove useless variables.
# Goalkeepers (NAs) and their stats are also removed


#WATCH OUT WITH X...  !!!!
data <- data %>%
  select(-one_of("X...", "ID", "Photo", "Flag", "Club.Logo",
                 "Real.Face", "Jersey.Number", "Joined", "Release.Clause",
                 "Loaned.From", "Contract.Valid.Until","Body.Type")) %>%
  filter(complete.cases(data)) %>%
  filter(Position != "GK") %>%
  select(-one_of("GKDiving", "GKHandling", "GKKicking", "GKPositioning", "GKReflexes")) 

# Next we change units, parse numbers and separate variables that merged

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
  mutate_at(vars(Weak.Foot, Skill.Moves, WR.Attack, WR.Defense), factor, ordered = TRUE)

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


# -----------------------
# --- RESAMPLING DATA ---
# -----------------------

# 1. shuffle data
data <- data[sample(nrow(data)),]

# 2. split training and test data (20% test data)
bound <- floor((nrow(data)/5))

test <- data[1:bound,]
train <- data[(bound+1):nrow(data),]


# -------------------
# --- DATA MATRIX ---
# -------------------

numerical <- !sapply(data, is.factor)

data.m <- sapply(data[,numerical], as.numeric)
data.m <- scale(data.m)

# --------------
# ---- PCA -----
# --------------

comp <- princomp(data.m)
screeplot(comp)


kmeans.3 <- cclust(data.m, centers = 3, method = "kmeans")


par(mfrow = c(1, 2))
plot(comp$scores[,1], comp$scores[,2], col = kmeans.3$cluster, 
     main = "K-means", xlab = "Comp1", ylab = "Comp2")
points(comp$scores[1:10,1], comp$scores[1:10,2], pch = 8, col = "gold")
plot(comp$scores[,1], comp$scores[,2], col = cut(data$Overall, breaks = 3) , main = "Role",
     xlab = "Comp1", ylab = "Comp2")
points(comp$scores[1:10,1], comp$scores[1:10,2], pch = 8, col = "gold")
par(mfrow = c(1, 1))

points(comp$scores[1:10,1], comp$scores[1:10,2], pch = 8)


#-------------
#Covariance matrix
#-------------
Cov_M <- cov(data.m)
#Which variables are correlated?

#COMMENT VARIABLES!!!

#

#-------------
#---- GLM ----
#-------------

#First approximation to a GLM using Step-Algorithm.

#We use a subset of the train data 

train_sub <- train

train_sub <- sapply(train[,numerical], as.numeric)

train_sub <- as.data.frame(train_sub)


First.Model <- glm(Value~.-Overall, data = train_sub, family = gaussian(link = "identity"))


summary(First.Model)

plot(First.Model)


#The first approximation computes a poor model. 

#Let's try the Step algorithm.

First.Model.AIC <- step(First.Model)

#The model gives a very large AIC, that is, it is overfitted.

#Second Approximation

Sec.model <- glm(Value~.-Overall -ATT -MID -DEF -Special, data=train_sub, family = gaussian(link="identity"))
summary(Sec.model)

#Remove some variables (Position Puntuation)
Sec.model <- step(Sec.model)


### Let's try to predict the potential



