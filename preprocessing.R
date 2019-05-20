library(dplyr)
library(readr)
library(tidyr)
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

test.data <- data.2[1:bound,]
train.data <- data.2[(bound+1):nrow(data),]


# -------------------
# --- DATA MATRIX ---
# -------------------

numerical <- !sapply(data, is.factor)
data.m <- sapply(data[,numerical], as.numeric)

numerical.tr <- !sapply(train.data, is.factor)
train.m <- sapply(train.data[,numerical.tr], as.numeric)

numerical.te <- !sapply(test.data, is.factor)
test.m <- sapply(test.data[,numerical.te], as.numeric)

