library(dplyr)
library(tidyr)
library(readr)
library(cclust)

data <- read.csv("fifa.csv")



# --- Preprocessing ---


data <- data %>%
  select(-one_of("X", "ID", "Photo", "Flag", "Club.Logo",
                 "Real.Face", "Jersey.Number", "Joined", "Release.Clause",
                 "Loaned.From", "Contract.Valid.Until","Body.Type")) %>%
  filter(complete.cases(data)) %>%
  filter(Position != "GK") %>%
  separate(Height, c('ft', 'inch'), "'", convert = TRUE) %>%
  separate(Work.Rate, c('WR.Attack', 'WR.Defense'), "/ ") %>%
  mutate(Value = parse_number(Value),
         Wage = parse_number(Wage),
         Weight = parse_number(Weight) * 0.453592,  # lbs to kg
         Height = ft*30.48 + inch*2.54
  ) %>%
  mutate_at(vars(LS:RB), funs(parse_number)) %>%
  mutate_at(vars(Weak.Foot, Skill.Moves, WR.Attack, WR.Defense,), funs(as.factor)) %>%
  select(-one_of("ft", "inch"))

#data <- data[0:500,]


data$Role <- data$Position
levels(data$Role) <- c("", "ATT", "DEF", "MID", "ATT", "MID",
            "DEF", "ATT", "DEF", "DEF", "MID", "MID",
            "ATT", "MID", "ATT", "ATT", "DEF", "MID",
            "DEF", "DEF", "MID", "MID", "ATT", "MID",
            "ATT", "ATT", "DEF", "ATT")
data$Position <- NULL


# --- resampling data ---

# 1. shuffle data
data <- data[sample(nrow(data)),]

# 2. split training and test data (20% test data)
bound <- floor((nrow(data)/5))

test <- data[1:bound,]
train <- data[(bound+1):nrow(data),]


# --- data matrix ---

numerical <- !sapply(data, is.factor)

data.m <- sapply(data[,numerical], as.numeric)
data.m <- scale(data.m)

# ---- PCA -----

comp <- princomp(data.m)
screeplot(comp)


kmeans.3 <- cclust(data.m, centers = 3, method = "kmeans")


par(mfrow = c(1, 2))
plot(comp$scores[,1], comp$scores[,2], col = kmeans.3$cluster, 
     main = "K-means", xlab = "Comp1", ylab = "Comp2")
points(comp$scores[1:10,1], comp$scores[1:10,2], pch = 8, col = "gold")
plot(comp$scores[,1], comp$scores[,2], col = data$Role , main = "Role",
     xlab = "Comp1", ylab = "Comp2")
points(comp$scores[1:10,1], comp$scores[1:10,2], pch = 8, col = "gold")
par(mfrow = c(1, 1))

#COMENTAR LO DEL MIG DEL CAMP QUE SON UNA MESCLA DE ATAC I DEFENSA
#I QUE EL NOMBRE DE CLUSTERS ES TRES I TAL
#HEM "PREDIT" EL ROL DEL JUGADOR


points(comp$scores[1:10,1], comp$scores[1:10,2], pch = 8)


