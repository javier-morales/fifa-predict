library(dplyr)
library(tidyr)
library(readr)
library(cclust)

data <- read.csv("fifa.csv")



# --- Preprocessing ---


data <- data %>%
  select(-one_of("X", "ID", "Photo", "Flag", "Club.Logo",
                 "Real.Face", "Jersey.Number", "Joined",
                 "Loaned.From", "Contract.Valid.Until","Body.Type")) %>%
  filter(complete.cases(data)) %>%
  filter(Position != "GK") %>%
  separate(Height, c('ft', 'inch'), "'", convert = TRUE) %>%
  separate(Work.Rate, c('WR.Attack', 'WR.Defense'), "/ ") %>%
  mutate(Value = parse_number(Value),
         Wage = parse_number(Value),
         Release.Clause = parse_number(Value),
         Weight = parse_number(Value) * 0.453592,  # lbs to kg
         Height = ft*30.48 + inch*2.54
  ) %>%
  mutate_at(vars(LS:RB), funs(parse_number)) %>%
  mutate_at(vars(Weak.Foot, Skill.Moves, WR.Attack, WR.Defense,), funs(as.factor)) %>%
  select(-one_of("ft", "inch"))

data <- data[1:1000,]

# --- data matrix ---

numerical <- !sapply(data, is.factor)

data.m <- sapply(data[,numerical], as.numeric)
data.m <- scale(data.m)

# ---- PCA -----

comp <- princomp(data.m)
screeplot(comp)


kmeans.4 <- cclust(data.m, centers = 4, method = "kmeans")

plot(comp$scores[,1], comp$scores[,2], col = kmeans.4$cluster)

points(comp$scores[1:10,1], comp$scores[1:10,2], pch = 8)




