library(dplyr)
library(tidyr)
library(readr)

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
  mutate_at(vars(LS:RB), funs(parse_number))

