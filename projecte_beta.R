library(dplyr)

data <- read.csv("fifa.csv")



# --- Preprocessing ---

data <- data %>%
  select(-one_of("X", "ID", "Photo", "Flag", "Club.Logo", "Jersey.Number",
                 "Joined", "Loaned.From", "Contract.Valid.Until")) %>%
  filter(complete.cases(data))