library(tidyverse)
setwd("~/Documents/Hyannis")

# Read in PBP Data
pbp22 <- read_rds("2022pbp.rds")
pbp21 <- read_rds("2021pbp.rds")
pbp19 <- read_rds("2019pbp.rds")
pbp18 <- read_rds("2018pbp.rds")
pbp17 <- read_rds("2017pbp.rds")
pbp16 <- read_rds("2016pbp.rds")
pbp15 <- read_rds("2015pbp.rds")
pbp14 <- read_rds("2014pbp.rds")
pbp13 <- read_rds("2013pbp.rds")
pbp12 <- read_rds("2012pbp.rds")
pbp11 <- read_rds("2011pbp.rds")

# Merge PBP files together
pbps <- c(pbp11, pbp12, pbp13, pbp14, pbp15, pbp16, pbp17, pbp18, pbp19, pbp21, pbp22)

# Read PBP data from first game
game <- pbps[[1]]
apbp <- game$away_pbp
hpbp <- game$home_pbp
game_id <- max(hpbp$GameID, apbp$GameID, na.rm = TRUE)
full_pbp <- bind_rows(apbp, hpbp)
full_pbp$GameID <- game_id

# Read in PBP Data from all other games
for (i in seq(2,length(pbps))) {
  game <- pbps[[i]]
  apbp <- game$away_pbp
  hpbp <- game$home_pbp
  game_id <- max(hpbp$GameID, apbp$GameID, na.rm = TRUE)
  apbp$AtBatCount <- as.numeric(apbp$AtBatCount)
  hpbp$AtBatCount <- as.numeric(hpbp$AtBatCount)
  pbp <- bind_rows(apbp, hpbp)
  pbp$GameID <- game_id
  full_pbp <- bind_rows(full_pbp, pbp)
  
}

# Write PBP Data to a CSV 
write_csv(full_pbp, "full_pbp.csv")

