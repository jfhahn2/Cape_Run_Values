library(tidyverse)
setwd("~/Documents/Hyannis")

# Read in data
full_data <- read_csv("full_pbp.csv")
full_data$Year <- substr(full_data$Date, start = nchar(full_data$Date)-3, nchar(full_data$Date))

# Group by half inning
data <- full_data %>% mutate(half_inn_id = paste(GameID, Top_Btm, Inning, sep = "-"))
half_innings <- data %>%
  arrange(half_inn_id, InningPACount, PitchNumPA) %>%
  mutate(OutsOnPlay = (lead(Outs) - Outs) %% 3) %>%
  group_by(half_inn_id) %>%
  summarize(RunsInInning = sum(RunsScoredOnPlay), OutsInInning = sum(OutsOnPlay))

# Join on half inning
join_data <- data %>%
  inner_join(half_innings, by = "half_inn_id")

# Make sure 3 outs in inning, dummy var at start of inning
three_outs <- join_data %>%
  filter(OutsInInning == 3) %>%
  mutate(NewInning = if_else(half_inn_id == lag(half_inn_id, default = "0"), 0, 1))

# Calculate runs already scored and yet to be scored
three_outs %>% 
  group_by(grp = cumsum(NewInning == 1)) %>% 
  mutate(AlreadyScored = cumsum(RunsScoredOnPlay) - RunsScoredOnPlay) %>% 
  ungroup() %>% 
  mutate(YetToScore = RunsInInning - AlreadyScored) -> run_vars

# Base occupation variables
base_states <- run_vars %>% mutate(R1 = ifelse(is.na(Runner1B), 0, 1)) %>% mutate(R2 = ifelse(is.na(Runner2B), 0, 1)) %>% mutate(R3 = ifelse(is.na(Runner3B), 0, 1))

# Calculate RunExp for all 288 states
base_states %>% filter(Balls < 4, Strikes < 3, Outs < 3) %>% group_by(Outs, Balls, Strikes, R1, R2, R3) %>% summarize(RunExp = mean(YetToScore), num = n()) -> run_exp_pbp

# Create count variables in both dataframes
run_exp_short <- run_exp_pbp %>% mutate(BSOR = paste0(Balls, Strikes, Outs, " ", R1, R2, R3))
base_states <- base_states %>% mutate(BSOR = paste0(Balls, Strikes, Outs, " ", R1, R2, R3)) %>% mutate(OR = paste0(Outs, " ", R1, R2, R3))

# Count-independent variable
no_count <- run_exp_short %>% filter(Balls == 0, Strikes == 0) %>% mutate(OR = paste0(Outs, " ", R1, R2, R3))

# Merge both count-independent and count-dependent dfs
with_states <- base_states %>% left_join(run_exp_short %>% select(RunExp, BSOR), by = "BSOR")
with_states_nc <- with_states %>% left_join(no_count %>% select(RunExp, BSOR, OR), by = "OR") %>% rename(RE288 = RunExp.x, RE24 = RunExp.y)

# Calculate change in run value
next_states <- with_states_nc %>% mutate(NextValue288 = ifelse(grp == lead(grp, default = 0), lead(RE288), 0)) %>% mutate(RunValue288 = RunsScoredOnPlay + NextValue288 - RE288) %>% 
  mutate(NextValue24 = ifelse(grp == lead(grp), lead(RE24), 0)) %>% mutate(RunValue24 = RunsScoredOnPlay + NextValue24 - RE24)

# Group by batter
next_states %>% group_by(BatterName, Year) %>% summarise(TRV288 = sum(RunValue288), pitches = n()) %>% arrange(-TRV288) -> batter_runvalue

# Fix hit by pitches
next_states %>% mutate(Result = ifelse(PitchResult == "In Play", PAResult, PitchResult)) %>% 
  mutate(ResultH = ifelse(is.na(PAResult), Result, ifelse(PAResult == "Hit by pitch", "HitByPitch", Result))) -> results

# Run value by play
results %>% group_by(ResultH) %>% summarize(AvgRV = mean(RunValue288, na.rm = TRUE)) -> play_runvalue

# Modify names to merge with trackman file
tm_result_table <- results %>% mutate(tmResult = case_when(
  grepl("Field out", Result) ~ "Field Out",
  grepl("Choice", Result) ~ "FieldersChoice",
  grepl("Foul", Result) ~ "FoulBall",
  grepl("bunt", Result) ~ "SacBunt",
  grepl("fly", Result) ~ "SacFly",
  grepl("error", Result) ~ "Error",
  TRUE ~ ResultH
))
 
# Run values of possible trackman results
tm_runvalues <- tm_result_table %>% group_by(tmResult) %>% summarize(MeanRV = mean(RunValue288))
write_csv(tm_runvalues, "tm_run_values.csv")




## Running play calculations
results %>% filter((RunningEvent == TRUE | grepl("caught", Description)) & PitchResult != "In Play") -> running
running %>% mutate(RunPlay = ifelse(grepl("caught", Description), "CS", 
                                    ifelse(grepl("steals", Description), "SB", 
                                    ifelse(grepl("passed ball", Description), "PB", 
                                           ifelse(grepl("ild pitch", Description), "WP", "Other"))))) -> running
running %>% filter(RunPlay != "Other") -> running
running %>% group_by(RunPlay) %>% summarize(AvgRV = mean(RunValue288, na.rm = TRUE)) -> running_runvalue
names(running_runvalue) <- c("wobaResult", "Weight")
write_csv(running_runvalue, "running.csv")





## WOBA Calculation
state_tbl <- table(next_states$PAResult)

# Change names to wOBA Results
woba_table <- results %>% mutate(wobaResult = case_when(
  grepl("acrifice fly", Result) ~ "SacFly",
  grepl("bunt", Result) ~ "SacBunt",
  grepl("Field out", Result) ~ "Out",
  grepl("Strikeout", PAResult) ~ "Out",
  grepl("Choice", Result) ~ "Out",
  grepl("error", Result) ~ "Out",
  TRUE ~ PAResult
))

# Filter for wOBA-relevant results
woba_table %>% group_by(wobaResult) %>% summarize(AvgRV = mean(RunValue24, na.rm = TRUE), num = n()) %>% 
  filter(wobaResult %in% c("Single", "Double", "Triple", "Home run", "Walk", "Hit by pitch", "Out", "SacBunt", "SacFly")) -> play_runvalue

# Find value of an out
out_rv <- play_runvalue[play_runvalue$wobaResult == "Out",] %>% pull(AvgRV)
play_runvalue <- play_runvalue %>% mutate(OutZero = AvgRV - out_rv) 

# Weighted sum to find league woba
play_runvalue <- play_runvalue %>% mutate(WeightedSum = num * OutZero)
woba_denom <- sum(play_runvalue$WeightedSum) / sum(play_runvalue$num)

# Find league OBP
league_obp <- next_states %>% filter(PAResult %in% c("Single", "Double","Triple", "Home run", "Walk", "Hit by pitch", "Intentional walk")) %>% 
  summarise(lwoBA = n() / sum(state_tbl)) %>% pull(lwoBA)

# Calculate wOBA Scale
woba_scale <- league_obp / woba_denom

# Find linear weights
play_runvalue <- play_runvalue %>% mutate(Weight = OutZero * woba_scale)
linear_weights <- play_runvalue %>% select(wobaResult, Weight)

# Weights by event
w1B <- linear_weights %>% filter(wobaResult == "Single") %>% pull(Weight)
w2B <- linear_weights %>% filter(wobaResult == "Double") %>% pull(Weight)
w3B <- linear_weights %>% filter(wobaResult == "Triple") %>% pull(Weight)
wHR <- linear_weights %>% filter(wobaResult == "Home run") %>% pull(Weight)
wBB <- linear_weights %>% filter(wobaResult == "Walk") %>% pull(Weight)
wHBP <- linear_weights %>% filter(wobaResult == "Hit by pitch") %>% pull(Weight)
wSAC <- linear_weights %>% filter(wobaResult == "SacBunt") %>% pull(Weight)
wSF <- linear_weights %>% filter(wobaResult == "SacFly") %>% pull(Weight)

write_csv(linear_weights, "linear_weights.csv")

# Find wOBA for each Batter Season
woba_summary <- woba_table %>%
  group_by(BatterName, Year, OffenseTeam, wobaResult) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = wobaResult, values_from = count, values_fill = 0) %>%
  mutate(PA = Single + Double + Triple + `Home run` + Walk + `Hit by pitch` + Out + SacFly + SacBunt) %>%
  mutate(wOBA = (w1B * Single + w2B * Double + w3B * Triple + wHR * `Home run` + wBB * Walk + wHBP * `Hit by pitch` + wSAC * `SacBunt` + wSF * `SacFly`) / PA)
  

## Build an RE288 Matrix
run_exp_pbp %>% mutate(Base2 = ifelse(R2 == 1, 2, 0)) %>% mutate(Base3 = ifelse(R3 == 1, 3, 0)) %>% mutate(Bases = paste0(R1, Base2, Base3)) %>% 
  mutate(Bases = gsub("0", "", Bases)) %>% mutate(Bases = ifelse(Bases == "", "0", Bases)) %>% select(R1, R2, R3, Bases, Outs, Balls, Strikes, RunExp) -> re288

# Calculate impact of a strike call
incorrect_strike <- function(balls, strikes, outs, bases) {
  strikes <- strikes + 1
  if (strikes == 3) {
    balls <- 0
    strikes <- 0
    outs <- outs + 1
    if (outs == 3) {
      return(0)
    }
  }
  
  new <- re288 %>% filter(Balls == balls, Strikes == strikes, Bases == bases, Outs == outs) %>% pull(RunExp)
  return(new)
  
}

# Calculate impact of a ball call
incorrect_ball <- function(balls, strikes, outs, bases) {
  balls <- balls + 1
  if (balls == 4) {
    balls <- 0
    strikes <- 0
    
    if(bases == "0") {
      bases = "1"
    }
    else if(bases == "1") {
      bases = "12"
    }
    else if(bases == "2") {
      bases = "12"
    }
    else if(bases == "3") {
      bases = "13"
    }
    else if(bases == "12") {
      bases = "123"
    }
    else if(bases == "13") {
      bases = "123"
    }
    else if(bases == "23") {
      bases = "123"
    }
    else if(bases == "123") {
      new <- re288 %>% filter(Balls == 0, Strikes == 0, Bases == "123", Outs == outs) %>% pull(RunExp)
      return(new + 1)
    }
  }
  
  new <- re288 %>% filter(Balls == balls, Strikes == strikes, Bases == bases, Outs == outs) %>% pull(RunExp)
  return(new)
  
}

# Add call impacts to RE288 table
re288$IncStrike <- -re288$RunExp + mapply(incorrect_strike, balls = re288$Balls, strikes = re288$Strikes, outs = re288$Outs, bases = re288$Bases)
re288$IncBall <- -re288$RunExp + mapply(incorrect_ball, balls = re288$Balls, strikes = re288$Strikes, outs = re288$Outs, bases = re288$Bases)
re288$CallSpread <- re288$IncBall - re288$IncStrike
write_csv(re288, "re288_cape.csv")
