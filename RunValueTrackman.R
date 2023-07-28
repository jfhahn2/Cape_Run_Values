library(tidyverse)
library(ggplot2)

setwd("~/Documents/Hyannis")
# Read in Data
new_data <- read_csv("cape724.csv")


# Delete Duplicate Pitches
new_data <- new_data %>%
  arrange(GameUID, PitchNo) %>%
  filter(PitchUID != lead(PitchUID), PitchNo != lead(PitchNo), Time != lead(Time))

# Read in Trackman run value chart
tm_rv <- read_csv("tm_run_values.csv")
playResults <- tmdata %>% mutate(tmResult = ifelse(PlayResult == "Undefined", PitchCall, PlayResult))
table(tmResults$tmResult)

# Modify result names to match
tmResults <- playResults %>% mutate(tmResult = case_when(
  grepl("Foul", tmResult) ~ "FoulBall",
  grepl("Ball", tmResult) ~ "Ball",
  grepl("eCalled", tmResult) ~ "Called Strike",
  grepl("Out", tmResult) ~ "Field Out",
  grepl("Home", tmResult) ~ "Home run",
  grepl("Swing", tmResult) ~ "Swinging Strike",
  grepl("eC", tmResult) ~ "Called Strike",
  TRUE ~ tmResult
))


table(tmResults$TaggedPitchType)
# Group Pitch types 
tmResults_fixed <- tmResults %>% mutate(TaggedPitchType = case_when(
  TaggedPitchType == "FourSeamFastBall" ~ "Fastball",
  TaggedPitchType == "OneSeamFastBall" ~ "Sinker",
  TaggedPitchType == "TwoSeamFastBall" ~ "Sinker",
  TaggedPitchType == "Undefined" ~ AutoPitchType,
  TaggedPitchType == "Other" ~ AutoPitchType,
  TRUE ~ TaggedPitchType
)) %>% mutate(TaggedPitchType = case_when(
  TaggedPitchType == "Changeup" ~ "ChangeUp",
  TaggedPitchType == "Four-Seam" ~ "Fastball",
  TRUE ~ TaggedPitchType
))
table(tmResults_fixed$TaggedPitchType)

# Differentiate Sac Bunts and Sac Flies
tmResults_sacs <- tmResults_fixed %>% mutate(tmResult = ifelse(tmResult == "Sacrifice", ifelse(TaggedHitType == "Bunt" | TaggedHitType == "GroundBall", "SacBunt", "SacFly"), tmResult)) 

# Merge results and run values
withRV <- tmResults_sacs %>% left_join(tm_rv, by = "tmResult")

# Run value by Pitcher
withRV %>% group_by(Pitcher) %>% summarise(totalRV = sum(MeanRV), pitches = n(), RV_100 = 100 * totalRV / pitches) %>% arrange(totalRV) -> pitcher_run_vals

# Run value by Pitcher and Pitch Type
withRV %>% group_by(Pitcher, TaggedPitchType) %>% summarise(totalRV = sum(MeanRV, na.rm = TRUE), pitches = n(), RV_100 = 100 * totalRV / pitches) %>% arrange(RV_100) %>% filter(pitches > 20) -> pit_rv724

# Run value by Batter and Pitch Type
withRV %>% group_by(Batter, TaggedPitchType) %>% summarise(totalRV = sum(MeanRV, na.rm = TRUE), pitches = n(), RV_100 = 100 * totalRV / pitches) %>% arrange(-RV_100) %>% filter(pitches > 20) -> bat_rv



