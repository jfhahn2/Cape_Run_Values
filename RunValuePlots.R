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

# Filter for a specific pitcher
withRV %>% filter(Pitcher == "Frank, Hayden") -> player

# Group pitches into zones
withZones <- player %>% mutate(Zone = case_when(
  PlateLocHeight > 3.4 & PlateLocSide < -0.84 ~ 1,
  PlateLocHeight > 3.4 & PlateLocSide < -0.28 ~ 2,
  PlateLocHeight > 3.4 & PlateLocSide < 0.28 ~ 3,
  PlateLocHeight > 3.4 & PlateLocSide < 0.84 ~ 4,
  PlateLocHeight > 3.4 & PlateLocSide > 0.84 ~ 5,
  PlateLocHeight > 2.8 & PlateLocSide < -0.84 ~ 6,
  PlateLocHeight > 2.8 & PlateLocSide < -0.28 ~ 7,
  PlateLocHeight > 2.8 & PlateLocSide < 0.28 ~ 8,
  PlateLocHeight > 2.8 & PlateLocSide < 0.84 ~ 9,
  PlateLocHeight > 2.8 & PlateLocSide > 0.84 ~ 10,
  PlateLocHeight > 2.2 & PlateLocSide < -0.84 ~ 11,
  PlateLocHeight > 2.2 & PlateLocSide < -0.28 ~ 12,
  PlateLocHeight > 2.2 & PlateLocSide < 0.28 ~ 13,
  PlateLocHeight > 2.2 & PlateLocSide < 0.84 ~ 14,
  PlateLocHeight > 2.2 & PlateLocSide > 0.84 ~ 15,
  PlateLocHeight > 1.6 & PlateLocSide < -0.84 ~ 16,
  PlateLocHeight > 1.6 & PlateLocSide < -0.28 ~ 17,
  PlateLocHeight > 1.6 & PlateLocSide < 0.28 ~ 18,
  PlateLocHeight > 1.6 & PlateLocSide < 0.84 ~ 19,
  PlateLocHeight > 1.6 & PlateLocSide > 0.84 ~ 20,
  PlateLocSide < -0.84 ~ 21,
  PlateLocSide < -0.28 ~ 22,
  PlateLocSide < 0.28 ~ 23,
  PlateLocSide < 0.84 ~ 24,
  TRUE ~ 25
))

# Set up bounds of zones
x_val <- rep(c(-1.12,-0.56, 0, 0.56, 1.12),5)
y_val <- c(rep(3.7,5), rep(3.1,5), rep(2.5,5), rep(1.9, 5), rep(1.3,5))
Zone <- seq(1,25)
bounds <- as.data.frame(cbind(Zone, x_val, y_val))

# Filter most common pitch types
pitchList <- withZones %>% group_by(TaggedPitchType) %>% summarise(pc = n()) %>% filter(pc > 25) %>% pull(TaggedPitchType)

# Find Run Value by Zone
group_zone <- withZones %>% filter(TaggedPitchType %in% pitchList) %>%
  group_by(Zone, TaggedPitchType) %>% summarize(RV = sum(MeanRV, na.rm = TRUE), pitches = n()) %>% inner_join(bounds, by = "Zone")

# Draw Home Plate
pentagon <- data.frame(
  x = c(-17 / 24, -17 / 24, 0, 17 / 24, 17 / 24),
  y = c(0, .3, .58, .3, 0)
)

# Plot run value by zone
ggplot(group_zone) +
  aes(x = x_val, y = y_val, fill = RV) +
  geom_tile(width = 0.56, height = 0.6) +
#  geom_text(aes(label = round(RV,2)), size = 3) +  # Add geom_text() for labels
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_rect(xmin = -0.84, xmax = 0.84, ymin = 1.6, ymax = 3.4, col = "green", alpha = 0, size = 0.2) +
  facet_wrap(~ TaggedPitchType) +
  geom_polygon(data = pentagon, aes(x, y), color = "black", fill = "white", alpha = 0.5)



