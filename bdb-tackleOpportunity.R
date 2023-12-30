library(dplyr)

games <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/games.csv", header = TRUE)
players <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/players.csv", header = TRUE)
plays <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/plays.csv", header = TRUE)
tackles <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/tackles.csv", header = TRUE)
week1 <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/tracking_week_1.csv", header = TRUE)

## standardize data so all plays move from left to right
tracking <- week1 %>%
  mutate(xAdj = ifelse(playDirection == "left", 120-x, x)) %>%
  mutate(yAdj = ifelse(playDirection == "left", 160/3-y, y)) %>%
  mutate(dirAdj = ifelse(playDirection == "left", 180+dir, dir)%%360) %>%
  mutate(oAdj = ifelse(playDirection == "left", 180+o, o)%%360) %>%
  dplyr::select(-c(x, y, dir, o))

## adjusting angles to be between -90 and 90 instead of from 0 to 360
trackingNew <- tracking %>%
  mutate(dirNew = case_when(
    dirAdj <= 90 ~ 90 - dirAdj,
    dirAdj > 90 & dirAdj < 180 ~ dirAdj - 90,
    dirAdj > 180 & dirAdj < 270 ~ 270 - dirAdj,
    TRUE ~ dirAdj - 270
  )) %>%
  dplyr::select(-dirAdj)

### Isolating all RB and WR's ###
runningbacks <- players %>%
  filter(position == "RB" | position == "WR")

### Isolating all run plays ###
runPlays <- plays %>%
  filter(passResult == "R" | (ballCarrierId %in% runningbacks$nflId & passResult != "C"))

### Isolating ball carrier tracking data to add to other tracking data later ###
trackingBallCarrier <- trackingNew %>%
  inner_join(runPlays, by = c("gameId", "playId")) %>%
  filter(ballCarrierId == nflId) 

### Making ball carrier tracking column names easier to identify for when this is merged with linebacker tracking data that have the same column names ###
colnames(trackingBallCarrier)[c(14:17)] <- c("ballCarrierX", "ballCarrierY", "ballCarrierO", "ballCarrierDir")

### Keep only columns that will be useful when merged to linebacker tracking data later ###
trackingBallCarrier <- trackingBallCarrier %>%
  dplyr::select(gameId, playId, frameId, ballCarrierX, ballCarrierY, ballCarrierDir, ballCarrierO, ballCarrierDisplayName, ballCarrierId)

### Isolating linebackers tracking data to be merged with ball carrier tracking data later ###
trackingLinebackers <- trackingNew %>%
  inner_join(runPlays, by = c("gameId", "playId")) %>%
  inner_join(players, by = c("nflId", "displayName")) %>%
  #filter(displayName != "football") %>%
  #filter(club == defensiveTeam) %>%
  filter(position == "MLB" | position == "ILB") %>%
  left_join(tackles, by = c("gameId", "playId", "nflId"))

### Making linebacker tracking column names easier to identify for when this is merged with ball carrier tracking data that have the same column names ###
colnames(trackingLinebackers)[c(3, 4, 14:17)] <- c("defenderId", "defenderDisplayName", "defenderX", "defenderY", "defenderO", "defenderDir")

### Keep only columns that will be useful when merged to ball carrier tracking data later ###
trackingLinebackers <- trackingLinebackers %>%
  dplyr::select(gameId, playId, frameId, defenderId, defenderDisplayName, defenderX, defenderY, defenderDir, defenderO, club, s, a, dis, event, tackle, assist, pff_missedTackle)
trackingLinebackers <- replace(trackingLinebackers, is.na(trackingLinebackers), 0)

### Adding ball carrier metrics to defender tracking data and calculating Eucledian distance of defender to ball carrier ###
trackingPlays <- trackingLinebackers %>%
  left_join(trackingBallCarrier, by = c("gameId", "playId", "frameId")) %>%
  mutate(distToBallCarrier = sqrt((defenderX - ballCarrierX)^2 + (defenderY - ballCarrierY)^2))

### Adding in tackle attempt metric ###
### if the player made a tackle or first contact then this is a tackle attempt ###
trackingPlays <- trackingPlays %>%
  mutate(tackleAttempt = ifelse(event == "tackle" & (tackle == 1 | assist == 1 | pff_missedTackle == 1), 1, 0))

## R uses radians for trig functions so angles from orientation and direction need to be converted to radians
df <- trackingPlays %>%
  mutate(defenderDirRads = defenderDir * (pi/180),
         defenderORads = defenderO * (pi/180),
         ballCarrierDirRads = ballCarrierDir * (pi/180),
         ballCarrierORads = ballCarrierO * (pi/180)) %>%
  dplyr::select(-c(ballCarrierDir, ballCarrierO, defenderDir, defenderO))

## finding length of vector so vector components can be calculated 
df <- df %>%
  mutate(lenDefender = sqrt(defenderX^2 + defenderY^2),
         lenBallCarrier = sqrt(ballCarrierX^2 + ballCarrierY^2))

## calculating vector components
df <- df %>%
  mutate(vxDefenderDir = lenDefender*cos(defenderDirRads), 
         vyDefenderDir = lenDefender*sin(defenderDirRads), 
         vxBallCarrierDir = lenBallCarrier*cos(ballCarrierDirRads),
         vyBallCarrierDir = lenBallCarrier*sin(ballCarrierDirRads), 
         vxDefenderO = lenDefender*cos(defenderORads), 
         vyDefenderO = lenDefender*sin(defenderORads), 
         vxBallCarrierO = lenBallCarrier*cos(ballCarrierORads),
         vyBallCarrierO = lenBallCarrier*sin(ballCarrierORads))

