library(dplyr)

games <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/games.csv", header = TRUE)
players <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/players.csv", header = TRUE)
plays <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/plays.csv", header = TRUE)
tackles <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/tackles.csv", header = TRUE)
week1 <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/tracking_week_1.csv", header = TRUE)
week2 <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/tracking_week_2.csv", header = TRUE)
week3 <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/tracking_week_3.csv", header = TRUE)
week4 <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/tracking_week_4.csv", header = TRUE)
week5 <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/tracking_week_5.csv", header = TRUE)
week6 <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/tracking_week_6.csv", header = TRUE)
week7 <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/tracking_week_7.csv", header = TRUE)
week8 <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/tracking_week_8.csv", header = TRUE)
week9 <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/tracking_week_9.csv", header = TRUE)

## combining weeks 1-3 of tracking data
tracking <- rbind(week1, week2, week3, week4, week5, week6, week7, week8, week9)

## standardizing x, y, dir, and o so all plays move from left to right
trackingNew <- tracking %>%
  mutate(xAdj = ifelse(playDirection == "left", 120-x, x)) %>%
  mutate(yAdj = ifelse(playDirection == "left", 160/3-y, y)) %>%
  mutate(dirAdj = ifelse(playDirection == "left", 180+dir, dir)%%360) %>%
  mutate(oAdj = ifelse(playDirection == "left", 180+o, o)%%360) %>%
  dplyr::select(-c(x, y, dir, o))

## making angles go between -90 and 90
trackingNew <- trackingNew %>%
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
colnames(trackingBallCarrier)[c(10, 14:17)] <- c("ballCarrierS", "ballCarrierX", "ballCarrierY", "ballCarrierDir", "ballCarrierO")

### Keep only columns that will be useful when merged to linebacker tracking data later ###
trackingBallCarrier <- trackingBallCarrier %>%
  dplyr::select(gameId, playId, frameId, ballCarrierX, ballCarrierY, ballCarrierDir, ballCarrierO, ballCarrierS, ballCarrierDisplayName, ballCarrierId)

### Isolating linebackers tracking data to be merged with ball carrier tracking data later ###
trackingLinebackers <- trackingNew %>%
  inner_join(runPlays, by = c("gameId", "playId")) %>%
  inner_join(players, by = c("nflId", "displayName")) %>%
  #filter(displayName != "football") %>%
  #filter(club == defensiveTeam) %>%
  filter(position == "MLB" | position == "ILB") %>%
  left_join(tackles, by = c("gameId", "playId", "nflId"))

### Making linebacker tracking column names easier to identify for when this is merged with ball carrier tracking data that have the same column names ###
colnames(trackingLinebackers)[c(3, 4, 10, 14:17)] <- c("defenderId", "defenderDisplayName", "defenderS", "defenderX", "defenderY", "defenderDir", "defenderO")

### Keep only columns that will be useful when merged to ball carrier tracking data later ###
trackingLinebackers <- trackingLinebackers %>%
  dplyr::select(gameId, playId, frameId, defenderId, defenderDisplayName, defenderX, defenderY, defenderDir, defenderO, club, defenderS, a, dis, event, tackle, assist, pff_missedTackle)
trackingLinebackers <- replace(trackingLinebackers, is.na(trackingLinebackers), 0)

### Adding ball carrier metrics to defender tracking data and calculating Eucledian distance of defender to ball carrier ###
trackingPlays <- trackingLinebackers %>%
  left_join(trackingBallCarrier, by = c("gameId", "playId", "frameId")) %>%
  mutate(distToBallCarrier = sqrt((defenderX - ballCarrierX)^2 + (defenderY - ballCarrierY)^2))

### R uses radians for trig functions so angles from orientation and direction need to be converted to radians
trackingPlays <- trackingPlays %>%
  mutate(defenderDirRads = defenderDir * (pi/180),
         defenderORads = defenderO * (pi/180),
         ballCarrierDirRads = ballCarrierDir * (pi/180),
         ballCarrierORads = ballCarrierO * (pi/180)) %>%
  dplyr::select(-c(ballCarrierDir, ballCarrierO, defenderDir, defenderO))

### calculating dot product and x and y projection vectors for defender and ball carrier respectively ###
### for checking to see if the defender is facing the ball carrier ###
trackingPlays <- trackingPlays %>%
  mutate(dotProd = defenderS*ballCarrierS*cos(ballCarrierDirRads - defenderDirRads),
         defenderProjX = defenderX + (defenderS*cos(defenderDirRads)),
         defenderProjY = defenderY + (defenderS*sin(defenderDirRads)),
         ballCarrierProjX = ballCarrierX + (ballCarrierS*cos(ballCarrierDirRads)),
         ballCarrierProjY = ballCarrierY + (ballCarrierS*sin(ballCarrierDirRads)))

### checking to see if the two players distances are getting closer to each other ###
trackingPlays <- trackingPlays %>%
  group_by(gameId, playId, defenderDisplayName) %>%
  mutate(distDiff = distToBallCarrier - lag(distToBallCarrier), 
         distDiffInd = ifelse(distDiff < 0, 1, 0)) %>%
  ungroup()
trackingPlays <- replace(trackingPlays, is.na(trackingPlays), 0)

## defining first contact and pulling frame number and ball carrier x position
startFrame <- trackingPlays %>%
  mutate(firstContact = ifelse(distToBallCarrier < 1 & distDiffInd == 1, 1, 0)) %>%
  filter(firstContact == 1) %>%
  group_by(gameId, playId, defenderId) %>%
  mutate(minFrame = min(frameId)) %>% 
  filter(minFrame == frameId) %>%
  dplyr::select(gameId, playId, defenderId, defenderDisplayName, frameId, ballCarrierX)

## updating column names for frame and ball carrier x position so when it is merged it will be clear this is the starting x position for the ball carrier
colnames(startFrame) <- c("gameId", "playId", "defenderId", "defenderDisplayName", "startFrameId", "startBallCarrierX")

## find tackle frame and designating this as the end frame of the play
tackleFrame <- trackingPlays %>%
  filter(tackle == 1 | assist == 1 | pff_missedTackle == 1) %>%
  filter(event == "tackle") %>%
  dplyr::select(gameId, playId, defenderDisplayName, defenderId, frameId, ballCarrierX)

## updating column names for frame and ball carrier x position so when it is merged it will be clear this is the ending x position for the ball carrier
colnames(tackleFrame) <- c("gameId", "playId", "defenderDisplayName", "defenderId", "endFrameId", "endBallCarrierX")

## merging columns of ending frame with starting frame to calculate ball carrier distance traveled after first contact
yac <- tackleFrame %>%
  left_join(startFrame, by = c("gameId", "playId", "defenderId", "defenderDisplayName"))

## if there was no first contact by the definition above, first contact was defined by the first_contact event (this was done manually, but doesn't have to be)
write.csv(yac, "C:/Users/danie/Downloads/yac.csv")
yacNew <- read.csv("C:/Users/danie/Downloads/yac.csv", header = TRUE)

## calculating yards after initial contact
yacNew <- yacNew %>%
  mutate(yardsAfterContact = endBallCarrierX - startBallCarrierX)

## summarizing average yards after contact for each defender and number of plays contributing to this calculation 
## calculating percentile and giving rank/grade based on percentile
yardsAfterContact <- yacNew %>%
  group_by(defenderDisplayName) %>%
  summarise(averageYAC = mean(yardsAfterContact), 
            numPlays = n()) %>%
  filter(numPlays >= 10) %>%
  mutate(yacPercentile = round(percent_rank(-averageYAC)*100, 0)) %>%
  mutate(yacGrade = case_when(
    yacPercentile > 90 & yacPercentile <= 100 ~ 1,
    yacPercentile > 80 & yacPercentile <= 90 ~ 2,
    yacPercentile > 70 & yacPercentile <= 80 ~ 3,
    yacPercentile > 60 & yacPercentile <= 70 ~ 4,
    .default = 5
  ))

## find where the defender makes contact and designate this as where contact begins 
## later this ball carrier x position will be subtracted from the end frame ball carrier x position to calculate how many yards the linebacker gave up after contact on a run play
## contact criteria is if the defender is within one yard of the ball carrier and facing him that is considered contact
## not every play has a tag for first contact and it is not clear which player made first contact therefore this event could not be used
## this contact criteria gets close enough for a reasonable answer of when first contact was made