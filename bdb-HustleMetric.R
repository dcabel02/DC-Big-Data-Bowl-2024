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

### Combine tracking data ###
tracking <- rbind(week1, week2, week3, week4, week5, week6, week7, week8, week9)

### Standardizing position variables so all plays move in the same direction ###
trackingNew <- tracking %>%
  mutate(xAdj = ifelse(playDirection == "left", 120-x, x)) %>%
  mutate(yAdj = ifelse(playDirection == "left", 160/3-y, y)) %>%
  mutate(dirAdj = ifelse(playDirection == "left", 180+dir, dir)%%360) %>%
  mutate(oAdj = ifelse(playDirection == "left", 180+o, o)%%360) %>%
  dplyr::select(-c(x, y, dir, o))

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
colnames(trackingBallCarrier)[c(14:17)] <- c("ballCarrierX", "ballCarrierY", "ballCarrierDir", "ballCarrierO")

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
colnames(trackingLinebackers)[c(3, 4, 14:17)] <- c("defenderId", "defenderDisplayName", "defenderX", "defenderY", "defenderDir", "defenderO")

### Keep only columns that will be useful when merged to ball carrier tracking data later ###
trackingLinebackers <- trackingLinebackers %>%
  dplyr::select(gameId, playId, frameId, defenderId, defenderDisplayName, defenderX, defenderY, defenderDir, defenderO, club, s, a, dis, event, tackle, assist, pff_missedTackle)
trackingLinebackers <- replace(trackingLinebackers, is.na(trackingLinebackers), 0)

### Adding ball carrier metrics to defender tracking data and calculating Eucledian distance of defender to ball carrier ###
trackingPlays <- trackingLinebackers %>%
  left_join(trackingBallCarrier, by = c("gameId", "playId", "frameId")) %>%
  mutate(distToBallCarrier = sqrt((defenderX - ballCarrierX)^2 + (defenderY - ballCarrierY)^2))

### finding linebackers not involved in the tackle and indicating if they were within 3 yards of ball carrier at time of tackle
hustleDf <- trackingPlays %>%
  filter(tackle == 0 & assist == 0) %>%
  filter(event == "tackle") %>%
  mutate(distLessThan3 = ifelse(distToBallCarrier <= 3, 1, 0))

### Calculate mean distance to ball carrier and percentage of time distance to ball carrier is less than 2 at time of tackle so a percentile ranking can be given ###
### calculating percentile and rank/grade for each player based on their percentile
df <- hustleDf %>%
  group_by(defenderDisplayName) %>%
  summarise(meanDistance = mean(distToBallCarrier), 
            percentPlaysWithin3 = mean(distLessThan3), 
            totalPlays = n()) %>%
  filter(totalPlays > 25) %>%
  mutate(hustlePercentile = round(percent_rank(percentPlaysWithin3)*100, 0)) %>%
  mutate(hustleGrade = case_when(
    hustlePercentile > 90 & hustlePercentile <= 100 ~ 1,
    hustlePercentile > 80 & hustlePercentile <= 90 ~ 2,
    hustlePercentile > 70 & hustlePercentile <= 80 ~ 3,
    hustlePercentile > 60 & hustlePercentile <= 70 ~ 4,
    .default = 5
  ))
