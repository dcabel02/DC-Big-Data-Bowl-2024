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

### Isolating ball carrier ###
ballCarrier <- trackingNew %>%
  filter(event == "tackle") %>%
  inner_join(plays, by = c("gameId", "playId")) %>%
  filter(ballCarrierId == nflId) %>%
  dplyr::select(ballCarrierId, ballCarrierDisplayName, xAdj, yAdj, gameId, playId)

### Updating x and y column names so it's easier to identify when added to other tracking data sets ###
colnames(ballCarrier)[3:4] <- c("ballCarrierX", "ballCarrierY")

### Filtering all plays that end in tackle and only keeping last frame ###
tackling <- trackingNew %>%
  filter(event == "tackle") %>%
  inner_join(plays, by = c("gameId", "playId")) %>%
  inner_join(players, by = c("nflId", "displayName")) %>%
  filter(position == "ILB" | position == "MLB") %>%
  dplyr::select(nflId, displayName, xAdj, yAdj, gameId, playId) 

### Merging data sets together to calculate linebacker distance to ball carrier at time of tackle ###
tacklingNew <- tackling %>%
  inner_join(ballCarrier, by = c("gameId", "playId")) %>%
  mutate(distanceToBallCarrier = sqrt((xAdj - ballCarrierX)^2 + (yAdj - ballCarrierY)^2)) %>%
  mutate(distLessThan2 = ifelse(distanceToBallCarrier < 2, 1, 0))

### Calculate mean distance to ball carrier and percentage of time distance to ball carrier is less than 2 at time of tackle so a percentile ranking can be given ###
df <- tacklingNew %>%
  group_by(displayName) %>%
  summarise(meanDistance = mean(distanceToBallCarrier), 
            percentPlaysWithin2 = mean(distLessThan2), 
            totalPlays = n()) %>%
  filter(totalPlays > 25) %>%
  dplyr::select(-totalPlays) %>%
  mutate(percentileRank = ntile(percentPlaysWithin2, 100))
