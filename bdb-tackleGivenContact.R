library(dplyr)
library(REdaS)
library(caret)
library(pROC)

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

### adding week identifier for splitting data later on
week1$weekIdentifier <- "week 1"
week2$weekIdentifier <- "week 2"
week3$weekIdentifier <- "week 3"
week4$weekIdentifier <- "week 4"
week5$weekIdentifier <- "week 5"
week6$weekIdentifier <- "week 6"
week7$weekIdentifier <- "week 7"
week8$weekIdentifier <- "week 8"
week9$weekIdentifier <- "week 9"

### Combine tracking data for weeks 1-3 as proof of concept (model takes too long with all 9 weeks of data) ###
tracking <- rbind(week1, week2, week3, week4, week5, week6, week7, week8, week9)

### Standardizing position variables so all plays move in the same direction###
trackingNew <- tracking %>%
  mutate(xAdj = ifelse(playDirection == "left", 120-x, x)) %>%
  mutate(yAdj = ifelse(playDirection == "left", 160/3-y, y)) %>%
  mutate(dirAdj = ifelse(playDirection == "left", 180+dir, dir)%%360) %>%
  mutate(oAdj = ifelse(playDirection == "left", 180+o, o)%%360) %>%
  dplyr::select(-c(x, y, dir, o))

### updating angles to be between -90 and 90 degrees
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
colnames(trackingBallCarrier)[c(10, 15:18)] <- c("ballCarrierS", "ballCarrierX", "ballCarrierY", "ballCarrierO", "ballCarrierDir")

### Keep only columns that will be useful when merged to linebacker tracking data later ###
trackingBallCarrier <- trackingBallCarrier %>%
  dplyr::select(gameId, playId, frameId, ballCarrierS, ballCarrierX, ballCarrierY, ballCarrierDir, ballCarrierO, ballCarrierDisplayName, ballCarrierId)

### Isolating linebackers tracking data to be merged with ball carrier tracking data later ###
trackingLinebackers <- trackingNew %>%
  inner_join(runPlays, by = c("gameId", "playId")) %>%
  inner_join(players, by = c("nflId", "displayName")) %>%
  filter(position == "MLB" | position == "ILB") %>%
  left_join(tackles, by = c("gameId", "playId", "nflId"))

### Making linebacker tracking column names easier to identify for when this is merged with ball carrier tracking data that have the same column names ###
colnames(trackingLinebackers)[c(3, 4, 10, 15:18)] <- c("defenderId", "defenderDisplayName", "defenderS", "defenderX", "defenderY", "defenderO", "defenderDir")

### Keep only columns that will be useful when merged to ball carrier tracking data later ###
trackingLinebackers <- trackingLinebackers %>%
  dplyr::select(gameId, playId, frameId, defenderId, defenderDisplayName, defenderX, defenderY, defenderDir, defenderO, club, defenderS, a, dis, event, tackle, assist, pff_missedTackle, weekIdentifier)
trackingLinebackers <- replace(trackingLinebackers, is.na(trackingLinebackers), 0)

### Adding ball carrier metrics to defender tracking data and calculating Eucledian distance of defender to ball carrier ###
trackingPlays <- trackingLinebackers %>%
  left_join(trackingBallCarrier, by = c("gameId", "playId", "frameId")) %>%
  mutate(distToBallCarrier = sqrt((defenderX - ballCarrierX)^2 + (defenderY - ballCarrierY)^2))

### Adding in tackle attempt metric ###
### if the player made a tackle or first contact then this is a tackle attempt ###
### if player made or assisted tackle then this is tackle made ###
trackingPlays <- trackingPlays %>%
  mutate(tackleAttempt = ifelse(event == "tackle" & (tackle == 1 | assist == 1 | pff_missedTackle == 1), 1, 0),
         tackleMade = ifelse(event == "tackle" & (tackle == 1 | assist == 1), 1, 0))

### dropping last 4 frames after tackle ###
### since these frames are after the play ends, they don't need to be included. Excluded so they don't mess with the following models
tackleFrame <- trackingPlays %>%
  filter(event == "tackle") %>%
  dplyr::select(gameId, playId, frameId) %>%
  distinct()
colnames(tackleFrame)[3] <- "tackleFrame"

trackingPlays <- trackingPlays %>%
  inner_join(tackleFrame, by = c("gameId", "playId")) %>%
  group_by(gameId, playId, defenderId) %>%
  filter(frameId <= tackleFrame) %>%
  ungroup()

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

### defining tackle opportunity
trackingPlays <- trackingPlays %>%
  mutate(tackleOppor = ifelse(dotProd > 0 & distToBallCarrier < 2 & distDiffInd == 1, 1, 0))

### splitting into training and testing data sets based on weeks of play ###
dfTrain <- trackingPlays %>%
  filter(weekIdentifier == "week 1" | weekIdentifier == "week 2" | weekIdentifier == "week 3" | weekIdentifier == "week 4" | weekIdentifier == "week 5" | weekIdentifier == "week 6")
dfTest <- trackingPlays %>%
  filter(weekIdentifier == "week 7" | weekIdentifier == "week 8" | weekIdentifier == "week 9")

###########################################
# Probability of tackle opportunity model #
###########################################

### Variables to use in tackle opportunity model ###
dfTrainTackleOppor <- dfTrain %>%
  dplyr::select(defenderX, defenderY, defenderS, a, defenderDirRads, tackleOppor, distToBallCarrier) %>%
  mutate(isTackleOppor = ifelse(tackleOppor == 1, "yes", "no")) %>%
  dplyr::select(-tackleOppor)
dfTrainTackleOppor$isTackleOppor  <- as.factor(dfTrainTackleOppor$isTackleOppor)

dfTestTackleOppor <- dfTest %>%
  dplyr::select(defenderX, defenderY, defenderS, a, defenderDirRads, tackleOppor, distToBallCarrier) %>%
  mutate(isTackleOppor = ifelse(tackleOppor == 1, "yes", "no")) %>%
  dplyr::select(-tackleOppor)

### Cross Validation ###
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 2,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)
ctrl$sampling <- "up"

### Gradient Boosting Machine model to calculate tackle opportunity probability ###
mod<- train(isTackleOppor ~ .,
            data = dfTrainTackleOppor,
            method = "gbm",
            verbose = FALSE,
            metric = "ROC",
            trControl = ctrl)

testRoc <- function(model, data) {
  
  roc(data$isTackleOppor,
      predict(model, data, type = "prob")[,1])
  
}

mod %>%
  testRoc(data = dfTrainTackleOppor) %>%
  auc()

predsTrain <- predict(mod, dfTrainTackleOppor, type = 'prob')
colnames(predsTrain) = c("noOpportunity", "yesOpportunity")
dfTrain <- cbind(dfTrain, predsTrain)
predsTest <- predict(mod, dfTestTackleOppor, type = 'prob')
colnames(predsTest) = c("noOpportunity", "yesOpportunity")
dfTest <- cbind(dfTest, predsTest)

################################################################
# Probability of tackle attempt given tackle opportunity model #
################################################################

### Variables to use in tackle attempt model ###
dfTrainTackleAttempt <- dfTrain %>%
  dplyr::select(defenderX, defenderY, defenderS, a, defenderDirRads, tackleAttempt, yesOpportunity, distToBallCarrier) %>%
  mutate(isTackleAttempt = ifelse(tackleAttempt == 1, "yes", "no")) %>%
  dplyr::select(-tackleAttempt)
dfTrainTackleAttempt$isTackleAttempt  <- as.factor(dfTrainTackleAttempt$isTackleAttempt)

dfTestTackleAttempt <- dfTest %>%
  dplyr::select(defenderX, defenderY, defenderS, a, defenderDirRads, tackleAttempt, yesOpportunity, distToBallCarrier) %>%
  mutate(isTackleAttempt = ifelse(tackleAttempt == 1, "yes", "no")) %>%
  dplyr::select(-tackleAttempt)

### Gradient Boosting Machine model to calculate tackle attempt probability ###
mod1<- train(isTackleAttempt ~ .,
            data = dfTrainTackleAttempt,
            method = "gbm",
            verbose = FALSE,
            metric = "ROC",
            trControl = ctrl)

testRoc1 <- function(model, data) {
  
  roc(data$isTackleAttempt,
      predict(model, data, type = "prob")[,1])
  
}

mod1 %>%
  testRoc1(data = dfTrainTackleAttempt) %>%
  auc()

predsTrain1 <- predict(mod1, dfTrainTackleAttempt, type = 'prob')
colnames(predsTrain1) = c("noAttempt", "yesAttempt")
dfTrain <- cbind(dfTrain, predsTrain1)
predsTest1 <- predict(mod1, dfTestTackleAttempt, type = 'prob')
colnames(predsTest1) = c("noAttempt", "yesAttempt")
dfTest <- cbind(dfTest, predsTest1)


################################################################################
# Probability of made tackle given tackle attempt and tackle opportunity model #
################################################################################

### Variables to use in tackle made given tackle attempt and tackle opportunity model ###
dfTrainTackle <- dfTrain %>%
  dplyr::select(defenderX, defenderY, defenderS, a, defenderDirRads, tackleMade, distToBallCarrier, yesAttempt, yesOpportunity) %>%
  mutate(isTackle = ifelse(tackleMade == 1, "yes", "no")) %>%
  dplyr::select(-tackleMade)
dfTrainTackle$isTackle  <- as.factor(dfTrainTackle$isTackle)

dfTestTackle <- dfTest %>%
  dplyr::select(defenderX, defenderY, defenderS, a, defenderDirRads, tackleMade, distToBallCarrier, yesAttempt, yesOpportunity) %>%
  mutate(isTackle = ifelse(tackleMade == 1, "yes", "no")) %>%
  dplyr::select(-tackleMade)

### Gradient Boosting Machine model to calculate tackle attempt and made tackle probability ###
mod2 <- train(isTackle ~ .,
              data = dfTrainTackle,
              method = "gbm",
              verbose = FALSE,
              metric = "ROC",
              trControl = ctrl)

testRoc2 <- function(model, data) {
  
  roc(data$isTackle,
      predict(model, data, type = "prob")[,1])
  
}

mod2 %>%
  testRoc2(data = dfTrainTackle) %>%
  auc()

predsTrain2 <- predict(mod2, dfTrainTackle, type = 'prob')
colnames(predsTrain2) = c("noTackle", "yesTackle")
dfTrain <- cbind(dfTrain, predsTrain2)
predsTest2 <- predict(mod2, dfTestTackle, type = 'prob')
colnames(predsTest2) = c("noTackle", "yesTackle")
dfTest <- cbind(dfTest, predsTest2)

df <- rbind(dfTrain, dfTest)

#################
# data analysis #
#################

### ball snap happens on frame 6 always so we want 2 frames after the snap probabilities
snapFrame <- df %>%
  group_by(gameId, playId, defenderId) %>%
  filter(frameId == 8)%>%
  dplyr::select(gameId, playId, defenderDisplayName, defenderId, frameId, yesOpportunity, yesAttempt, yesTackle) 

### updating column names because some column names are the same in data sets that will be merged to it later on
colnames(snapFrame) <- c("gameId", "playId", "defenderDisplayName", "defenderId", "snapFrameId", "opporSnap", "attemptSnap", "tackleSnap")

### dropping frames before snap and finding max tackle opportunity probability for each linebacker after snap
opporProb <- df %>%
  group_by(gameId, playId, defenderDisplayName) %>%
  filter(frameId > 8) %>%
  summarise(maxOpportunity = max(yesOpportunity)) %>%
  ungroup() 

### dropping frames before snap and finding max tackle attempt probability for each linebacker after snap
attemptProb <- df %>%
  group_by(gameId, playId, defenderDisplayName) %>%
  filter(frameId > 8) %>%
  summarise(maxAttempt = max(yesAttempt)) %>%
  ungroup() 

### dropping frames before snap and finding max tackle probability for each linebacker after snap
tackleProb <- df %>%
  group_by(gameId, playId, defenderDisplayName) %>%
  filter(frameId > 8) %>%
  summarise(maxTackle = max(yesTackle)) %>%
  ungroup() 

### finding total number of snaps for each player because players with less than 30 snaps will be removed
totalSnaps <- df %>%
  dplyr::select(gameId, playId, defenderDisplayName) %>%
  distinct() %>%
  group_by(defenderDisplayName) %>%
  summarise(totalSnaps = n())

### calculating difference between probabilty from snap to highest probability post snap
### finding percentile and giving players ranks/grades based on percentile 
probsDf <- opporProb %>%
  inner_join(attemptProb, by = c("gameId", "playId", "defenderDisplayName")) %>%
  inner_join(tackleProb, by = c("gameId", "playId", "defenderDisplayName")) %>%
  inner_join(snapFrame, by = c("gameId", "playId", "defenderDisplayName")) %>%
  mutate(opporDelta = maxOpportunity - opporSnap,
         attemptDelta = maxAttempt - attemptSnap,
         tackleDelta = maxTackle - tackleSnap) %>%
  group_by(defenderDisplayName) %>%
  summarise(avgOppor = mean(opporDelta),
            avgAttempt = mean(attemptDelta),
            avgTackle = mean(tackleDelta)) %>%
  inner_join(totalSnaps, by = "defenderDisplayName") %>%
  filter(totalSnaps >= 30) %>%
  mutate(opportunityPercentile = round(percent_rank(avgOppor)*100, 0),
         attemptPercentile = round(percent_rank(avgAttempt)*100, 0),
         tacklePercentile = round(percent_rank(avgTackle)*100, 0)) %>%
  mutate(tackleGrade = case_when(
    tacklePercentile > 90 & tacklePercentile <= 100 ~ 1,
    tacklePercentile > 80 & tacklePercentile <= 90 ~ 2,
    tacklePercentile > 70 & tacklePercentile <= 80 ~ 3,
    tacklePercentile > 60 & tacklePercentile <= 70 ~ 4,
    .default = 5), 
    tackleAttemptGrade = case_when(
      attemptPercentile > 90 & attemptPercentile <= 100 ~ 1,
      attemptPercentile > 80 & attemptPercentile <= 90 ~ 2,
      attemptPercentile > 70 & attemptPercentile <= 80 ~ 3,
      attemptPercentile > 60 & attemptPercentile <= 70 ~ 4,
      .default = 5
    ),
    opportunityGrade = case_when(
      opportunityPercentile > 90 & opportunityPercentile <= 100 ~ 1,
      opportunityPercentile > 80 & opportunityPercentile <= 90 ~ 2,
      opportunityPercentile > 70 & opportunityPercentile <= 80 ~ 3,
      opportunityPercentile > 60 & opportunityPercentile <= 70 ~ 4,
      .default = 5
    ))

### define tackle opportunity as being within 2 yards and facing the ball carrier
### must see if ball carrier is in front or behind defender (if ball carriers x position is less than defenders ball carrier is in front)
### define what facing the ball carrier is 
### calculate which players had low tackle probability and still made it within 5 yards of ball carrier at the time of tackle


## average distance to ball carrier at time of tackle is 1.061882 yards