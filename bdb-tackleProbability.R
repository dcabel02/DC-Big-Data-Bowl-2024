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

### Combine tracking data for weeks 1-2 as proof of concept (model takes too long with all 9 weeks of data) ###
tracking <- rbind(week1, week2)

### Standardizing position variables so all plays move in the same direction###
trackingNew <- tracking %>%
  mutate(xAdj = ifelse(playDirection == "left", 120-x, x)) %>%
  mutate(yAdj = ifelse(playDirection == "left", 160/3-y, y)) %>%
  mutate(dirAdj = ifelse(playDirection == "left", 180+dir, dir)%%360) %>%
  mutate(oAdj = ifelse(playDirection == "left", 180+o, o)%%360) %>%
  mutate(gamePlayId = paste0(as.character(gameId), as.character(playId), sep = "")) %>%
  dplyr::select(-c(x, y, dir, o))

### Isolating plays with tackles made by ILB or MLB ###
linebackers <- players %>%
  filter(position == "MLB" | position == "ILB") %>%
  inner_join(tackles, by = "nflId") %>%
  inner_join(plays, by = c("gameId", "playId"))

### Isolating all RB and WR's ###
runningbacks <- players %>%
  filter(position == "RB" | position == "WR")

### Isolating plays where QB runs the ball ###
qbRun <- linebackers %>%
  filter(passResult == "R")

### Isolating plays where a RB or WR runs the ball ###
runningbackRun <- linebackers %>%
  filter(ballCarrierId %in% runningbacks$nflId) %>%
  filter(passResult != "C")

### Combine for all run plays with tackles by an ILB or MLB ###
runPlays <- rbind(qbRun, runningbackRun)

### When combined with the tracking data, there will be two nflId and DisplayName columns that should not be the same this will ensure the tackler nflId and displayName will be separate from the tracking player nflId and displayName ###
colnames(runPlays)[c(1,7)] <- c("tackler", "tacklerName")

### Adds tracking data excluding offensive team and football ###
trackingRunPlaysDefenders <- trackingNew %>%
  inner_join(runPlays, by = c("gameId", "playId")) %>%
  filter(displayName != "football") %>%
  filter(club == defensiveTeam)

### Adds tracking data for ball carrier ###
trackingRunPlaysBallCarrier <- trackingNew %>%
  inner_join(runPlays, by = c("gameId", "playId")) %>%
  filter(ballCarrierId == nflId) 

### Making ball carrier x and y coordinates column names easier to identify for when this is merged with other tracking data that will x and y coordinate columns for other players ###
colnames(trackingRunPlaysBallCarrier)[c(14,15)] <- c("ballCarrierX", "ballCarrierY")

### Keep only columns that will be used later on ###
trackingRunPlaysBallCarrier <- trackingRunPlaysBallCarrier %>%
  dplyr::select(gameId, playId, frameId, ballCarrierX, ballCarrierY)

### Adding Ball Carrier Position to Defender Tracking Data and Calculating Eucledian Distance of Defender to Ball Carrier ###
trackingRunPlays <- trackingRunPlaysDefenders %>%
  left_join(trackingRunPlaysBallCarrier, by = c("gameId", "playId", "frameId")) %>%
  mutate(distToBallCarrier = sqrt((xAdj - ballCarrierX)^2 + (yAdj - ballCarrierY)^2))

### Sampling from unique game/play combinations so the testing data is not contaminated with data from a play in the training data set ###
gamePlayId <- unique(trackingRunPlays$gamePlayId)
sampledf <- sample(c(TRUE, FALSE), length(gamePlayId), replace=TRUE, prob=c(0.8,0.2))
trainGames <- gamePlayId[sampledf]
testGames <- gamePlayId[!sampledf]
dfTrain <- trackingRunPlays %>%
  filter(gamePlayId %in% trainGames) 
dfTest <- trackingRunPlays %>%
  filter(gamePlayId %in% testGames)


###############################
# Probability of tackle model #
###############################


### Variables to use in tackle probability model ###
dfTrainTackleProb <- dfTrain %>%
  select(xAdj, yAdj, s, a, dirAdj, event, distToBallCarrier) %>%
  mutate(isTackle = ifelse(event == 'tackle', "yes", "no")) %>%
  select(-event)
dfTrainTackleProb <- replace(dfTrainTackleProb, is.na(dfTrainTackleProb), "no")
dfTrainTackleProb$isTackle  <- as.factor(dfTrainTackleProb$isTackle)

### Cross Validation ###
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 2,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)
ctrl$sampling <- "up"

### Gradient Boosting Machine model to calculate tackle probability ###
mod<- train(isTackle ~ .,
            data = dfTrainTackleProb,
            method = "gbm",
            verbose = FALSE,
            metric = "ROC",
            trControl = ctrl)

testRoc <- function(model, data) {
  
  roc(data$isTackle,
      predict(model, data, type = "prob")[,1])
  
}

mod %>%
  testRoc(data = dfTrainTackleProb) %>%
  auc()

preds <- predict(mod, dfTrainTackleProb, type = 'prob')
predsTest <- predict(mod, dfTest, type = 'prob')