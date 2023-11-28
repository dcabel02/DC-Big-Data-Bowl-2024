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
  filter(position == "MLB" | position == "ILB")

### Making linebacker tracking column names easier to identify for when this is merged with ball carrier tracking data that have the same column names ###
colnames(trackingLinebackers)[c(3, 4, 14:17)] <- c("linebackerId", "linebackerDisplayName", "linebackerX", "linebackerY", "linebackerDir", "linebackerO")

### Keep only columns that will be useful when merged to ball carrier tracking data later ###
trackingLinebackers <- trackingLinebackers %>%
  dplyr::select(gameId, playId, frameId, linebackerId, linebackerDisplayName, linebackerX, linebackerY, linebackerDir, linebackerO, club, s, a, dis, event)

### Adding ball carrier metrics to defender tracking data and calculating Eucledian distance of defender to ball carrier ###
trackingPlays <- trackingLinebackers %>%
  left_join(trackingBallCarrier, by = c("gameId", "playId", "frameId")) %>%
  mutate(distToBallCarrier = sqrt((linebackerX - ballCarrierX)^2 + (linebackerY - ballCarrierY)^2))

### tackle opportunity is considered if: ###
### the linebacker is within 3 yards of the ball carrier ###
### the linebackers dir and orientation are within 155 and 205 degrees of the ball carrier ###

trackingPlays <- trackingPlays %>%
  mutate(oLowerBound = ballCarrierO + 155, 
         oUpperBound = ballCarrierO + 205, 
         dirLowerBound = ballCarrierDir + 155, 
         dirUpperBound = ballCarrierDir + 205) %>%
  mutate(oLower = ifelse(oLowerBound >= 360, oLowerBound - 360, oLowerBound),
         oUpper = ifelse(oUpperBound >= 360, oUpperBound - 360, oUpperBound), 
         dirLower = ifelse(dirLowerBound >= 360, dirLowerBound - 360, dirLowerBound), 
         dirUpper = ifelse(dirUpperBound >= 360, dirUpperBound - 360, dirUpperBound)) %>%
  dplyr::select(-c(oLowerBound, oUpperBound, dirLowerBound, dirUpperBound))

trackingPlays <- trackingPlays %>%
  mutate(tackleOpportunity = ifelse(distToBallCarrier > 3, 0, ifelse((linebackerDir <= 155 & linebackerDir >= 205), 0, ifelse((linebackerO <= 155 & linebackerO >= 205), 0, 1))))

### Sampling from unique game/play combinations so the testing data is not contaminated with data from a play in the training data set ###
trackingPlays <- trackingPlays %>%
  mutate(gamePlayId = paste0(as.character(gameId), as.character(playId), sep = ""))

gamePlayId <- unique(trackingPlays$gamePlayId)
sampledf <- sample(c(TRUE, FALSE), length(gamePlayId), replace=TRUE, prob=c(0.8,0.2))
trainGames <- gamePlayId[sampledf]
testGames <- gamePlayId[!sampledf]
dfTrain <- trackingPlays %>%
  filter(gamePlayId %in% trainGames) 
dfTest <- trackingPlays %>%
  filter(gamePlayId %in% testGames)

###########################################
# Probability of tackle opportunity model #
###########################################

### Variables to use in tackle opportunity model ###
dfTrainTackleOppor <- dfTrain %>%
  select(linebackerX, linebackerY, s, a, linebackerDir, tackleOpportunity, distToBallCarrier) %>%
  mutate(isTackleOpportunity = ifelse(tackleOpportunity == 1, "yes", "no")) %>%
  select(-tackleOpportunity)
dfTrainTackleOppor$isTackleOpportunity  <- as.factor(dfTrainTackleOppor$isTackleOpportunity)

### Cross Validation ###
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 2,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)
ctrl$sampling <- "up"

### Gradient Boosting Machine model to calculate tackle probability ###
mod<- train(isTackleOpportunity ~ .,
            data = dfTrainTackleOppor,
            method = "gbm",
            verbose = FALSE,
            metric = "ROC",
            trControl = ctrl)

testRoc <- function(model, data) {
  
  roc(data$isTackleOpportunity,
      predict(model, data, type = "prob")[,1])
  
}

mod %>%
  testRoc(data = dfTrainTackleOppor) %>%
  auc()

preds <- predict(mod, dfTrainTackleOppor, type = 'prob')
predsTest <- predict(mod, dfTest, type = 'prob')
