### Load Libraries ###
library(ggplot2)
library(dplyr)
library(REdaS)
library(lubridate)
library(caret)
library(pROC)

### Load Data ###
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

### Combine Tracking Data for Weeks 1-2 as Proof of Concept ###
tracking <- rbind(week1, week2)

### Standardizing Position Variables ###
tracking_new <- tracking %>%
  mutate(x_adj = ifelse(playDirection == "left", 120-x, x)) %>%
  mutate(y_adj = ifelse(playDirection == "left", 160/3-y, y)) %>%
  mutate(dir_adj = ifelse(playDirection == "left", 180+dir, dir)%%360) %>%
  mutate(o_adj = ifelse(playDirection == "left", 180+o, o)%%360) %>%
  select(-c(x, y, dir, o))

### Isolating Plays with Tackles Made By ILB or MLB ###
linebackers <- players %>%
  filter(position == "MLB" | position == "ILB") %>%
  inner_join(tackles, by = "nflId") %>%
  inner_join(plays, by = c("gameId", "playId"))

### Isolating all RB and WR ###
runningbacks <- players %>%
  filter(position == "RB" | position == "WR")

### Isolating Plays Where QB runs the ball ###
qbRun <- linebackers %>%
  filter(passResult == "R")

### Isolating Plays Where a RB or WR Runs the Ball ###
runningbackRun <- linebackers %>%
  filter(ballCarrierId %in% runningbacks$nflId) %>%
  filter(passResult != "C")

### All Run Plays with Tackles Made By ILB or MLB ###
runPlays <- rbind(qbRun, runningbackRun)

### Update Column Names ###
colnames(runPlays)[c(1,7)] <- c("tackler", "tacklerName")

### Adds Tracking Data Excluding Offensive Team and Football ###
tracking_runPlays_defenders <- tracking_new %>%
  inner_join(runPlays, by = c("gameId", "playId")) %>%
  filter(displayName != "football") %>%
  filter(club == defensiveTeam)

### Adds Tracking Data for Ball Carrier ###
tracking_runPlays_ballCarrier <- tracking_new %>%
  inner_join(runPlays, by = c("gameId", "playId")) %>%
  filter(ballCarrierId == nflId) 

### Update Column Names ###
colnames(tracking_runPlays_ballCarrier)[c(14,15)] <- c("ballCarrier_xAdj", "ballCarrier_yAdj")

### Keep Only Necessary Columns ###
tracking_runPlays_ballCarrier <- tracking_runPlays_ballCarrier %>%
  select(gameId, playId, frameId, ballCarrier_xAdj, ballCarrier_yAdj)

### Adding Ball Carrier Position to Defender Tracking Data and Calculating Eucledian Distance of Defender to Ball Carrier ###
tracking_runPlays <- tracking_runPlays_defenders %>%
  left_join(tracking_runPlays_ballCarrier, by = c("gameId", "playId", "frameId")) %>%
  mutate(distToBallCarrier = sqrt((x_adj - ballCarrier_xAdj)^2 + (y_adj - ballCarrier_yAdj)^2))

### Spliting Data into 80/20 Training and Testing Data Sets ###
sampledf <- sample(c(TRUE, FALSE), nrow(tracking_runPlays), replace=TRUE, prob=c(0.8,0.2))
train <- tracking_runPlays[sampledf, ]
test <- tracking_runPlays[!sampledf, ]

### Variables to Use in Tackle Probability Model ###
train <- train %>%
  select(x_adj, y_adj, s, a, dir_adj, event, distToBallCarrier) %>%
  mutate(is_tackle = ifelse(event == 'tackle', "yes", "no")) %>%
  select(-event)
train <- replace(train, is.na(train), "no")
train$is_tackle  <- as.factor(train$is_tackle)

### Cross Validation ###
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 2,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)
ctrl$sampling <- "up"

### Gradient Boosting Machine Model to Calculate Tackle Probability ###
mod<- train(is_tackle ~ .,
            data = train,
            method = "gbm",
            verbose = FALSE,
            metric = "ROC",
            trControl = ctrl)

test_roc <- function(model, data) {
  
  roc(data$is_tackle,
      predict(model, data, type = "prob")[,1])
  
}

mod %>%
  test_roc(data = train) %>%
  auc()

preds <- predict(mod, train, type = 'prob')
preds_test <- predict(mod, test, type = 'prob')
