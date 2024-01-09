library(gganimate)
library(cowplot)
library(repr)
library(gifski)
library(ggpmisc)

### animation of play proof of concept ###
### code from https://www.kaggle.com/code/tombliss/pass-rushing-edge-get-off-speed ###


source("https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R")

tackleProbs <- read.csv("C:/Users/danie/Downloads/tackleProbabilities.csv", header = TRUE)
tackleProbs <- tackleProbs %>%
  filter(gameId == 2022090800 & playId == 80 & defenderId == 53532) %>%
  dplyr::select(gameId, playId, frameId, defenderId, yesOpportunity, yesAttempt, yesTackle)
colnames(tackleProbs)[4] <- "nflId"

tackleProbs$yesAttempt <- round(tackleProbs$yesAttempt, 2)
tackleProbs$yesOpportunity <- round(tackleProbs$yesOpportunity, 2)
tackleProbs$yesTackle <- round(tackleProbs$yesTackle, 2)

games <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/games.csv", header = TRUE)
players <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/players.csv", header = TRUE)
plays <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/plays.csv", header = TRUE)
tackles <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/tackles.csv", header = TRUE)
week1 <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/tracking_week_1.csv", header = TRUE)

tracking <- week1 

trackingNew <- tracking %>%
  mutate(xAdj = ifelse(playDirection == "left", 120-x, x)) %>%
  mutate(yAdj = ifelse(playDirection == "left", 160/3-y, y)) %>%
  mutate(dirAdj = ifelse(playDirection == "left", 180+dir, dir)%%360) %>%
  mutate(oAdj = ifelse(playDirection == "left", 180+o, o)%%360) %>%
  mutate(gamePlayId = paste0(as.character(gameId), as.character(playId), sep = "")) %>%
  dplyr::select(-c(x, y, dir, o)) %>%
  filter(gameId == 2022090800 & playId == 80)

df <- trackingNew %>%
  left_join(tackleProbs, by = c("gameId", "playId", "frameId", "nflId"))
df <- replace(df, is.na(df), 0)

df <- df %>%
  mutate(clubColor = case_when(
    displayName == "Ernest Jones" ~ "color_yellow",
    displayName == "Josh Allen" ~ "color_blue",
    displayName == "football" ~ "color_brown",
    .default = "color_grey"
  ))

examplePlay <- plays %>%
  dplyr::select(gameId, playId, playDescription) %>%
  filter(gameId == 2022090800 & playId == 80) 

#merging tracking data to play
examplePlayTracking <- inner_join(examplePlay,
                                  df,
                                  by = c("gameId" = "gameId",
                                         "playId" = "playId"))

#attributes used for plot. first is away, second is home, third is football.
# depending on play colors get messed up sometimes 
cols_fill <- c("yellow2", "#663300", "dodgerblue1")
cols_fill <- c("dodgerblue1", "#663300", "gray", "yellow2")
cols_col <- c("#000000", "#663300", "#000000")
cols_col <- c("#000000", "#663300", "#000000", "#000000")
size_vals <- c(8, 4, 8, 8)
shape_vals <- c(21, 16, 21, 21)
plot_title <- examplePlay$playDescription
nFrames <- max(examplePlayTracking$frameId)

#plotting
anim <- ggplot() +
  
  
  #creating field underlay
  gg_field(yardmin = 0, yardmax = 75) +
  
  #filling forest green for behind back of endzone
  theme(panel.background = element_rect(fill = "forestgreen",
                                        color = "forestgreen"),
        panel.grid = element_blank()) +
  #setting size and color parameters
  scale_size_manual(values = size_vals, guide = FALSE) + 
  scale_shape_manual(values = shape_vals, guide = FALSE) +
  scale_fill_manual(values = cols_fill, guide = FALSE) + 
  scale_colour_manual(values = cols_col, guide = FALSE) +
  
  
  #adding players
  geom_point(data = examplePlayTracking, aes(x = xAdj,
                                             y = yAdj, 
                                             shape = clubColor,
                                             fill = clubColor,
                                             group = nflId,
                                             size = clubColor,
                                             colour = clubColor), 
             alpha = 0.7) + 
  #adding jersey numbers
  geom_text(data = examplePlayTracking,
            aes(x = xAdj, y = yAdj, label = jerseyNumber),
            colour = "white", 
            vjust = 0.36, size = 3.5) + 
  
  geom_text(tackleProbs, mapping = aes(label=paste0("Tackle Opportunity = ", yesOpportunity), x= 25, y=4.5,)) +
  geom_text(tackleProbs, mapping = aes(label=paste0("Tackle Attempt Given Opportunity = ", yesAttempt), x= 25, y=3)) +
  geom_text(tackleProbs, mapping = aes(label=paste0("Tackle Made Given Attempt & Opportunity = ", yesTackle), x= 25, y=1.5)) +
  geom_text(mapping = aes(label = "Tackle Probabilities for Ernest Jones"), x = 25, y = 6) +
  
  
  #titling plot with play description
  labs(title = plot_title) +
  
  #setting animation parameters
  transition_time(frameId)  +
  ease_aes("linear") + 
  NULL 

#saving animation to display in markdown cell below:
anim_save("ExamplePlay.gif",
          animate(anim, width = 1000, height = 800,
                  fps = 5, nframes = nFrames, renderer = gifski_renderer()), path = "C:/Users/danie/Downloads/")
