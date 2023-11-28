library(gganimate)
library(cowplot)
library(repr)
library(gifski)

### animation of play proof of concept ###
### code from https://www.kaggle.com/code/tombliss/pass-rushing-edge-get-off-speed ###


source("https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R")

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
tracking <- rbind(week1, week2, week3, week4, week5, week6, week7, week8, week9)

### Standardizing position variables so all plays move in the same direction###
trackingNew <- tracking %>%
  mutate(xAdj = ifelse(playDirection == "left", 120-x, x)) %>%
  mutate(yAdj = ifelse(playDirection == "left", 160/3-y, y)) %>%
  mutate(dirAdj = ifelse(playDirection == "left", 180+dir, dir)%%360) %>%
  mutate(oAdj = ifelse(playDirection == "left", 180+o, o)%%360) %>%
  mutate(gamePlayId = paste0(as.character(gameId), as.character(playId), sep = "")) %>%
  dplyr::select(-c(x, y, dir, o))

examplePlay <- plays %>%
  select(gameId, playId, playDescription) %>%
  sample_n(1)

#merging tracking data to play
examplePlayTracking <- inner_join(examplePlay,
                                     trackingNew,
                                     by = c("gameId" = "gameId",
                                            "playId" = "playId")) 

#attributes used for plot. first is away, second is home, third is football.
# depending on play colors get messed up sometimes 
cols_fill <- c("dodgerblue1", "firebrick1", "#663300")
cols_col <- c("#000000", "#000000", "#663300")
size_vals <- c(6, 6, 4)
shape_vals <- c(21, 21, 16)
plot_title <- examplePlay$playDescription
nFrames <- max(examplePlayTracking$frameId)

#plotting
anim <- ggplot() +
  
  
  #creating field underlay
  gg_field(yardmin = 0, yardmax = 122) +
  
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
                                                shape = club,
                                                fill = club,
                                                group = nflId,
                                                size = club,
                                                colour = club), 
             alpha = 0.7) + 
  #adding jersey numbers
  geom_text(data = examplePlayTracking,
            aes(x = xAdj, y = yAdj, label = jerseyNumber),
            colour = "white", 
            vjust = 0.36, size = 3.5) + 
  
  
  #titling plot with play description
  labs(title = plot_title) +
  
  #setting animation parameters
  transition_time(frameId)  +
  ease_aes("linear") + 
  NULL 

#saving animation to display in markdown cell below:
anim_save("ExamplePlay.gif",
          animate(anim, width = 720, height = 440,
                  fps = 10, nframes = nFrames, renderer = gifski_renderer()), path = "C:/Users/danie/Downloads/")
