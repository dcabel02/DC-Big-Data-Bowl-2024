library(formattable)
library(dplyr)
library(nflfastR)
library(kableExtra)

## all results were exported from bdb-YardsAfterContact.R bdb-tackleGivenContact.R and bdb-HustleMetric.R files and loaded here
averageTackle <- read.csv("C:/Users/danie/Downloads/averageTackle.csv", header = TRUE)
yardsAfterContact <- read.csv("C:/Users/danie/Downloads/yardsAfterContact.csv", header = TRUE)
hustleMetric <- read.csv("C:/Users/danie/Downloads/hustleDf2.csv", header = TRUE)
players <- read.csv("C:/Users/danie/Documents/NFL Big Data Bowl/Data/players.csv", header = TRUE)

## joining data sets and omiting NA values because those players were omitted from other data sets due to low snap counts
df <- averageTackle %>%
  full_join(yardsAfterContact, by = "defenderDisplayName") %>%
  full_join(hustleMetric, by = "defenderDisplayName")
df <- na.omit(df)

## calculating final grades and ranks for each player
df <- df %>%
  mutate(percentileTotal = tacklePercentile + yacPercentile + hustlePercentile + opportunityPercentile + attemptPercentile,
         overallPercentile = round(percent_rank(percentileTotal)*100, 0))

## ordering data set by overall percentile rank
df <- df %>%
  arrange(-overallPercentile)

## adjusting tackling probabilities for number of snaps
## a player that has played lots of meaningful snaps should be rewarded and weighted higher
## calculating new percentiles, rankings, and grades based on weighted categories
test <- df %>%
  mutate(adjOppor = avgOppor*(totalSnaps/196),
         adjAttempt = avgAttempt*(totalSnaps/196),
         adjTackle = avgTackle*(totalSnaps/196)) %>%
  mutate(tacklePercentileAdj = round(percent_rank(adjTackle)*100, 0),
         attemptPercentileAdj = round(percent_rank(adjAttempt)*100, 0),
         opportunityPercentileAdj = round(percent_rank(adjOppor)*100, 0)) %>%
  mutate(tackleRank = rank(-tacklePercentileAdj),
         attemptRank = rank(-attemptPercentileAdj),
         opportunityRank = rank(-opportunityPercentileAdj),
         yacRank = rank(-yacPercentile),
         hustleRank = rank(-hustlePercentile)) %>%
  mutate(percentileTotalAdj = tacklePercentileAdj + yacPercentile + hustlePercentile + opportunityPercentileAdj + attemptPercentileAdj,
         overallPercentileAdj = round(percent_rank(percentileTotalAdj)*100, 0), 
         overallRank = rank(-overallPercentileAdj)) %>%
  arrange(-overallPercentileAdj)

## grabbing head shots for table
pics = fast_scraper_roster(2022) %>%
  dplyr::select(full_name, team, pff_id, headshot_url) 

## getting data ready for table
tab <- test %>%
  dplyr::select(defenderDisplayName, overallRank, opportunityRank, attemptRank, tackleRank, averageYAC, percentPlaysWithin3) %>%
  left_join(pics, by = join_by("defenderDisplayName" == "full_name")) %>%
  mutate (` ` = "") %>% 
  arrange(overallRank) %>%
  dplyr::select(` `, defenderDisplayName, team, overallRank, opportunityRank, attemptRank, tackleRank, averageYAC, percentPlaysWithin3)

colnames(tab) <- c(" ", "Player Name", "Team", "Overall Rank", "Opportunity", "Attempt", "Tackle", "Average YAC", "Rally to Ball")

tab$`Average YAC` = color_tile("transparent", "red")(tab$`Average YAC`)
tab$`Rally to Ball` = color_tile("green", "transparent")(tab$`Rally to Ball`)

## selecting top 24 players
player_pics = pics %>%
  filter(full_name %in% c("Rashaan Evans", "C.J. Mosley", "Ja'Whaun Bentley", "Roquan Smith", "Bobby Okereke", "Foyesade Oluokun", 
                          "Fred Warner", "Devin Lloyd", "Shaq Thompson", "Quay Walker", "Elandon Roberts", "Bobby Wagner", "Myles Jack", 
                          "Ernest Jones", "De'Vondre Campbell", "Jordyn Brooks", "Lavonte David", "Nick Bolton", "Demario Davis", "Mykal Walker", 
                          "Patrick Queen", "Devin White", "Cole Holcomb", "Alex Anzalone")) %>%
  inner_join(tab, by = c("full_name" = "Player Name")) %>%
  arrange(`Overall Rank`) %>%
  dplyr::select(full_name,headshot_url)

## creating table with top 12 linebackers
tab[c(1:12),] %>% 
  kable(caption = "2022 Tackling Talent Ratings Weeks 1-9",booktabs = T, align="c", escape = F) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, image = spec_image(player_pics$headshot_url[c(1:12)], 175,125)) %>%
  column_spec(2, bold = T, width = "5cm") %>%
  column_spec(3, width = "5cm", bold = T, border_right = "2px solid gray") %>%
  column_spec(4, bold = T, width = "3cm", border_right = "2px solid gray") %>%
  column_spec(5, width = "3cm", bold = T) %>%
  column_spec(6, width = "3cm", bold = T) %>%
  column_spec(7, width = "2.3cm", bold = T, border_right = "2px solid gray") %>%
  column_spec(8, width = "3cm", bold = T, border_right = "2px solid gray") %>%
  column_spec(9, width = "3cm", bold = T) %>%
  save_kable("C:/Users/danie/Documents/NFL Big Data Bowl/table1.png")

## creating table with top 13-24 linebackers
tab[c(13:24),] %>% 
  kable(caption = "2022 Tackling Talent Ratings Weeks 1-9",booktabs = T, align="c", escape = F) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, image = spec_image(player_pics$headshot_url[c(13:24)], 175,125)) %>%
  column_spec(2, bold = T, width = "5cm") %>%
  column_spec(3, width = "5cm", bold = T) %>%
  column_spec(4, bold = T, width = "3cm", border_right = "2px solid gray") %>%
  column_spec(5, width = "3cm", bold = T, border_right = "2px solid gray") %>%
  column_spec(6, width = "3cm", bold = T) %>%
  column_spec(7, width = "2.3cm", bold = T) %>%
  column_spec(8, width = "3cm", bold = T, border_right = "2px solid gray") %>%
  column_spec(9, width = "3cm", bold = T, border_right = "2px solid gray") %>%
  save_kable("C:/Users/danie/Documents/NFL Big Data Bowl/table2.png")

## creating data set for top 3 players that had lower snap count but were in the top 24 before snap count adjustment
tab <- test %>%
  dplyr::select(defenderDisplayName, overallRank, opportunityRank, attemptRank, tackleRank, averageYAC, percentPlaysWithin3) %>%
  filter(defenderDisplayName %in% c("Dylan Cole", "Denzel Perryman", "Damien Wilson")) %>%
  left_join(pics, by = join_by("defenderDisplayName" == "full_name")) %>%
  mutate (` ` = "") %>% 
  arrange(overallRank) %>%
  dplyr::select(` `, defenderDisplayName, team, overallRank, opportunityRank, attemptRank, tackleRank, averageYAC, percentPlaysWithin3)

colnames(tab) <- c(" ", "Player Name", "Team", "Overall Rank", "Opportunity", "Attempt", "Tackle", "Average YAC", "Rally to Ball")


tab$`Average YAC` = color_tile("transparent", "red")(tab$`Average YAC`)
tab$`Rally to Ball` = color_tile("green", "transparent")(tab$`Rally to Ball`)

player_pics = pics %>%
  filter(full_name %in% c("Dylan Cole", "Denzel Perryman", "Damien Wilson")) %>%
  inner_join(tab, by = c("full_name" = "Player Name")) %>%
  arrange(`Overall Rank`) %>%
  dplyr::select(full_name,headshot_url)

tab %>% 
  kable(caption = "2022 Tackling Talent Ratings Weeks 1-9",booktabs = T, align="c", escape = F) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, image = spec_image(player_pics$headshot_url[c(1:3)], 175,125)) %>%
  column_spec(2, bold = T, width = "5cm") %>%
  column_spec(3, width = "5cm", bold = T, border_right = "2px solid gray") %>%
  column_spec(4, bold = T, width = "3cm", border_right = "2px solid gray") %>%
  column_spec(5, width = "3cm", bold = T) %>%
  column_spec(6, width = "3cm", bold = T) %>%
  column_spec(7, width = "2.3cm", bold = T, border_right = "2px solid gray") %>%
  column_spec(8, width = "3cm", bold = T, border_right = "2px solid gray") %>%
  column_spec(9, width = "3cm", bold = T) %>%
  save_kable("C:/Users/danie/Documents/NFL Big Data Bowl/table3.png")
