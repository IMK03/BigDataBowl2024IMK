library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(nflfastR)
library(na.tools)
library(ggimage)
library(rlang)
library(nflreadr)
library(nflplotR)
library(ggimage)
library(gt)
library(gtExtras)

#Load in data
setwd("C:/Users/imksy/Downloads/nfl-big-data-bowl-2024")


#First the chart of tackle_efficiency vs expected points saved

#Second, individual vs assists tackles

#Thirdly, onsistency in tackling (all non TFL and FF and sack tackles) vs explosiveness in tackling (splash plays, onyl TFL and FF and sacks)

#Finally, Aggregate Data

# Read game data
game_data <- read_csv("games.csv")

# Read play data
play_data <- read_csv("plays.csv")

# Read player data
player_data <- read_csv("players.csv")

# Read tackles data
tackles_data <- read_csv("tackles.csv")

# Read tracking data (replace [week] with the actual week number)
tracking_data <- read_csv("tracking_week_1.csv")
tracking_data <- read_csv("tracking_week_2.csv")
tracking_data <- read_csv("tracking_week_3.csv")
tracking_data <- read_csv("tracking_week_4.csv")
tracking_data <- read_csv("tracking_week_5.csv")
tracking_data <- read_csv("tracking_week_6.csv")
tracking_data <- read_csv("tracking_week_7.csv")
tracking_data <- read_csv("tracking_week_8.csv")
tracking_data <- read_csv("tracking_week_9.csv")

# Filter for successful tackles and merge with play_data
successful_tackles <- tackles_data %>%
  left_join(play_data, by = c("gameId", "playId")) %>%
  left_join(player_data, by = "nflId")

# Calculate Tackler Efficiency for each player
tackle_efficiency <- successful_tackles %>%
  group_by(nflId, displayName, position) %>%
  summarise(
    missed_tackles = sum(pff_missedTackle),
    tackles_successful = sum(tackle),
    assists = sum(assist),
    total_tackles_attempted = (missed_tackles + tackles_successful),
    average_tackle_efficiency = ifelse(total_tackles_attempted > 0, tackles_successful / total_tackles_attempted, 0),
    average_expectedPointsSaved = sum(
      case_when(
        tackle == 1 & pff_missedTackle == 0 ~ expectedPointsAdded,
        tackle == 0 & pff_missedTackle == 1 ~ -expectedPointsAdded,
        TRUE ~ 0), na.rm = TRUE) / total_tackles_attempted
    , .groups = 'drop') %>%
  filter(total_tackles_attempted >= 20, position %in% c("ILB", "MLB"))  # Filter by position

# Calculate snap count for each player
snap_count_data <- tracking_data %>%
  filter(frameId == 1) %>%
  group_by(nflId) %>%
  summarise(snap_count = n())

# Join snap count data with tackle_efficiency
tackle_efficiency <- tackle_efficiency %>%
  left_join(snap_count_data, by = "nflId")

# Calculate medians
median_x <- median(tackle_efficiency$average_tackle_efficiency)
median_y <- median(-tackle_efficiency$average_expectedPointsSaved)

# Create a new variable for quadrant using medians
tackle_efficiency <- tackle_efficiency %>%
  mutate(quadrantA = case_when(
    average_tackle_efficiency >= median_x & -average_expectedPointsSaved >= median_y ~ "Efficient tackler, saves lots of points",
    average_tackle_efficiency < median_x & -average_expectedPointsSaved >= median_y ~ "Inefficient tackler, saves lots of points",
    average_tackle_efficiency >= median_x & -average_expectedPointsSaved < median_y ~ "Efficient tackler, doesn't save lots of points",
    average_tackle_efficiency < median_x & -average_expectedPointsSaved < median_y ~ "Inefficient tackler, doesn't save a lot of points",
    TRUE ~ NA_character_
  ))

# Calculate individual tackle frequency
individual_tackle_frequency <- tackle_efficiency %>%
  group_by(nflId, displayName, position, snap_count) %>%
  summarise(
    individual_tackle_frequency = sum(tackles_successful) / snap_count
  )

# Calculate assist tackle frequency
assist_tackle_frequency <- tackle_efficiency %>%
  group_by(nflId, displayName, position, snap_count) %>%
  summarise(
    assist_tackle_frequency = sum(assists) / snap_count
  )

# Combine individual and assist tackle frequencies
total_tackle_frequency <- left_join(individual_tackle_frequency, assist_tackle_frequency, by = c("nflId", "displayName", "position", "snap_count")) %>%
  mutate(total_tackle_frequency = individual_tackle_frequency + assist_tackle_frequency)
# Calculate median or mean for x and y axis
median_x <- median(total_tackle_frequency$individual_tackle_frequency, na.rm = TRUE)
median_y <- median(total_tackle_frequency$total_tackle_frequency, na.rm = TRUE)

# Create a new variable for quadrant
total_tackle_frequency <- total_tackle_frequency %>%
  mutate(quadrantB = case_when(
    individual_tackle_frequency >= median_x & total_tackle_frequency >= median_y ~ "Involved in a lot of tackles, often first to ball",
    individual_tackle_frequency < median_x & total_tackle_frequency >= median_y ~ "Involved in a lot of tackles, not often first to ball",
    individual_tackle_frequency >= median_x & total_tackle_frequency < median_y ~ "Involved in less tackles, often first to ball",
    individual_tackle_frequency < median_x & total_tackle_frequency < median_y ~ "Involved in less tackles, not often first to ball",
    TRUE ~ NA_character_
  ))

# Join Tackles data with Play data to get relevant information
tackles_with_play_info <- left_join(tackles_data, play_data, by = c("gameId", "playId"))

# Filter out plays where playNullifiedByPenalty is Y, and where penaltyYards is not NA
tackles_with_play_info <- tackles_with_play_info %>%
  filter((playNullifiedByPenalty != "Y"))

tackles_with_play_info <- tackles_with_play_info %>%
  filter((is.na(penaltyYards)) | (penaltyYards == 0))

tackles_with_play_info <- tackles_with_play_info %>%
  filter((pff_missedTackle != 1))

tackles_with_play_info <- tackles_with_play_info %>%
  filter((forcedFumble != 1))

tackles_with_play_info <- tackles_with_play_info %>%
  filter((playResult >= 0))

tackles_with_play_info <- tackles_with_play_info %>%
  filter((passResult < 20 & !(is.na(passResult))) | (is.na(passResult) & playResult < 10))

# Join with Tracking data to get additional player tracking information
tackles_with_tracking_info <- left_join(tackles_with_play_info, tracking_data, by = c("gameId", "playId", "nflId"))

# Join with Player data to get player positions
tackles_with_positions <- left_join(tackles_with_tracking_info, player_data %>% select(nflId, position), by = "nflId")

# Join with Game data to map home and visiting teams
tackles_with_team_info <- left_join(tackles_with_positions, game_data %>% select(gameId, homeTeamAbbr, visitorTeamAbbr), by = "gameId") %>%
  mutate(
    defensiveTeam = ifelse(possessionTeam == homeTeamAbbr, visitorTeamAbbr, homeTeamAbbr),
    win_prob_added = ifelse(possessionTeam == homeTeamAbbr, visitorTeamWinProbilityAdded, homeTeamWinProbabilityAdded)
  )

# Filter for ILB or MLB players
filtered_tackles <- tackles_with_team_info %>%
  filter(position %in% c("ILB", "MLB"))



# Check if 'dis' column exists in filtered_tackles
if (!"dis" %in% colnames(filtered_tackles)) {
 stop("Required variable 'dis' not found in filtered_tackles.")
#}



# Calculate Nonexplosive Tackle Value for each play separately and remove duplicates
nonexplosive_Butkus_tackle_value <- filtered_tackles %>%
  group_by(gameId, playId, nflId, defensiveTeam, possessionTeam) %>%
  summarise(
    win_prob_added = last(win_prob_added),
    expected_points_added = last(expectedPointsAdded),
    yards_gained = last(playResult),
    tackle_range = sqrt((last(x) - first(x))^2 + (last(y) - first(y))^2),
    tackle_value = (0.15*win_prob_added) - (0.30*expected_points_added) - (0.40*yards_gained) + (0.15*tackle_range)
  ) 



# Normalize the nonexplosive_tackle_value column to a scale of -1 to 1
nonexplosive_Butkus_tackle_value$normalized_tackle_value <- scales::rescale(nonexplosive_Butkus_tackle_value$tackle_value, 
                                                                            to = c(-0.5, 0.5))

# View the results
head(nonexplosive_Butkus_tackle_value)



# Join Tackles data with Play data to get relevant information
explosive_tackles <- left_join(tackles_data, play_data, by = c("gameId", "playId"))

# Filter out plays where playNullifiedByPenalty is Y, and where penaltyYards is not NA
explosive_tackles <- explosive_tackles %>%
  filter((playNullifiedByPenalty != "Y"))

explosive_tackles <- explosive_tackles %>%
  filter((is.na(penaltyYards)) | (penaltyYards == 0))

explosive_tackles <- explosive_tackles %>%
  filter((pff_missedTackle != 1))

explosive_tackles <- explosive_tackles %>%
  filter((forcedFumble == 1) | (playResult < 0)) #plays where tackler forces a fumble or loss


# Join with Tracking data to get additional player tracking information
explosive_tackles_tracking <- left_join(explosive_tackles, tracking_data, by = c("gameId", "playId", "nflId"))

# Join with Player data to get player positions
explosive_tackles_players <- left_join(explosive_tackles_tracking, player_data %>% select(nflId, position), by = "nflId")

# Join with Game data to map home and visiting teams
explosive_tackles_teams <- left_join(explosive_tackles_players, game_data %>% select(gameId, homeTeamAbbr, visitorTeamAbbr), by = "gameId") %>%
  mutate(
    defensiveTeam = ifelse(possessionTeam == homeTeamAbbr, visitorTeamAbbr, homeTeamAbbr),
    win_prob_added = ifelse(possessionTeam == homeTeamAbbr, visitorTeamWinProbilityAdded, homeTeamWinProbabilityAdded)
  )

# Filter for ILB or MLB players
explosive_tackles_LB <- explosive_tackles_teams %>%
  filter(position %in% c("ILB", "MLB"))


# Check if 'dis' column exists in filtered_tackles
if (!"dis" %in% colnames(explosive_tackles_LB)) {
stop("Required variable 'dis' not found in filtered_tackles.")
#}

# Calculate Nonexplosive Tackle Value for each play separately and remove duplicates
explosive_Butkus_tackle_value <- explosive_tackles_LB %>%
  group_by(gameId, playId, nflId, defensiveTeam, possessionTeam) %>%
  summarise(
    win_prob_added = last(win_prob_added),
    expected_points_added = last(expectedPointsAdded),
    yards_gained = last(playResult),
    tackle_range = sqrt((last(x) - first(x))^2 + (last(y) - first(y))^2),
    tackle_value = (0.15*win_prob_added) - (0.30*expected_points_added) - (0.40*yards_gained) + (0.15*tackle_range)
  ) 

# Normalize the nonexplosive_tackle_value column to a scale of -1 to 1
explosive_Butkus_tackle_value$normalized_tackle_value <- scales::rescale(explosive_Butkus_tackle_value$tackle_value, 
                                                                         to = c(0.5, 1))

# Join Tackles data with Play data to get relevant information
offensive_explosive_plays <- left_join(tackles_data, play_data, by = c("gameId", "playId"))

# Filter out plays where playNullifiedByPenalty is Y, and where penaltyYards is not NA
offensive_explosive_plays <- offensive_explosive_plays %>%
  filter((playNullifiedByPenalty != "Y"))

offensive_explosive_plays <- offensive_explosive_plays %>%
  filter((is.na(penaltyYards)) | (penaltyYards == 0))

offensive_explosive_plays <- offensive_explosive_plays %>%
  filter((pff_missedTackle != 1))

offensive_explosive_plays <- offensive_explosive_plays %>%
  filter((forcedFumble != 1))

offensive_explosive_plays <- offensive_explosive_plays %>%
  filter((passLength >= 20) | (is.na(passLength)) & (playResult >= 10))


# Join with Tracking data to get additional player tracking information
offensive_explosive_players <- left_join(offensive_explosive_plays, tracking_data, by = c("gameId", "playId", "nflId"))

# Join with Player data to get player positions
offensive_explosive_positions <- left_join(offensive_explosive_players, player_data %>% select(nflId, position), by = "nflId")

# Join with Game data to map home and visiting teams
offensive_explosive_teams <- left_join(offensive_explosive_positions, game_data %>% select(gameId, homeTeamAbbr, visitorTeamAbbr), by = "gameId") %>%
  mutate(
    defensiveTeam = ifelse(possessionTeam == homeTeamAbbr, visitorTeamAbbr, homeTeamAbbr),
    win_prob_added = ifelse(possessionTeam == homeTeamAbbr, visitorTeamWinProbilityAdded, homeTeamWinProbabilityAdded)
  )

# Filter for ILB or MLB players
offensive_explosive_LBs <- offensive_explosive_teams %>%
  filter(position %in% c("ILB", "MLB"))


# Check if 'dis' column exists in filtered_tackles
if (!"dis" %in% colnames(offensive_explosive_LBs)) {
stop("Required variable 'dis' not found in filtered_tackles.")
#}

# Calculate Tackle Value for each play separately and remove duplicates where offense has explosive play
explosive_Butkus_tackle_value_bad <- offensive_explosive_LBs %>%
  group_by(gameId, playId, nflId, defensiveTeam, possessionTeam) %>%
  summarise(
    win_prob_added = last(win_prob_added),
    expected_points_added = last(expectedPointsAdded),
    yards_gained = last(playResult),
    tackle_range = sqrt((last(x) - first(x))^2 + (last(y) - first(y))^2),
    tackle_value = (0.15*win_prob_added) - (0.30*expected_points_added) - (0.40*yards_gained) + (0.15*tackle_range)
  ) 

# Normalize the nonexplosive_tackle_value column to a scale of -1 to 1
explosive_Butkus_tackle_value_bad$normalized_tackle_value <- scales::rescale(explosive_Butkus_tackle_value_bad$tackle_value, 
                                                                             to = c(-1, -0.5))                                                                             
                                                                             # Filter players with 20 or more tackles from nonexplosive_Butkus_tackle_value
                                                                             nonexplosive_tackle_by_player <- nonexplosive_Butkus_tackle_value %>%
                                                                               group_by(nflId) %>%
                                                                               summarise(
                                                                                 avg_nonexplosive_tackle_value = mean(tackle_value),
                                                                                 normalized_avg_nonexplosive_tackle_value = scales::rescale(mean(tackle_value)),
                                                                                 total_attempts = n()
                                                                               ) %>%
                                                                               left_join(player_data, by = "nflId") %>%
                                                                               left_join(tackle_efficiency, by = "nflId") %>%
                                                                               left_join(tackle_efficiency %>% filter(position %in% c("ILB", "MLB") & total_tackles_attempted >= 20), by = "nflId")
                                                                             
                                                                             
                                                                             nonexplosive_tackle_by_player <- inner_join(nonexplosive_tackle_by_player, tackle_efficiency, by = "nflId")
                                                                             
                                                                             # Join with existing nonexplosive_tackle_by_player (if it already exists)
                                                                             if (exists("nonexplosive_tackle_by_player")) {
                                                                             nonexplosive_tackle_by_player <- left_join(nonexplosive_tackle_by_player, nonexplosive_tackle_by_player, by = c("nflId" = "nflId"))
                                                                             #}
                                                                             
                                                                             # Calculate snap count for each player
                                                                             snap_count_data <- tracking_data %>%
                                                                              filter(frameId == 1) %>%
                                                                              group_by(nflId, displayName) %>%
                                                                              summarise(snap_count = n())
                                                                             
                                                                             # Join snap count data with nonexplosive_Butkus_tackle_value
                                                                             nonexplosive_tackle_with_snap_count <- left_join(nonexplosive_Butkus_tackle_value, snap_count_data, by = "nflId")
                                                                             
                                                                             # Calculate total nonexplosive tackles by player
                                                                             nonexplosive_tackle_by_player <- nonexplosive_tackle_with_snap_count %>%
                                                                               group_by(nflId, displayName, snap_count) %>%
                                                                               summarise(
                                                                                 avg_normalized_tackle_value = mean(avg_normalized_tackle_value, na.rm = TRUE),
                                                                                 total_nonexplosive_tackles = sum(!is.na(tackle_value & tackle_value >= 0)))
                                                                             
                                                                             # Calculate tackle frequency
                                                                             nonexplosive_tackle_by_player <- nonexplosive_tackle_by_player %>%
                                                                               mutate(tackle_frequency = total_nonexplosive_tackles / snap_count)
                                                                             
                                                                             # Assuming there's a common identifier, adjust the key accordingly
                                                                             common_key <- "nflId"
                                                                             # Inner join nonexplosive_tackle_by_player with the top 50 players from tackle_efficiency
                                                                             mapped_nonexplosive_tackle <- inner_join(
                                                                               nonexplosive_tackle_by_player,
                                                                               tackle_efficiency,
                                                                               by = common_key
                                                                             )
                                                                             
                                                                             # Calculate medians
                                                                             median_x3 <- median(mapped_nonexplosive_tackle$tackle_frequency)
                                                                             median_y3 <- median(mapped_nonexplosive_tackle$avg_normalized_tackle_value)
                                                                             
                                                                             # Create a new variable for quadrant using medians
                                                                             mapped_nonexplosive_tackle <- mapped_nonexplosive_tackle %>%
                                                                               mutate(quadrantC = case_when(
                                                                                 tackle_frequency >= median_x3 & avg_normalized_tackle_value >= median_y3 ~ "Make a lot of valuable nonexplosive tackles",
                                                                                 tackle_frequency < median_x3 & avg_normalized_tackle_value >= median_y3 ~ "Make little of valauable nonexplosive tackles",
                                                                                 tackle_frequency >= median_x3 & avg_normalized_tackle_value < median_y3 ~ "Make a lot of less valuable nonexplosive tackles",
                                                                                 tackle_frequency < median_x3 & avg_normalized_tackle_value < median_y3 ~ "Make little of less valuable nonexplosive tackles",
                                                                                 TRUE ~ NA_character_
                                                                               ))
                                                                             
                                                                             # Filter players with 20 or more tackles from explosive_Butkus_tackle_value
                                                                             explosive_tackle_by_player <- explosive_Butkus_tackle_value %>%
                                                                               group_by(nflId) %>%
                                                                               summarise(
                                                                                 avg_explosive_tackle_value = mean(tackle_value),
                                                                                 normalized_avg_explosive_tackle_value = scales::rescale(mean(tackle_value)),
                                                                                 total_attempts = n()
                                                                               ) %>%
                                                                               left_join(player_data, by = "nflId") %>%
                                                                               left_join(tackle_efficiency %>% filter(position %in% c("ILB", "MLB") & total_tackles_attempted >= 20), by = "nflId")
                                                                             
                                                                             # Calculate snap count for each player
                                                                             snap_count_data_explosive <- tracking_data %>%
                                                                             filter(frameId == 1) %>%
                                                                             group_by(nflId, displayName) %>%
                                                                             summarise(snap_count = n())
                                                                             
                                                                             # Join snap count data with explosive_Butkus_tackle_value
                                                                             explosive_tackle_with_snap_count <- left_join(explosive_Butkus_tackle_value, snap_count_data_explosive, by = "nflId")
                                                                             
                                                                             # Calculate total explosive tackles by player
                                                                             explosive_tackle_by_player <- explosive_tackle_with_snap_count %>%
                                                                               group_by(nflId, displayName, snap_count) %>%
                                                                               summarise(
                                                                                 avg_normalized_tackle_value_exp = mean(normalized_tackle_value, na.rm = TRUE),
                                                                                 total_explosive_tackles = sum(!is.na(tackle_value & tackle_value >= 0))) #TFL or FF
                                                                             
                                                                             explosive_tackle_by_player <- explosive_tackle_by_player %>%
                                                                               mutate(tackle_frequency_exp = total_explosive_tackles / snap_count)
                                                                             
                                                                             # Filter out rows with missing values
                                                                             explosive_tackle_by_player <- explosive_tackle_by_player %>%
                                                                               filter(!is.na(avg_normalized_tackle_value_exp) & !is.na(total_explosive_tackles))
                                                                             
                                                                             # Assuming there's a common identifier, adjust the key accordingly
                                                                             common_key <- "nflId"
                                                                             # Inner join explosive_tackle_by_player with the top 50 players from tackle_efficiency
                                                                             mapped_explosive_tackle <- inner_join(
                                                                               explosive_tackle_by_player,
                                                                               tackle_efficiency,
                                                                               by = common_key
                                                                             )
                                                                             
                                                                             
                                                                             # Calculate medians
                                                                             median_x2 <- median(mapped_explosive_tackle$tackle_frequency_exp, na.rm = TRUE)
                                                                             median_y2 <- median(mapped_explosive_tackle$avg_normalized_tackle_value_exp, na.rm = TRUE)
                                                                             
                                                                             # Create a new variable for quadrant using medians
                                                                             mapped_explosive_tackle <- mapped_explosive_tackle %>%
                                                                               mutate(quadrantD = case_when(
                                                                                 tackle_frequency_exp >= median_x2 & avg_normalized_tackle_value_exp >= median_y2 ~ "Many big tackles, more valuable tackles",
                                                                                 tackle_frequency_exp < median_x2 & avg_normalized_tackle_value_exp >= median_y2 ~ "Less big tackles, more valuable tackles",
                                                                                 tackle_frequency_exp >= median_x2 & avg_normalized_tackle_value_exp < median_y2 ~ "Many big tackles, less valuable tackles",
                                                                                 tackle_frequency_exp < median_x2 & avg_normalized_tackle_value_exp < median_y2 ~ "Less big tackles, less valuable tackles",
                                                                                 TRUE ~ NA_character_
                                                                               ))
                                                                             
                                                                             
                                                                             # Calculate snap count for each player
                                                                             snap_count_data_offensive_explosive <- tracking_data %>%
                                                                             filter(frameId == 1) %>%
                                                                             group_by(nflId, displayName) %>%
                                                                             summarise(snap_count = n())
                                                                             
                                                                             # Join snap count data with explosive_Butkus_tackle_value_bad
                                                                             explosive_tackle_with_snap_count_bad <- left_join(explosive_Butkus_tackle_value_bad, snap_count_data_offensive_explosive, by = "nflId")
                                                                             
                                                                             # Filter players with 20 or more tackles from explosive_Butkus_tackle_value_bad
                                                                             explosive_tackle_by_player_bad <- explosive_Butkus_tackle_value_bad %>%
                                                                               group_by(nflId) %>%
                                                                               summarise(
                                                                                 avg_normalized_tackle_value_bad = mean(normalized_tackle_value, na.rm = TRUE),
                                                                                 total_explosive_tackles_bad = sum(!is.na(tackle_value & tackle_value >= 0))
                                                                               ) %>%
                                                                               left_join(player_data, by = "nflId") %>%
                                                                               left_join(tackle_efficiency %>% filter(position %in% c("ILB", "MLB") & total_tackles_attempted >= 20 & !(is.na(total_tackles_attempted))), by = "nflId")
                                                                             explosive_tackle_by_player_bad <- inner_join(explosive_tackle_by_player_bad, tackle_efficiency, by = "nflId")
                                                                             
                                                                             # Calculate tackle frequency using the correct dataset
                                                                             explosive_tackle_by_player_bad <- explosive_tackle_by_player_bad %>%
                                                                               mutate(tackle_frequency_bad = total_explosive_tackles_bad / snap_count.y)
                                                                             
                                                                             # Calculate medians
                                                                             median_x1 <- median(explosive_tackle_by_player_bad$tackle_frequency_bad)
                                                                             median_y1 <- median(explosive_tackle_by_player_bad$avg_normalized_tackle_value_bad)
                                                                             
                                                                             # Create a new variable for quadrants
                                                                             explosive_tackle_by_player_bad <- explosive_tackle_by_player_bad %>%
                                                                               mutate(quadrantE = case_when(
                                                                                 tackle_frequency_bad >= median_x1 & avg_normalized_tackle_value_bad >= median_y1 ~ "High rate of tackles being on explosive plays, less explosive plays",
                                                                                 tackle_frequency_bad < median_x1 & avg_normalized_tackle_value_bad >= median_y1 ~ "Low rate of tackles being on explosive  plays, less explosive plays",
                                                                                 tackle_frequency_bad < median_x1 & avg_normalized_tackle_value_bad < median_y1 ~ "Low rate of tackles being on explosive plays, more explosive plays",
                                                                                 tackle_frequency_bad >= median_x1 & avg_normalized_tackle_value_bad < median_y1 ~ "High rate of tackles being on explosive plays, more explosive plays",
                                                                                 TRUE ~ "Undefined"
                                                                               ))
                                                                             
                                                                             common_key <- "nflId"
                                                                             
                                                                             
                                                                             # Combine datasets into aggregate_data
                                                                             aggregate_data <- left_join(tackle_efficiency, total_tackle_frequency, by = common_key) %>%
                                                                               left_join(mapped_explosive_tackle, by = common_key) %>%
                                                                               left_join(mapped_nonexplosive_tackle, by = common_key) %>%
                                                                               left_join(explosive_tackle_by_player_bad, by = common_key) %>% 
                                                                               select(
                                                                                 nflId,
                                                                                 displayName,
                                                                                 position,
                                                                                 missed_tackles,
                                                                                 tackles_successful,
                                                                                 assists,
                                                                                 total_tackles_attempted,
                                                                                 average_tackle_efficiency,
                                                                                 average_expectedPointsSaved,
                                                                                 quadrantA,
                                                                                 snap_count.y.x,
                                                                                 individual_tackle_frequency,
                                                                                 assist_tackle_frequency,
                                                                                 total_tackle_frequency,
                                                                                 quadrantB,
                                                                                 avg_normalized_tackle_value,
                                                                                 total_nonexplosive_tackles,
                                                                                 tackle_frequency,
                                                                                 quadrantC,
                                                                                 avg_normalized_tackle_value_exp,
                                                                                 total_explosive_tackles,
                                                                                 tackle_frequency_exp,
                                                                                 quadrantD,
                                                                                 avg_normalized_tackle_value_bad,
                                                                                 total_explosive_tackles_bad,
                                                                                 tackle_frequency_bad,
                                                                                 quadrantE
                                                                               )
                                                                             
                                                                             # View the structure of the aggregated dataset
                                                                             str(aggregate_data)
                                                                             
                                                                             # Assuming player_data has 'nflId', 'displayName', and 'position' columns
                                                                             common_player_key <- c("nflId", "displayName", "position")
                                                                             
                                                                             # Fill in NA values in aggregate_data using player_data
                                                                             aggregate_data <- aggregate_data %>%
                                                                               left_join(player_data %>% select(all_of(common_player_key)), by = "nflId")
                                                                             
                                                                             # If there are still NA values after the left join, you can use coalesce to prioritize non-NA values
                                                                             aggregate_data <- aggregate_data %>%
                                                                               mutate(
                                                                                 displayName = coalesce(displayName.x, displayName.y),
                                                                                 position = coalesce(position.x, position.y)
                                                                               ) %>%
                                                                               select(-starts_with("displayName."), -starts_with("position."))
                                                                             # Columns to normalize and include in the composite
                                                                             columns_to_normalize <- c(
                                                                               "average_tackle_efficiency",
                                                                               "average_expectedPointsSaved",
                                                                               "individual_tackle_frequency",
                                                                               "assist_tackle_frequency",
                                                                               "total_tackle_frequency",
                                                                               "avg_normalized_tackle_value",
                                                                               "tackle_frequency",
                                                                               "avg_normalized_tackle_value_exp",
                                                                               "tackle_frequency_exp",
                                                                               "avg_normalized_tackle_value_bad",
                                                                               "tackle_frequency_bad"
                                                                             )
                                                                             
                                                                             # Normalize specified columns
                                                                             normalized_data <- aggregate_data %>%
                                                                               mutate(across(all_of(columns_to_normalize), scales::rescale))
                                                                             
                                                                             # Define weights
                                                                             weights <- c(1,-1,1,1,1,-1,1,1,1,1,-1)
                                                                             
                                                                             # Calculate composite score
                                                                             normalized_data <- normalized_data %>%
                                                                               mutate(
                                                                                 normalized_added_composite = (
                                                                                   weights[1] * coalesce(average_tackle_efficiency, 0) +
                                                                                     weights[2] * coalesce(average_expectedPointsSaved, 0) +
                                                                                     weights[3] * coalesce(individual_tackle_frequency, 0) +
                                                                                     weights[4] * coalesce(assist_tackle_frequency, 0) +
                                                                                     weights[5] * coalesce(total_tackle_frequency, 0) +
                                                                                     weights[6] * coalesce(avg_normalized_tackle_value, 0) +
                                                                                     weights[7] * coalesce(tackle_frequency, 0) +
                                                                                     weights[8] * coalesce(avg_normalized_tackle_value_exp, 0) +
                                                                                     weights[9] * coalesce(tackle_frequency_exp, 0) +
                                                                                     weights[10] * coalesce(avg_normalized_tackle_value_bad, 0) +
                                                                                     weights[11] * coalesce(tackle_frequency_bad, 0)
                                                                                 )
                                                                               ) %>%
                                                                               mutate(
                                                                                 normalized_added_composite = scales::rescale(normalized_added_composite, to = c(0, 1))
                                                                               )
                                                                             
                                                                             
                                                                             # Create Scatter Plot with Labels and Custom Axis Scales
                                                                             ggplot(tackle_efficiency, aes(x = average_tackle_efficiency, y = -average_expectedPointsSaved, size = total_tackles_attempted)) +
                                                                               geom_point(aes(color = quadrantA)) + #color is quadrants
                                                                               geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +  # Add linear trendline
                                                                               geom_text_repel(aes(label = displayName), box.padding = 0.5, point.padding = 0.2, size = 2.5, max.overlaps = 100) +  # Adjust max.overlaps
                                                                               labs(title = "NFL 2022 Weeks 1-9 ILB Tackling Prowess",
                                                                                    x = "Tackler Efficiency (Individual Attempts Only)",
                                                                                    y = "Average Expected Points Saved per Solo Tackle Attempt") +
                                                                               theme(legend.position = "none") +  # Remove the legend
                                                                               scale_x_continuous(breaks = seq(0.5, 1, 0.1)) +  # Set x-axis breaks from 0 to 1 with 0.1 intervals
                                                                               scale_y_continuous(breaks = seq(-1, 1, 0.1)) +  # Set y-axis breaks
                                                                               scale_size_continuous(range = c(2, 10)) +  # Adjust the size range of points
                                                                               geom_vline(xintercept = median_x, linetype = "dotted", color = "red") +
                                                                               geom_hline(yintercept = median_y, linetype = "dotted", color = "red")
                                                                             
                                                                             
                                                                             
                                                                             # Plot the graph with lines at median of x and y axes
                                                                             ggplot(total_tackle_frequency, aes(x = individual_tackle_frequency, y = total_tackle_frequency, size = snap_count)) +
                                                                               geom_point(aes(color = quadrantB)) +
                                                                               geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +  # Add linear trendline
                                                                               geom_text_repel(aes(label = displayName), box.padding = 0.5, point.padding = 0.2, size = 2.5, max.overlaps = 1000) +
                                                                               geom_vline(xintercept = median_x, linetype = "dashed", color = "black") +
                                                                               geom_hline(yintercept = median_y, linetype = "dashed", color = "black") +
                                                                               labs(title = "Nose for the Football",
                                                                                    x = "Individual Tackle Frequency",
                                                                                    y = "Total Tackle Frequency (Assists + Solo)") +
                                                                               theme_minimal() +
                                                                               scale_x_continuous(limits = c(0.05, 0.15), breaks = seq(0.05, 0.15, 0.02)) +
                                                                               scale_y_continuous(limits = c(0.1, 0.25), breaks = seq(0.1, 0.25, 0.03)) +
                                                                               theme(legend.position = "none")
                                                                             
                                                                             # Plot the graph with text repel
                                                                             ggplot(mapped_nonexplosive_tackle, aes(x = tackle_frequency, y = avg_normalized_tackle_value, size = total_nonexplosive_tackles)) +
                                                                               geom_point(aes(color = quadrantC)) +
                                                                               geom_text_repel(aes(label = displayName.x), box.padding = 0.5, point.padding = 0.2, size = 2.5, max.overlaps = 1000) +
                                                                               labs(title = "Butkus Tackle Value, Nonexplosive Plays",
                                                                                    x = "Tackle Frequency, nonexplosive plays",
                                                                                    y = "Average Tackle Value, -0.5 to 0.5") +
                                                                               theme_minimal() +
                                                                               scale_x_continuous(limits = c(0.03, 0.15), breaks = seq(0.03, 0.15, 0.03)) +
                                                                               scale_y_continuous(limits = c(-0.2, 0), breaks = seq(-0.2, 0, 0.05)) +
                                                                               geom_vline(xintercept = median_x3, linetype = "solid", color = "purple") +
                                                                               geom_hline(yintercept = median_y3, linetype = "solid", color = "purple") +
                                                                               theme(legend.position = "none")
                                                                             
                                                                             
                                                                             
                                                                             
                                                                             # Add other layers and settings
                                                                             ggplot(mapped_explosive_tackle, aes(x = tackle_frequency_exp, y = avg_normalized_tackle_value_exp, size = total_explosive_tackles)) +
                                                                               geom_point(aes(color = quadrantD)) +
                                                                               geom_text_repel(aes(label = displayName.x), box.padding = 0.5, point.padding = 0.2, size = 2.5, max.overlaps = 1000) +
                                                                               labs(title = "Butkus Tackle Value, Big Plays",
                                                                                    x = "Explosive Tackle Frequency",
                                                                                    y = "Average Tackle Value, scaled 0.5 to 1") +
                                                                               theme_minimal() +
                                                                               scale_x_continuous(limits = c(0, 0.035), breaks = seq(0, 0.035, 0.007)) +
                                                                               scale_y_continuous(limits = c(0.55, 0.70), breaks = seq(0.55, 0.70, 0.03)) +
                                                                               geom_vline(xintercept = median_x2, linetype = "solid", color = "green") +
                                                                               geom_hline(yintercept = median_y2, linetype = "solid", color = "green") +
                                                                               theme(legend.position = "none")
                                                                             
                                                                             
                                                                             # Plot the graph with text repel
                                                                             ggplot(explosive_tackle_by_player_bad, aes(x = tackle_frequency_bad, y = avg_normalized_tackle_value_bad, size = total_explosive_tackles_bad)) +
                                                                               geom_point(aes(color = quadrantE)) +
                                                                               geom_text_repel(aes(label = displayName), box.padding = 0.5, point.padding = 0.2, size = 2.5, max.overlaps = 1000) +
                                                                               labs(title = "Butkus Tackle Value, Offensive Explosive Plays",
                                                                                    x = "Tackle Frequency",
                                                                                    y = "Average Tackle Value, scaled -0.5 to -1") +
                                                                               theme_minimal() +
                                                                               scale_x_continuous(limits = c(0, 0.024), breaks = seq(0, 0.024, 0.003)) +
                                                                               scale_y_continuous(limits = c(-0.85, -0.55), breaks = seq(-0.85,-0.55, 0.05)) +
                                                                               geom_vline(xintercept = median_x1, linetype = "solid", color = "blue") +
                                                                               geom_hline(yintercept = median_y1, linetype = "solid", color = "blue") +
                                                                               theme(legend.position = "none")