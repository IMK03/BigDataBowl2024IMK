library(dplyr)
library(ggplot2)
library(ggrepel)


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

#scale_color_discrete()
#size = total_tackles_attempted


# View the results
head(tackle_efficiency)
