#https://x.com/TotallyREALSpo1/status/1739767061880926441?s=20

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
    total_tackles_attempted = (missed_tackles + tackles_successful),
    average_tackle_efficiency = ifelse(total_tackles_attempted > 0, tackles_successful / total_tackles_attempted, 0),
    total_expectedPointsAdded = sum(
      case_when(
        tackle == 1 & pff_missedTackle == 0 ~ expectedPointsAdded,
        tackle == 0 & pff_missedTackle == 1 ~ -expectedPointsAdded,
        TRUE ~ 0), na.rm = TRUE)
    , .groups = 'drop') %>%
  filter(total_tackles_attempted >= 10, position %in% c("ILB", "MLB"))  # Filter by position
# Assuming nflId from tackles_data maps to playId from play_data
#tackle_efficiency <- left_join(tackle_efficiency, play_data %>% select(playId, defensiveTeam), by = c("nflId" = "playId"))

# Create Scatter Plot with Labels and Custom Axis Scales
ggplot(tackle_efficiency, aes(x = average_tackle_efficiency, y = -total_expectedPointsAdded)) + #color = defensiveTeam)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +  # Add linear trendline
  geom_text_repel(aes(label = displayName), box.padding = 0.5, point.padding = 0.2, size = 2.5, max.overlaps = 20) +  # Adjust max.overlaps
  labs(title = "NFL 2022 Weeks 1-9 ILB Tackling Prowess",
       x = "Tackler Efficiency (Individual Attempts Only)",
       y = "Expected Points Saved") +
  theme(legend.position = "none") +  # Remove the legend
  scale_x_continuous(breaks = seq(0.5, 1, 0.1)) +  # Set x-axis breaks from 0 to 1 with 0.1 intervals
  scale_y_continuous(breaks = seq(-15, 15, 5)) +  # Set y-axis breaks
  scale_size_continuous(range = c(2, 10))     # Adjust the size range of points
#scale_color_discrete()
#size = total_tackles_attempted
