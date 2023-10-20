
# Load R and Import necessary Libraries
install.packages("tidyverse")
install.packages("nflreadr")
install.packages("nflplotR")
#install.packages("nflfastr")

library(tidyverse)
library(nflreadr)
library(nflplotR)



# Loading in the Data and Taking a Look

pbp_22 = load_pbp(2022)

pbp_22
colnames(pbp_22)
View(pbp_22)


pbp_22 |> 
  filter(pass == 1) |> 
  group_by(passer) |> 
  summarise(ypa = mean(yards_gained, na.rm = TRUE),
            plays = n()) |> 
  arrange(-plays) |> 
  filter(plays > 100) |> 
  mutate(yards_gained = ypa * plays) |> 
  select(passer, yards_gained)




# RBs
pbp_22 |> 
  filter(posteam == "CAR", rush == 1) |> 
  group_by(rusher) |> 
  summarise(mean_epa = mean(epa),
            success_rate = mean(success),
            ypc = mean(yards_gained),
            plays = n()) |> 
  arrange(-mean_epa) |> 
  filter(plays >= 20)






# Offensive performances

pbp_23 = load_pbp(2023)

wk1_off = pbp_23 |> 
  filter(week == 1, !is.na(yards_gained), (pass == 1 | rush == 1)) |> 
  group_by(posteam) |> 
  summarise(mean_epa = mean(epa),
            success_rate = mean(success),
            ypa = mean(yards_gained),
            plays = n()) |> 
  arrange(-mean_epa)
wk1_off

ggplot(wk1_off, aes(x = mean_epa, y = success_rate)) +
  labs(
    x = "EPA per play",
    y = "Success Rate",
    title = "NFL Offensive Performances (Week 1)",
    caption = "By: Sam Burch  |  Data @nflfastR"
  ) +
  stat_smooth(formula = y ~ x, method = "lm", geom = "line", se = FALSE, color = "gray") +
  nflplotR::geom_mean_lines(aes(h_var = mean_epa, v_var = success_rate)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = .07, alpha = .7)

ggsave("off_performance_w1.png", width = 16, height = 9, units = "cm")



# Defensive Performances

wk1_def = pbp_23 |> 
  filter(week == 1, !is.na(yards_gained), (pass == 1 | rush == 1)) |> 
  group_by(defteam) |> 
  summarise(mean_epa = mean(epa),
            success_rate = mean(success),
            ypa = mean(yards_gained),
            plays = n()) |> 
  arrange(mean_epa)
wk1_def


# Final Plots

ggplot(wk1_def, aes(x = mean_epa, y = success_rate)) +
  labs(
    x = "EPA per play Allowed",
    y = "Success Rate Allowed",
    title = "NFL Defensive Performances (Week 1)",
    caption = "By:Sam Burch  |  Data @nflfastR"
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  stat_smooth(formula = y ~ x, method = 'lm', geom = 'line', se = FALSE, color = "gray") +
  nflplotR::geom_mean_lines(aes(h_var = mean_epa, v_var = success_rate)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = defteam), width = .07, alpha = .7)


ggsave("def_performance_w1.png", width = 16, height = 9, units = "cm")




