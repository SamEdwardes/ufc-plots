library(tidyverse)

# boilerplate
url <- "https://github.com/SamEdwardes/ufc-data/raw/master/fight_results.csv"
raw <- read_csv(url, col_types = cols()) %>%
  mutate(date = lubridate::mdy(date))
colours_2 <- list(blue = "#3775b7", purple = "#5737b7", green = "#37b779",
                  grey = "#e9e9e9")


# //////////////////////////////////////////////////////////////////////////
# Do UFC judges reward striking?
# //////////////////////////////////////////////////////////////////////////

df <- raw %>%
  mutate(fight_id = paste0(event_name, fighter_1_name, fighter_2_name)) %>%
  filter(grepl(".*DEC*", win_method)) %>%
  select(fight_id,
         winner_name = fighter_1_name,
         loser_name = fighter_2_name,
         Winner = fighter_1_str, 
         Loser = fighter_2_str) %>%
  mutate(winner_delta = Winner - Loser,
         loser_delta = Loser - Winner) %>%
  pivot_longer(cols = c(Winner, Loser), 
               values_to = "feature", 
               names_to = "result") %>%
  mutate(delta = if_else(result == "Winner", winner_delta, loser_delta),
         name = if_else(result == "Winner", winner_name, loser_name)) %>%
  select(fight_id, name, result, feature, delta)

# fig 1 - total strikes
fig_1 <- df %>%
  ggplot(aes(x = feature, y = result)) +
  geom_jitter(height = 1/4, colour = colours_2$blue) +
  geom_boxplot(width = 2/6, outlier.shape = NA, colour = colours_2$purple,
               fill = colours_2$grey) +
  # reminder, margins are c(top, right, bottom, left)
  # a good setting for twitter is c(0.5, 1, 1.3, 0.5)
  theme(plot.margin = unit(c(1, 1.5, 1, 1.5), "cm")) +
  labs(title = "Do UFC Judges Reward Striking?",
       x = "Number of Strikes Thrown",
       y = element_blank(),
       caption = "Created by Sam Edwardes\n(2020)")

fig_1

ggsave("imgs/do_judges_reward_striking.png", 
       fig_1, 
       height = 5,
       width = 8)

# fig 2 - strike difference
fig_2 <- df %>%
  ggplot(aes(x = delta, y = result)) +
  geom_jitter(height = 1/4, colour = colours_2$blue) +
  geom_boxplot(width = 2/6, outlier.shape = NA, colour = colours_2$purple,
               fill = colours_2$grey) +
  # reminder, margins are c(top, right, bottom, left)
  # a good setting for twitter is c(0.5, 1, 1.3, 0.5)
  theme(plot.margin = unit(c(1, 1.5, 1, 1.5), "cm")) +
  labs(title = "Do UFC Judges Reward Striking?",
       x = "Difference in Strikes Thrown",
       y = element_blank(),
       caption = "Created by Sam Edwardes\n(2020)")

fig_2

ggsave("imgs/do_judges_reward_striking_delta.png", 
       fig_2, 
       height = 5,
       width = 8)