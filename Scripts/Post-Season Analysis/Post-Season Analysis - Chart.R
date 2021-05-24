#Libraries
{
  library(Lahman)
  library(tidyverse)
  library(ggplot2)
  library(ggrepel)
  library(ggstar)
  library(directlabels)
  library(scales)
  library(gghighlight)
  library(hexbin)
}

Teams %>% 
  mutate(
    teamYear = paste(teamID, yearID, sep = "\n"),
    Win.Percent = W / G * 100,
    DivWin = ifelse(DivWin == "Y", "Division Winner", "Division Loser"),
    LgWin = ifelse(LgWin == "Y", "League Winner", "League Loser"),
    WSWin = ifelse(WSWin == "Y", "World Series Champion", "Non-Champion")
  ) %>% 
  filter(!is.na(DivWin), !is.na(LgWin), !is.na(WSWin)) %>% 
  #ggplot(aes(x = L, y = W, color = as.factor(Rank))) +
  ggplot(aes(x = L, y = W, fill = WSWin, starshape = WSWin)) +
  geom_star(size = 3) +
  facet_grid(
    #WSWin ~ DivWin + LgWin, scales = "fixed", margins = FALSE
    LgWin ~ DivWin, scales = "fixed", margins = FALSE
  ) +
  
  #50% Win Percentage
  geom_segment( 
    aes(x = 40, xend = 100, y = 40, yend = 100),
    inherit.aes = FALSE, linetype = "dotted"
  ) + 
  geom_text(
    aes(x = 100, y = 98, label = "50% Winning Percentage"),
    inherit.aes = FALSE, angle = 43.5,
    color = "gray40", size = 3
  ) +
  
  #110 Number of Games
  geom_segment( 
    aes(x = 70, xend = 45, y = 40, yend = 65),
    inherit.aes = FALSE, linetype = "dashed"
  ) + 
  geom_text(
    aes(x = 70, y = 43, label = "110 Games Played"),
    inherit.aes = FALSE, angle = -43.5,
    color = "gray40", size = 3
  ) +
  
  #160 Number of Games
  geom_segment( 
    aes(x = 105, xend = 50, y = 55, yend = 110),
    inherit.aes = FALSE, linetype = "dashed"
  ) + 
  geom_text(
    aes(x = 59, y = 108, label = "160 Games Played"),
    inherit.aes = FALSE, angle = -43.5,
    color = "gray40", size = 3
  ) +
  scale_x_continuous(n.breaks = 4) +
  scale_y_continuous(n.breaks = 6) +
  ggtitle(
    "Post-Season Performance Against Total Record",
    "1969 - 2019"
  ) +
  xlab("Total Season Losses") +
  ylab("Total Season Wins") +
  labs(
    fill = "World Series Champion",
    starshape = "World Series Champion"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  # scale_fill_gradient(
  #   low = "skyblue1", high = "midnightblue"
  # ) +
  scale_fill_manual(values = c("lightsteelblue3", "gold3")) +
  scale_starshape_manual(values = c(15, 1))

#ggsave("Visualizations/Post Season vs Regular Season.png", width = 14, height = 10)