
Teams %>% 
  left_join(
    x = ., y = periods, by = "yearID"
  ) %>% 
  group_by(Period.detail, name) %>% 
  summarize(
    win.percent = sum(W, na.rm = T) / sum(G, na.rm = T),
    total.games = sum(G, na.rm = T)
  ) %>% 
  filter(total.games > 1000) %>% 
  mutate(
    rank = rank(-win.percent)
  ) %>% 
  filter(rank < 11) %>% 
  ggplot(
    aes(
      x = Period.detail, y = rank, color = name,
      label = name
    )
  ) +
  geom_point(aes(size = win.percent)) +
  geom_line(aes(group = name)) +
  geom_text(color = "black", size = 3, vjust = .1) +
  scale_y_continuous(
    trans = "reverse"
  ) +
  scale_color_discrete(
    guide = "none"
  ) +
  scale_size_continuous(
    name = "Win Percentage",
    labels = label_percent(accuracy = 1)
  ) +
  ggtitle("Top 10 Teams by Winning Percentage") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = .5),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

ggsave("Visualizations/Manager & Team Comparison/Top 10 Ranked by Period.png")