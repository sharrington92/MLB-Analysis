

pos <- "identity"

p.all <- salary.by.team %>%
  select(
    Period.detail, teamID, "salary.total",
    W, G, L
  ) %>% 
  pivot_longer(
    -c(Period.detail, teamID, W, G, L),
    names_to = "Percentile", values_to = "Salary",
    names_prefix = "salary."
  ) %>% 
  filter(Percentile == "total") %>% 
  group_by(Period.detail) %>% 
  mutate(
    distance = sqrt(scale(Salary)^2 + scale(W/G)^2),
    win.percentage = W / G
  ) %>%  
  ggplot(aes(
    x = Salary,
    y = win.percentage,
    color = Period.detail, fill = Period.detail, 
    alpha = -distance
  )) +
  geom_point(shape = 21, fill = "gray70", size = 1.5) +
  geom_smooth(method = "loess", se = FALSE, fullrange = F) +
  scale_x_continuous(
    "Relative Salary as measured by standard deviations away from annual mean",
    breaks = c(-4:4),
    limits = c(-3,4)
  ) +
  scale_y_continuous(
    "Winning Percentage",
    labels = label_percent(accuracy = 1)
  ) +
  scale_alpha_continuous(guide = "none") +
  scale_color_discrete("Period: ") +
  scale_fill_discrete("Period: ") +
  ggtitle(
    "Relative Team Salaries to Winning Percentage"
    #subtitle = paste0(p, "th Percentile")
  ) +
  # annotate(
  #   geom = "text", x = 0, y = .25,
  #   label = paste0(p, "th Percentile Salary"), size = 2
  # ) +
  theme_test() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = .5),
    axis.text = element_text(size = 6),
    axis.title.y = element_text(size = 7),
    axis.title.x = element_text(size = 7)
  )

grid.arrange(
  ggMarginal(
    p = p.all
    ,groupFill = T, margins = "x", type = "histogram", size = 3
    , xparams = list(position=pos, bins = 60)
  )
)

# g <- arrangeGrob(ggMarginal(
#   p = p.all
#   ,groupFill = T, margins = "x", type = "histogram", size = 3
#   , xparams = list(position=pos, bins = 60)
# ))
# 
# 
# ggsave("Visualizations/Total Salaries by Win Percentage.png", g)