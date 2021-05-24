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


wins.ts %>% 
  mutate(WSWin = ifelse(WSWin == 1, TRUE, FALSE)) %>% 
  ggplot() +
  geom_line(
    aes(x = yearID, y = Running.W, color = nameFull)
  ) +
  gghighlight(
    #aes(x = yearID, y = Running.W, color = nameFull),
    max(Running.W), max_highlight = 10,
    use_direct_label = FALSE
  ) +
  #facet_wrap(. ~ lgID) +
  geom_star(
    aes(
      x = yearID, y = Running.W, starshape = as.factor(WSWin),
      fill = nameFull, size = as.factor(WSWin)
    ),
    color = "black"
  ) +
  geom_text_repel(
    aes(
      x = yearID, y = Running.W, label = ifelse(WSWin == 1, name,"")
    ),
    #nudge_y = .25,
    size = 2, 
    color = "gray15"
    #force = .1, max.overlaps = 15,
    #direction = "x"
  ) +
  geom_dl(
    aes(
      x = yearID, y = Running.W, color = nameFull,
      label = paste(nameFirst, nameLast, sep = " ")
    ), 
    method = list(
      dl.trans(x = x - .05), 
      dl.trans(y = y + .15),
      "last.points", rot = 65, cex = .75
    ),
    size = .75
  ) +
  #scale_size(range = c(0,2), guide = FALSE) +
  scale_starshape_manual(
    values = c(15, 1), guide = FALSE
  ) +
  scale_y_continuous(
    n.breaks = 10,
    limits = c(0, max(wins.byManager$W.Career)+300),
    labels = scales::comma
  ) +
  scale_x_continuous(
    n.breaks = 10,
    #limits = c( min(total.wins.top$yearID), max(total.wins.top$yearID)+1)
    limits = c( min(wins.byManager$year.bgn), max(wins.byManager$year.end)+1)
  ) +
  scale_fill_discrete(guide = FALSE) +
  scale_size_manual(
    values = c(0,3), guide = FALSE
  ) +
  ggtitle(
    "Top 10 Team Managers by Total Career Wins",
    "1884 - 2019"
  ) +
  xlab("Year") +
  ylab("Total Cumulative Wins") +
  labs(
    size = "World Series Champion",
    color = "Team Manager"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.key.size = unit(2,"line")
  ) +
  guides(
    size = guide_legend(override.aes = list(starshape = 1))
  )
