



#Team managers with >= 3 WS Championships
{
  
  
  wins.ts %>% 
    filter(WSWin.Career > 2) %>% 
    ggplot(aes(x = yearID, y = Running.WSWin, color = nameFull)) +
    geom_step(direction = "hv", size = 1) +
    geom_star(aes(starshape = as.factor(team.chg), fill = nameFull,
                  size = Win.Percent),
              color = "black") +
    geom_label_repel(aes(label = ifelse(team.chg == 1, name,""),
                         fill = nameFull),
                     nudge_y = .25, size = 2.5, color = "black",
                     force = .1, max.overlaps = 15) +
    geom_dl(aes(label = paste(nameFirst, nameLast, sep = "\n")), 
            method = list(#dl.trans(x = x + 0), 
              dl.trans(y = y + .25),
              "smart.grid", rot = 0, cex = .9),
            size = 2) +
    scale_size(range = c(2,5)) +
    scale_starshape_manual(values = c(28, 29)) +
    scale_y_continuous(breaks = c(0:7)) +
    scale_x_continuous(n.breaks = 10) +
    scale_fill_discrete(guide = FALSE) +
    ggtitle(
      "Team Managers with Three or More World Series Championships",
      "1884 - 2019"
    ) +
    xlab("Year") +
    ylab("Total World Series Championships") +
    labs(
      starshape = "First Year at Team",
      size = "Season Winning Percentage",
      color = "Team Manager"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.key.size = unit(2,"line")
    ) +
    guides(
      size = guide_legend(override.aes = list(starshape = 28))
    ) 
  
  ggsave("Visualizations/Top Managers - WS Champs.png", width = 14, height = 8)
}


