
# Pie chart comparison
{
  ws.win.proportion %>% 
  #select(., name) %>% distinct() %>% count() %>% View()
    ggplot(aes(x = 1, y = WSWin.prop, fill = name)) +
    geom_bar(stat="identity", width=1, color = "black") +
    # geom_text_repel(
    #   aes(label = name, y = ypos),
    #   size = 1
    # ) +
    facet_wrap(. ~ Period) +
    coord_polar("y", start=0) +
    scale_x_discrete("") +
    scale_y_continuous(
      ""
    ) +
    ggtitle(
      "World Series Championship Distribution Per Period",
      "Proportion of World Series Championships by Team"
    ) +
    theme(
      legend.position = "NULL",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      axis.text = element_blank()
    )
  
  ggsave("Visualizations/WS Championships Distribution by Period.png", width = 14, height = 8)
  
  unique(wins.ts$name)
  
}


