 

# Create charts
{
  # Heat map
  p.heat <- salary.by.team.2 %>%
    select(
      yearID, teamID,
      ends_with("p") & contains("salary")
    ) %>% 
    pivot_longer(
      -c(yearID, teamID),
      names_to = "Percentile", values_to = "Salary",
      names_prefix = "salary."
    ) %>% 
    filter(Percentile %in% c("25p", "50p", "75p", "90p")) %>% 
    group_by(yearID, Percentile) %>% 
    summarize(Salary = mean(Salary)) %>%  
    complete(yearID = 1980:2019, nesting(Percentile)) %>% 
    mutate(
      year.of.decade = yearID %% 10,
      decade = yearID - year.of.decade,
      Salary.cut = cut(Salary, c(
        -1, -.7, -.625, -.5, -.425, -.333, -.25, 0,
        .3, .4, .5, 1.15, 1.25, 1.35,1.5
      )),
      Percentile = case_when(
        Percentile == "25p" ~ "P(25)",
        Percentile == "50p" ~ "P(50)",
        Percentile == "75p" ~ "P(75)",
        Percentile == "90p" ~ "P(90)"
      ),
      Percentile = factor(Percentile, levels = c("P(90)", "P(75)", "P(50)", "P(25)"))
    ) %>% 
    ggplot(aes(
      y = year.of.decade,
      x = decade,
      fill = Salary,
      label = yearID
    )) +
    geom_tile(color = "gray10", size = .25) +
    geom_text(color = "black", size = 3) +
    facet_wrap(. ~ Percentile, labeller = ) +
    scale_fill_gradientn(
      colors = rainbow(20),
      na.value = "gray85",
      name = "Standard Deviations from Mean: "
    ) +
    scale_x_continuous("Decade") +
    scale_y_continuous("") +
    ggtitle(
      "Relative Salary at Various Percentiles",
      "As measured by standard deviations from annual mean"
    ) +
    theme_test() +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(2, "cm"),
      #legend.title = element_blank(),
      plot.title = element_text(hjust = .5),
      plot.subtitle = element_text(hjust = .5),
      axis.text = element_text(size = 6),
      axis.title = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    )
  
  
  
  # Corresponding Line Chart
  p.line <- salary.by.team.2 %>%
    select(
      yearID, teamID,
      ends_with("p") & contains("salary")
    ) %>% 
    pivot_longer(
      -c(yearID, teamID),
      names_to = "Percentile", values_to = "Salary",
      names_prefix = "salary."
    ) %>% 
    filter(Percentile %in% c("25p", "50p", "75p", "90p")) %>% 
    mutate(Percentile = case_when(
      Percentile == "25p" ~ "P(25)",
      Percentile == "50p" ~ "P(50)",
      Percentile == "75p" ~ "P(75)",
      Percentile == "90p" ~ "P(90)"
    )) %>% 
    group_by(yearID, Percentile) %>% 
    summarize(Salary = mean(Salary)) %>% 
    ggplot(aes(
      y = Salary,
      x = yearID
    )) +
    geom_line(
      aes(group = Percentile, color = Salary),
      size = 1
    ) +
    geom_dl(
      aes(label = Percentile), 
      method = list("chull.grid", cex = 0.65),
      color = 'gray20'
    ) +
    scale_color_gradientn(
      colors = rainbow(20),
      na.value = "gray85"
    ) +
    theme_test() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = .5),
      axis.text = element_text(size = 6),
      axis.title = element_blank()
    )
  
  
}

# Use theme-getter custom functions to set common components
# Source: http://www.sthda.com/english/wiki/wiki.php?id_contents=7930
{
  # Legend
  {
    get_legend <- function(myggplot){
      tmp <- ggplot_gtable(ggplot_build(myggplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)
    }
  }
  
  #x-axis title
  {
    get_xtitle <- function(myggplot){
      tmp <- ggplot_gtable(ggplot_build(myggplot))
      #str_which(tmp$name, "tag")
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "xlab-b")
      xtitle <- tmp$grobs[[12]]
      return(xtitle)
    }
  }
  
  #y-axis title
  {
    get_ytitle <- function(myggplot){
      tmp <- ggplot_gtable(ggplot_build(myggplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "ylab")
      ytitle <- tmp$grobs[[13]]
      return(ytitle)
    }
  }
  
  #plot title
  {
    get_title <- function(myggplot){
      tmp <- ggplot_gtable(ggplot_build(myggplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "ylab")
      title <- tmp$grobs[[32]]
      return(title)
    }
  }
  
  #plot subtitle
  {
    get_subtitle <- function(myggplot){
      tmp <- ggplot_gtable(ggplot_build(myggplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "ylab")
      title <- tmp$grobs[[31]]
      return(title)
    }
  }
  
}

legend.heat <- get_legend(p.heat)
title.heat <- get_title(p.heat)
subtitle.heat <- get_subtitle(p.heat)


grid.heat <- arrangeGrob(
  p.heat + theme(
    legend.position = "none", plot.title = element_blank(),
    plot.subtitle = element_blank()
  )
  , p.line + theme(legend.position = "none", plot.title = element_blank())
  , nrow = 1, ncol = 2, widths = c(2, 1)
)

grid.arrange(
  title.heat, subtitle.heat, grid.heat, legend.heat,
  nrow = 4, ncol = 1,
  heights = c(.15,.15, 2, .3)
)


# g <- arrangeGrob( #grid.arrange
#   title.heat, subtitle.heat, grid.heat, legend.heat,
#   nrow = 4, ncol = 1,
#   heights = c(.15,.15, 2, .3)
# )
# 
# ggsave("Visualizations/Salary Heat Map.png", g)
  