# Create function for charts
{
  salary.plot <- function(p){
    salary.by.team %>%
      select(
        Period.detail, teamID, ends_with("p") & contains("salary"),
        W, G, L
      ) %>% 
      pivot_longer(
        -c(Period.detail, teamID, W, G, L),
        names_to = "Percentile", values_to = "Salary",
        names_prefix = "salary."
      ) %>% 
      filter(Percentile == paste0(p, "p")) %>% 
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
      geom_smooth(method = "gam", se = FALSE, fullrange = F) +
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
      ) +
      annotation_custom(
        grid::grid.text(
          paste0(p, "th Percentile Salary"),
          y = .1, gp = grid::gpar(fontsize=8)
        )
      ) +
      theme_test() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = .5),
        axis.text = element_text(size = 6),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7)
      )
  }
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
    
    #my.legend <- get_legend(salary.plot(90))
  }
  
  #x-axis title
  {
    get_xtitle <- function(myggplot){
      tmp <- ggplot_gtable(ggplot_build(myggplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "xlab-b")
      xtitle <- tmp$grobs[[12]]
      return(xtitle)
    }
    
    #x.title <- get_xtitle(salary.plot(90))
  }
  
  #y-axis title
  {
    get_ytitle <- function(myggplot){
      tmp <- ggplot_gtable(ggplot_build(myggplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "ylab")
      ytitle <- tmp$grobs[[13]]
      return(ytitle)
    }
    
    #y.title <- get_ytitle(salary.plot(90))
  }
  
  #plot title
  {
    get_title <- function(myggplot){
      tmp <- ggplot_gtable(ggplot_build(myggplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "ylab")
      title <- tmp$grobs[[17]]
      return(title)
    }
    
    #title <- get_title(salary.plot(90))
  }
}


pos <- "identity"

# panel.plots <- arrangeGrob(
#    ggMarginal(
#      p = salary.plot(25) + theme(
#        legend.position = "none", axis.title.x = element_blank(),
#        axis.title.y = element_blank(), plot.title = element_blank()
#      )
#      ,groupFill = T, margins = "x", type = "histogram", size = 3
#      , xparams = list(position=pos, bins = 60)
#    )
#   , ggMarginal(
#     p = salary.plot(50) + theme(
#       legend.position = "none", axis.title.x = element_blank(),
#       axis.title.y = element_blank(), plot.title = element_blank()
#     )
#     ,groupFill = T, margins = "x", type = "histogram", size = 3
#     , xparams = list(position=pos, bins = 60)
#   )
#   , ggMarginal(
#     p = salary.plot(75) + theme(
#       legend.position = "none", axis.title.x = element_blank(),
#       axis.title.y = element_blank(), plot.title = element_blank()
#     )
#     ,groupFill = T, margins = "x", type = "histogram", size = 3
#     , xparams = list(position=pos, bins = 60)
#   )
#   , ggMarginal(
#     p = salary.plot(90) + theme(
#       legend.position = "none", axis.title.x = element_blank(),
#       axis.title.y = element_blank(), plot.title = element_blank()
#     )
#     ,groupFill = T, margins = "x", type = "histogram", size = 3
#     , xparams = list(position=pos, bins = 60)
#   )
#   , nrow = 2
#   , ncol = 2
#   , heights = c(2, 2)
# 
# )

# Get chart components
{
  # title <- get_title(salary.plot(90))
  # my.legend <- get_legend(salary.plot(90))
  # y.title <- get_ytitle(salary.plot(90))
  # x.title <- get_xtitle(salary.plot(90))
}

grid.arrange( # or grid.arrange
  arrangeGrob(
    ggMarginal(
      p = salary.plot(25) + theme(
        legend.position = "none", axis.title.x = element_blank(),
        axis.title.y = element_blank(), plot.title = element_blank()
      )
      ,groupFill = T, margins = "x", type = "histogram", size = 3
      , xparams = list(position=pos, bins = 60)
    )
    , ggMarginal(
      p = salary.plot(50) + theme(
        legend.position = "none", axis.title.x = element_blank(),
        axis.title.y = element_blank(), plot.title = element_blank()
      )
      ,groupFill = T, margins = "x", type = "histogram", size = 3
      , xparams = list(position=pos, bins = 60)
    )
    , ggMarginal(
      p = salary.plot(75) + theme(
        legend.position = "none", axis.title.x = element_blank(),
        axis.title.y = element_blank(), plot.title = element_blank()
      )
      ,groupFill = T, margins = "x", type = "histogram", size = 3
      , xparams = list(position=pos, bins = 60)
    )
    , ggMarginal(
      p = salary.plot(90) + theme(
        legend.position = "none", axis.title.x = element_blank(),
        axis.title.y = element_blank(), plot.title = element_blank()
      )
      ,groupFill = T, margins = "x", type = "histogram", size = 3
      , xparams = list(position=pos, bins = 60)
    )
    , nrow = 2
    , ncol = 2
    , heights = c(2, 2)
    
  )
  , get_legend(salary.plot(90)) %>% arrangeGrob()
  , get_xtitle(salary.plot(90)) %>% arrangeGrob()
  , get_ytitle(salary.plot(90)) %>% arrangeGrob()
  , get_title(salary.plot(90)) %>% arrangeGrob()
  , nrow = 4
  , ncol = 2
  , layout_matrix = cbind(c(NA,4,NA,NA), c(5,1,3,2))
  , widths = c(.15, 4)
  , heights = c(.25, 4, .25,.3)
)

# grid.arrange( # or grid.arrange
#   panel.plots
#   , get_legend(salary.plot(90))
#   , get_xtitle(salary.plot(90))
#   , get_ytitle(salary.plot(90))
#   , get_title(salary.plot(90))
#   , nrow = 4
#   , ncol = 2
#   , layout_matrix = cbind(c(NA,4,NA,NA), c(5,1,3,2))
#   , widths = c(.15, 4)
#   , heights = c(.25, 4, .25,.3)
# )

#ggsave("Visualizations/Percentile Salaries by Win Percentage.png", g)
