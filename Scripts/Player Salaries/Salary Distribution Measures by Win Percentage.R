


# Create function for distribution measure charts
{
  salary.plot.var <- function(var){
    salary.by.team %>%
      select(
        Period.detail, teamID, salary.sd, salary.skew,
        salary.kurtosis, W, G, L
      ) %>% 
      pivot_longer(
        -c(Period.detail, teamID, W, G, L),
        names_to = "Measure", values_to = "Value",
        names_prefix = "salary."
      ) %>% 
      filter(Measure == var) %>% 
      group_by(Period.detail) %>% 
      mutate(
        distance = sqrt(scale(Value)^2 + scale(W/G)^2),
        win.percentage = W / G
      ) %>%  
      ggplot(aes(
        x = Value,
        y = win.percentage,
        color = Period.detail, fill = Period.detail, 
        alpha = -distance
      )) +
      geom_point(shape = 21, fill = "gray70", size = 1.5) +
      geom_smooth(method = "gam", se = FALSE, fullrange = F) +
      scale_x_continuous(
        "Salary Distribution Measures",
        breaks = c(-4:4),
        #limits = c(-3,4)
      ) +
      scale_y_continuous(
        "Winning Percentage",
        labels = label_percent(accuracy = 1)
      ) +
      scale_alpha_continuous(guide = "none") +
      scale_color_discrete("Period: ") +
      scale_fill_discrete("Period: ") +
      ggtitle(
        "Relative Team Salary Distribution to Winning Percentage"
        #subtitle = paste0(p, "th Percentile")
      ) +
      annotation_custom(
        #geom = "text",# x = 1, y = .25,
        #label = 
        grid::grid.text(
          #ifelse(var == "sd", "Standard Distribution", "Skewness"),
          case_when(
            var == "sd" ~ "Standard Distribution",
            var == "skew" ~ "Skewness",
            TRUE ~ "Excess Kurtosis"
          ),
          y = .1, gp = grid::gpar(fontsize=8)
        )
        #size = 2
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
      title <- tmp$grobs[[17]]
      return(title)
    }
  }
}


pos <- "identity"

# panel.plots.var <- arrangeGrob(
#   ggMarginal(
#     p = salary.plot.var("sd") + theme(
#       legend.position = "none", axis.title.x = element_blank(),
#       axis.title.y = element_blank(), plot.title = element_blank()
#     )
#     ,groupFill = T, margins = "x", type = "histogram", size = 3
#     , xparams = list(position=pos)
#   )
#   , ggMarginal(
#     p = salary.plot.var("skew") + theme(
#       legend.position = "none", axis.title.x = element_blank(),
#       axis.title.y = element_blank(), plot.title = element_blank()
#     )
#     ,groupFill = T, margins = "x", type = "histogram", size = 3
#     , xparams = list(position=pos)
#   )
#   , ggMarginal(
#     p = salary.plot.var("kurtosis") + theme(
#       legend.position = "none", axis.title.x = element_blank(),
#       axis.title.y = element_blank(), plot.title = element_blank()
#     )
#     ,groupFill = T, margins = "x", type = "histogram", size = 3
#     , xparams = list(position=pos)
#   )
#   , nrow = 3
#   , ncol = 1
#   #, heights = c(2, 2)
#   
# )

# Get chart components
{
  # title.var <- get_title(salary.plot.var("sd")) %>% arrangeGrob()
  # y.title.var <- get_ytitle(salary.plot.var("sd")) %>% arrangeGrob()
  # x.title.var <- get_xtitle(salary.plot.var("sd")) %>% arrangeGrob()
  # my.legend.var <- get_legend(salary.plot.var("sd")) %>% arrangeGrob()
}

grid.arrange( #grid.arrange
  arrangeGrob(
    ggMarginal(
      p = salary.plot.var("sd") + theme(
        legend.position = "none", axis.title.x = element_blank(),
        axis.title.y = element_blank(), plot.title = element_blank()
      )
      ,groupFill = T, margins = "x", type = "histogram", size = 3
      , xparams = list(position=pos)
    )
    , ggMarginal(
      p = salary.plot.var("skew") + theme(
        legend.position = "none", axis.title.x = element_blank(),
        axis.title.y = element_blank(), plot.title = element_blank()
      )
      ,groupFill = T, margins = "x", type = "histogram", size = 3
      , xparams = list(position=pos)
    )
    , ggMarginal(
      p = salary.plot.var("kurtosis") + theme(
        legend.position = "none", axis.title.x = element_blank(),
        axis.title.y = element_blank(), plot.title = element_blank()
      )
      ,groupFill = T, margins = "x", type = "histogram", size = 3
      , xparams = list(position=pos)
    )
    , nrow = 3
    , ncol = 1
  )
  , get_legend(salary.plot.var("sd")) %>% arrangeGrob()
  , get_xtitle(salary.plot.var("sd")) %>% arrangeGrob()
  , get_ytitle(salary.plot.var("sd")) %>% arrangeGrob()
  , get_title(salary.plot.var("sd")) %>% arrangeGrob()
  , nrow = 4
  , ncol = 2
  , layout_matrix = cbind(c(NA,4,NA,NA), c(5,1,3,2))
  , widths = c(.15, 4)
  , heights = c(.25, 4, .25,.3)
)

#ggsave("Visualizations/Salary Distribution by Win Percentage.png", g)

