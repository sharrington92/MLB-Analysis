#Get Inflation Data
{
  {
  filepath <- "C:/Users/shaun/Documents/Boston College/ADEC 7200 Macroeconomics/Discussion 5/"
  }
  
  inflation <- read.csv(
    paste0(
      filepath,
      "API_FP.CPI.TOTL.ZG_DS2_en_csv_v2_2252195.csv"
    ),
    skip = 3
  ) %>% 
    select(
      -c(Country.Code, Indicator.Name, Indicator.Code)
    ) %>% 
    pivot_longer(
      starts_with("X"),
      names_to = "Year",
      values_to = "inflation",
      names_prefix = "X"
    ) %>% 
    filter(
      !is.na(inflation), Country.Name == "United States",
      Year <= max(Salaries$yearID)
    ) %>% 
    arrange(desc(Year)) %>% 
    mutate(
      Year = as.integer(Year),
      inflation.index = ifelse(
        Year == 2016, 1, 
        1 / lag(cumprod(1+inflation/100))
      )
    )
}

#Inflate salaries to 2016 price level
{
  Salaries.real <- left_join(
    x = Salaries, y = inflation,
    by = c("yearID" = "Year")
  ) %>% 
    mutate(salary = salary / inflation.index) %>% 
    select(yearID, teamID, lgID, playerID, salary) %>% 
    left_join(
      x = .,
      y = Batting,
      by = c("yearID", "playerID", "teamID", "lgID")
    ) %>% 
    # select(
    #   yearID, teamID, lgID, playerID, salary, H, BB, HBP,
    #   AB, SF
    # ) %>% 
    mutate(
      OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
      OBP.group = cut(
        OBP,
        c(0,.31, .34, 1)
        #seq(0,1,.01)
      ),
      AVG = H / AB,
      AVG.group = cut(
        OBP,
        c(0,.3, .34, 1)
        #seq(0,1,.1)
      ),
      OBP.to.AVG = OBP / AVG,
      OBP.to.AVG.group = cut(OBP.to.AVG, c(0,1.242399,10)),
      year.group = cut(yearID, c(1984,1992, 1997, 2006, 2016))
    ) %>% 
    filter(
      OBP != 0, OBP != 1, !is.na(OBP),
      AVG != 0, AVG != 1, !is.na(AVG)
    ) %>% 
    group_by(playerID) %>% 
    arrange(yearID) %>% 
    mutate(
      salary.diff = salary / lag(salary) - 1
    )
  
  
}

#Line plot: Salary X Year, color: OBP.to.avg
{
  ratio.plot <- Salaries.real %>% 
    filter(!is.na(OBP.group)) %>% 
    group_by(yearID, OBP.to.AVG.group) %>% 
    #slice_min(prop = .9999, order_by = OBP.to.AVG) %>% 
    summarize(
      salary = mean(salary)
    ) %>% 
    ggplot(
      aes(x = yearID, y = salary, color = OBP.to.AVG.group)
    ) +
    geom_line(size = 1) +
    scale_y_continuous(
      "Average Salary, millions of dollars",
      labels = label_dollar(scale = 1/1000000),
    ) +
    scale_x_continuous(
      "",
      breaks = seq(1985, 2016, 3)
    ) +
    scale_color_discrete(
      name = "OBP to AVG Ratio Grouping: ",
      labels = c("Below Median\n<1.24", "Above Median\n>1.24"),
      guide = F
    ) +
    ggtitle(
      "Inflation-Adjusted Average Salary",
      "By On-Base Percentage (OBP) to Average Batting (AVG) Ratio Grouping"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = .5),
      plot.subtitle = element_text(hjust = .5)
    )
  
}

#Line plot: OBP X Year, color: OBP.to.avg
{
  obp.plot <- Salaries.real %>% 
    filter(!is.na(OBP.group)) %>% 
    group_by(yearID, OBP.to.AVG.group) %>% 
    summarize(
      OBP = mean(OBP)
    ) %>% 
    ggplot(
      aes(x = yearID, y = OBP, color = OBP.to.AVG.group)
    ) +
    geom_line(size = 1) +
    scale_y_continuous(
      "On-Base Percentage",
      labels = label_percent(accuracy = 1)
    ) +
    scale_x_continuous(
      "",
      breaks = seq(1985, 2016, 3)
    ) +
    scale_color_discrete(
      name = "OBP to AVG Ratio Grouping: ",
      labels = c("Below Median\n<1.24", "Above Median\n>1.24"),
      guide = F
    ) +
    ggtitle(
      "Average On-Base Percentage",
      "By On-Base Percentage (OBP) to Average Batting (AVG) Ratio Grouping"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = .5),
      plot.subtitle = element_text(hjust = .5)
    )
  
}

#Line plot: AVG X Year, color: OBP.to.avg
{
  avg.plot <- Salaries.real %>% 
    filter(!is.na(OBP.group)) %>% 
    group_by(yearID, OBP.to.AVG.group) %>% 
    summarize(
      AVG = mean(AVG)
    ) %>% 
    ggplot(
      aes(x = yearID, y = AVG, color = OBP.to.AVG.group)
    ) +
    geom_line(size = 1) +
    scale_y_continuous(
      "Batting Average",
      labels = label_percent(accuracy = 1)
    ) +
    scale_x_continuous(
      "",
      breaks = seq(1985, 2016, 3)
    ) +
    scale_color_discrete(
      name = "OBP to AVG Ratio Grouping: ",
      labels = c("Below Median\n<1.24", "Above Median\n>1.24")
    ) +
    ggtitle(
      "Season-Average Batting Average",
      "By On-Base Percentage (OBP) to Average Batting (AVG) Ratio Grouping"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = .5),
      plot.subtitle = element_text(hjust = .5)
    )
  
}


 
# qplot(x = Salaries.real$OBP.to.AVG, geom = "histogram", bins = 100, xlim = c(0,2))
# qplot(x = OBP, y = AVG, geom = "point", data = Salaries.real)
# 
# 
# png(file = "Visualizations/Salaries.png", width = 2880, height = 1920, res = 180)
gridExtra::grid.arrange(ratio.plot, obp.plot, avg.plot
             , nrow = 3, ncol = 1
             #, widths = c(2, 1)
) 
# dev.off()
# ggsave(., "Visualizations/Salaries.png", width = 12, height = 14)
