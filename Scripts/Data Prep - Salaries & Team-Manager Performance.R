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
  library(ggExtra)
  library(gridExtra)
  library(lubridate)
  library(zoo)
}


#Get wins by each manager for each team
{
  wins.byManagerAndTeam <- left_join(
    x = Managers,
    y = Teams,
    by = c("yearID", "lgID", "teamID"), suffix = c("", ".y")
  ) %>%
    select(-contains(".y")) %>% 
    mutate(
      DivWin = ifelse(DivWin == "Y", 1, 0),
      WCWin = ifelse(WCWin == "Y", 1, 0),
      LgWin = ifelse(LgWin == "Y", 1, 0),
      WSWin = ifelse(WSWin == "Y", 1, 0)
    ) %>% 
    group_by(playerID, teamID) %>% 
    summarize(
      year.bgn = min(yearID),
      year.end = max(yearID),
      W = sum(W, na.rm = T),
      L = sum(L, na.rm = T),
      DivWin = sum(DivWin, na.rm = T),
      WCWin = sum(WCWin, na.rm = T),
      LgWin = sum(LgWin, na.rm = T),
      WSWin = sum(WSWin, na.rm = T)
    )  
}


#Get total wins by manager
{
  wins.byManager <- wins.byManagerAndTeam %>% 
    group_by(playerID) %>% 
    summarize(
      year.bgn = min(year.bgn),
      year.end = max(year.end),
      W.Career = sum(W, na.rm = TRUE),
      L.Career = sum(L, na.rm = TRUE),
      DivWin.Career = sum(DivWin, na.rm = TRUE),
      WCWin.Career = sum(WCWin, na.rm = TRUE),
      LgWin.Career = sum(LgWin, na.rm = TRUE),
      WSWin.Career = sum(WSWin, na.rm = TRUE)
    ) %>%
    mutate(
      WSWin.Avg.Career = WSWin.Career / (year.end - year.bgn + 1),
      Win.Avg.Career = W.Career / (year.end - year.bgn + 1)
    )  
}


#Get time series of wins per season by manager and team
{
  wins.ts <- left_join(
    x = Managers,
    y = Teams,
    by = c("yearID", "lgID", "teamID"), suffix = c("", ".y")
  ) %>% 
    select(-contains(".y")) %>% 
    select(playerID, yearID, teamID, name, lgID, G, W, L, franchID, DivWin, WCWin, LgWin, WSWin) %>% 
    mutate(
      DivWin = ifelse(DivWin == "Y", 1, 0),
      WCWin = ifelse(WCWin == "Y", 1, 0),
      LgWin = ifelse(LgWin == "Y", 1, 0),
      WSWin = ifelse(WSWin == "Y", 1, 0)
    ) %>% 
    left_join(
      x = ., y = wins.byManager, by = c("playerID"), suffix = c("",".y")
    ) %>% 
    select(-contains(".y"), -c(year.bgn, year.end)) %>% 
    replace_na(list(G = 0, W = 0, L = 0, DivWin = 0, LgWin = 0, WSWin = 0)) %>% 
    group_by(playerID) %>% 
    arrange(yearID) %>% 
    add_count() %>% 
    mutate(
      Running.WSWin = cumsum(WSWin),
      Running.W = cumsum(W),
      team.chg = ifelse(teamID == lag(teamID), 0, 1),
      Win.Percent = W / G * 100
    ) %>% 
    replace_na(list(team.chg = 1)) %>% 
    left_join(
      x = ., y = People, by = "playerID", suffix = c("", ".y")
    ) %>% 
    select(
      yearID, playerID, teamID, name, G, W, L, DivWin, LgWin, WSWin, contains(".Career"),
      contains("Running"), Win.Percent, team.chg, nameFirst, nameLast
    ) %>%
    distinct %>% 
    mutate(
      team.chg = ifelse(team.chg == 1, TRUE, FALSE),
      nameFull = paste(nameFirst, nameLast, sep = " ")
    )  
}


#Create dataframe of years to periods
{
  periods <- data.frame(yearID = seq(1860, 2020, 1)) %>% 
    as_tibble() %>% 
    mutate(
      Period = case_when(
        yearID < 1920 ~ "Deadball era\n<1920",
        yearID < 1975 ~ "Pre-Sabermetrics\n1920-1975",
        #yearID < 1997 ~ "Early Sabermetrics\n1975-1997",
        TRUE ~ "Post Sabermetrics\n>1975"
      ),
      Period = factor(
        Period,
        levels = c(
          "Deadball era\n<1920",
          "Pre-Sabermetrics\n1920-1975",
          "Post Sabermetrics\n>1975"
        )
      )
    ) %>% 
    mutate(
      Period.detail = case_when(
        yearID < 1920 ~ "Deadball era\n<1920",
        yearID < 1975 ~ "Pre-Sabermetrics\n1920-1975",
        yearID < 1996 ~ "Early Sabermetrics\n1975-1996",
        TRUE ~ "Money Ball\n>1996"
      ),
      Period.detail = factor(
        Period.detail,
        levels = c(
          "Deadball era\n<1920",
          "Pre-Sabermetrics\n1920-1975",
          "Early Sabermetrics\n1975-1996", "Money Ball\n>1996"
        )
      )
    )  
}


#Get percent of WS championships per team by period
{
  ws.win.proportion <- wins.ts %>% 
    left_join(
      x = ., y = periods, by = "yearID"
    ) %>% 
    # mutate(
    #   # Period = factor(
    #   #   Period,
    #   #   levels = c("Deadball era\n<1920", "Pre-Sabermetrics\n1920-1975",
    #   #              "Early Sabermetrics\n1975-1997", "Post Sabermetrics\n>1975")
    #   # )
    #   Period = Period.detail
    # ) %>%
    filter(WSWin == 1) %>% 
    group_by(Period) %>% 
    add_count(name = "wins.total") %>% 
    group_by(Period, name) %>% 
    add_count(name = "wins") %>% 
    select(Period, name, wins.total, wins) %>% 
    distinct() %>%
    mutate(WSWin.prop = wins / wins.total) %>% 
    group_by(Period) %>% 
    arrange(desc(WSWin.prop)) %>% 
    mutate(
      prop = WSWin.prop * 100,
      ypos = cumsum(prop)- 0.5*prop
    )
  
}


#Get total salaries paid per year by team
#Get percentiles then scale
{
  salary.by.team <- Salaries %>% 
    # group_by(yearID) %>%
    # mutate(salary = scale(salary)) %>%
    group_by(yearID, teamID) %>% 
    summarize(
      salary.total = sum(salary),
      salary.avg = mean(salary),
      salary.sd = sd(salary),
      salary.95p = quantile(salary, .95),
      salary.90p = quantile(salary, .9),
      salary.75p = quantile(salary, .75),
      salary.50p = quantile(salary, .5),
      salary.25p = quantile(salary, .25),
      salary.10p = quantile(salary, .1),
      salary.iqr = IQR(salary),
      salary.skew = moments::skewness(salary),
      salary.kurtosis = moments::kurtosis(salary)
    ) %>% 
    group_by(yearID) %>%
    mutate(across(
      .cols = c(
        salary.total, salary.avg, salary.sd, salary.skew, 
        salary.kurtosis, salary.90p, salary.95p,
        salary.75p, salary.50p, salary.25p, salary.10p, salary.iqr
      ),
      .fns = function(x){scale(x, scale = T)}
    )) %>%
    left_join(
      x = .,
      y = Teams,
      by = c("yearID", "teamID")
    ) %>% 
    left_join(
      x = .,
      y = periods,
      by = "yearID"
    ) 
}


#Get total salaries paid per year by team 
#scale salaries then get percentiles
{
  salary.by.team.2 <- Salaries %>% 
    group_by(yearID) %>%
    mutate(salary = scale(salary)) %>%
    group_by(yearID, teamID) %>% 
    summarize(
      salary.total = sum(salary),
      salary.avg = mean(salary),
      salary.sd = sd(salary),
      salary.95p = quantile(salary, .95),
      salary.90p = quantile(salary, .9),
      salary.75p = quantile(salary, .75),
      salary.50p = quantile(salary, .5),
      salary.25p = quantile(salary, .25),
      salary.10p = quantile(salary, .1),
      salary.iqr = IQR(salary),
      salary.skew = moments::skewness(salary),
      salary.kurtosis = moments::kurtosis(salary)
    ) %>% 
    # group_by(yearID) %>%
    # mutate(across(
    #   .cols = c(
    #     salary.total, salary.avg, salary.sd, salary.skew, 
    #     salary.kurtosis, salary.90p, salary.95p,
    #     salary.75p, salary.50p, salary.25p, salary.10p, salary.iqr
    #   ),
    #   .fns = function(x){scale(x, scale = T)}
    # )) %>%
    left_join(
      x = .,
      y = Teams,
      by = c("yearID", "teamID")
    ) %>% 
    left_join(
      x = .,
      y = periods,
      by = "yearID"
    ) 
}









