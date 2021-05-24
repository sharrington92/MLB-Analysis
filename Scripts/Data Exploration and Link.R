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

##Data source
  # http://www.seanlahman.com/baseball-archive/statistics
  # https://github.com/cdalzell/Lahman
##Documentation
  # http://www.seanlahman.com/files/database/readme2017.txt
##Main Tables:
  # Batting, People, Fielding, Pitching
##Supplementary Tables:
  # AllStarFull, HallofFame, Managers, Teams, BattingPost,
  # PitchingPost, TeamFranchises, FieldingOF, FieldingPost
  # FieldingOFsplit, ManagersHalf, TeamsHalf, Salaries,
  # SeriesPost, AwardsManagers, AwardsPlayers, AwardsShareManagers,
  # AwardsSharePlayers, Appearances, Schools, CollegePlaying,
  # Parks, HomeGames

#Documentation
{
  # The database is comprised of the following main tables:
  #   
    # People - Player names, DOB, and biographical info
    # Batting - batting statistics
    # Pitching - pitching statistics
    # Fielding - fielding statistics

  # It is supplemented by these tables:
  #   
    # AllStarFull - All-Star appearances
    # HallofFame - Hall of Fame voting data
    # Managers - managerial statistics
    # Teams - yearly stats and standings
    # BattingPost - post-season batting statistics
    # PitchingPost - post-season pitching statistics
    # TeamFranchises - franchise information
    # FieldingOF - outfield position data
    # FieldingPost- post-season fielding data
    # FieldingOFsplit - LF/CF/RF splits
    # ManagersHalf - split season data for managers
    # TeamsHalf - split season data for teams
    # Salaries - player salary data
    # SeriesPost - post-season series information
    # AwardsManagers - awards won by managers
    # AwardsPlayers - awards won by players
    # AwardsShareManagers - award voting for manager awards
    # AwardsSharePlayers - award voting for player awards
    # Appearances - details on the positions a player appeared at
    # Schools - list of colleges that players attended
    # CollegePlaying - list of players and the colleges they attended
    # Parks - list of major league ballparls
    # HomeGames - Number of homegames played by each team in each ballpark
}

#Batting
{
  #Cumulative Home-runs by Team
  {
    Batting %>% 
      filter(lgID == "AL", yearID > 1989) %>% 
      group_by(yearID, teamID) %>%    
      summarize(
        HR = sum(HR),
        HR.cumulative = cumsum(HR)
      ) %>% 
      group_by(teamID) %>% 
      arrange(yearID) %>% 
      mutate(
        HR.cumulative = cumsum(HR)
      ) %>% 
      ggplot(aes(x = yearID, y = HR.cumulative, color = teamID)) +
      geom_line() 
  }
  
  #Distribution of total home runs in a year by player
  {
    Batting %>% 
      filter(lgID == "AL", yearID > 1989) %>% 
      group_by(yearID, playerID) %>% 
      summarize(
        HR = sum(HR)
      ) %>% 
      ggplot(aes(x = 0, y = HR)) +
      geom_boxplot() +
      facet_grid(. ~ yearID) +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()
      )
  }
}

##Manager impact on teams and rankings of manager success over the years
# Me
{
  View(People)
  View(Managers)
  View(AwardsManagers)
  View(AwardsShareManagers)
  View(Teams)
  
  #Best managers overall
  {
    wins.byManager <- left_join(
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
        W = sum(W),
        L = sum(L),
        DivWin = sum(DivWin),
        WCWin = sum(WCWin),
        LgWin = sum(LgWin),
        WSWin = sum(WSWin)
      )
    
    #World Series Wins
    {
      ws.winManager <- wins.byManager %>% 
        #group_by(playerID, teamID) %>% 
        group_by(playerID) %>% 
        summarize(
          year.bgn = min(year.bgn),
          year.end = max(year.end),
          W = sum(W),
          L = sum(L),
          DivWin = sum(DivWin),
          WCWin = sum(WCWin),
          LgWin = sum(LgWin),
          WSWin = sum(WSWin)
        ) %>% 
        filter(!is.na(WSWin), WSWin > 0) %>% 
        mutate(
          WSWin.Avg = WSWin / (year.end - year.bgn + 1)
        )
      
      #Time Series Chart
      {
        temp <- left_join(
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
          filter(playerID %in% ws.winManager$playerID) %>% 
          group_by(playerID) %>% 
          arrange(yearID, teamID) %>% 
          add_count() %>% 
          mutate(
            Total.WSWin = sum(WSWin),
            Running.WSWin = cumsum(WSWin),
            WSWin.Percent = Total.WSWin / n,
            team.chg = ifelse(teamID == lag(teamID), 0, 1),
            Win.Percent = W / G * 100
          ) %>% #select(yearID, playerID, teamID, team.chg)
          replace_na(list(team.chg = 1)) %>% 
          filter(
            playerID %in% (ungroup(.) %>% 
                             select(playerID, Total.WSWin) %>%
                             distinct() %>% 
                             top_n(5, Total.WSWin))$playerID
          ) %>% 
          left_join(
            x = ., y = People, by = "playerID", suffix = c("", ".y")
          ) %>% 
          left_join(
            x = ., y = Teams, by = "teamID", suffix = c("", ".y")
          ) %>% 
          select(yearID, playerID, teamID, W, WSWin, WSWin.Percent, Win.Percent,
                 Running.WSWin, Total.WSWin, team.chg, nameFirst,
                 nameLast, name) %>% 
          distinct %>% 
          mutate(
            team.chg = ifelse(team.chg == 1, TRUE, FALSE),
            nameFull = paste(nameFirst, nameLast, sep = " ")
          )
        
        temp %>% 
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
            "Top 5 Team Managers by World Series Championships",
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
        
        
      }
    }
    
    #Total Wins
    {
      # total.winManager <- Managers %>% 
      #   #group_by(playerID, teamID) %>% 
      #   group_by(playerID) %>% 
      #   summarize(
      #     year.bgn = min(year.bgn),
      #     year.end = max(year.end),
      #     W = sum(W),
      #     L = sum(L),
      #     DivWin = sum(DivWin),
      #     WCWin = sum(WCWin),
      #     LgWin = sum(LgWin),
      #     WSWin = sum(WSWin)
      #   ) %>% 
      #   filter(!is.na(WSWin), WSWin > 0) %>% 
      #   mutate(
      #     WSWin.Avg = WSWin / (year.end - year.bgn + 1)
      #   )
      
      #Highlight chart
      {
        
        total.wins <- left_join(
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
          # filter(playerID %in% (wins.byManager %>%
          #                         ungroup %>% 
          #                         top_n(10, W))$playerID) %>% 
          group_by(playerID) %>% 
          arrange(yearID, teamID) %>% 
          add_count() %>% 
          mutate(
            Total.Win = sum(W),
            Running.Win = cumsum(W),
            team.chg = ifelse(teamID == lag(teamID), 0, 1),
            Win.Percent = W / G * 100
          ) %>% #select(yearID, playerID, teamID, team.chg)
          replace_na(list(
            team.chg = 1, DivWin = 0, WCWin = 0, LgWin = 0, WSWin = 0
          )) %>% 
          left_join(
            x = ., y = People, by = "playerID", suffix = c("", ".y")
          ) %>% 
          left_join(
            x = ., y = Teams, by = "teamID", suffix = c("", ".y")
          ) %>% 
          select(yearID, playerID, teamID, lgID, W, Win.Percent, DivWin, LgWin, WSWin,
                 Running.Win, Total.Win, team.chg, nameFirst,
                 nameLast, name) %>% 
          distinct %>% 
          mutate(
            team.chg = ifelse(team.chg == 1, TRUE, FALSE),
            nameFull = paste(nameFirst, nameLast, sep = " ")
          )
        
        total.wins.top <- total.wins %>% 
          filter(playerID %in% (wins.byManager %>%
                                  ungroup %>%
                                  top_n(10, W))$playerID)
        
        total.wins %>% 
          mutate(WSWin = ifelse(WSWin == 1, TRUE, FALSE)) %>% 
          ggplot() +
          geom_line(
            aes(x = yearID, y = Running.Win, color = nameFull)
          ) +
          gghighlight(
            #aes(x = yearID, y = Running.Win, color = nameFull),
            max(Running.Win), max_highlight = 10,
            use_direct_label = FALSE
          ) +
          #facet_wrap(. ~ lgID) +
          geom_star(
            aes(
              x = yearID, y = Running.Win, starshape = as.factor(WSWin),
              fill = nameFull, size = as.factor(WSWin)
            ),
            color = "black"
          ) +
          geom_text_repel(
            aes(
              x = yearID, y = Running.Win, label = ifelse(WSWin == 1, name,"")
            ),
            #nudge_y = .25,
            size = 2, 
            color = "gray15"
            #force = .1, max.overlaps = 15,
            #direction = "x"
          ) +
          geom_dl(
            aes(
              x = yearID, y = Running.Win, color = nameFull,
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
            limits = c(0, max(total.wins.top$Running.Win)+300),
            labels = scales::comma
          ) +
          scale_x_continuous(
            n.breaks = 10,
            limits = c( min(total.wins.top$yearID), max(total.wins.top$yearID)+1)
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
        
        
        
      }
    }
  }
  
  #Best managers per money spent
  
  #Largest performance variance from manager switching teams
  
  #Most improved manager of team
  
  #Best managers for...
  {
    #Post season
    #Regular season
    #Runs scored
    #Homeruns, Doubles, Triples
    #Shutouts
    #Homeruns Allowed
    #Stolen Bases
    #Sacrifice Flies
    
  }
  
  #Managers that were previously athletes vs non
  {
    
  }
  
}

##Comparing a team's regular season statistics to its postseason statistics. 
# Me
{
  #Scatterplot of regular season wins to postseason
  {
    Teams %>% 
      mutate(
        teamYear = paste(teamID, yearID, sep = "\n"),
        Win.Percent = W / G * 100,
        DivWin = ifelse(DivWin == "Y", "Division Winner", "Division Loser"),
        LgWin = ifelse(LgWin == "Y", "League Winner", "League Loser"),
        WSWin = ifelse(WSWin == "Y", "World Series Champion", "Non-Champion")
      ) %>% 
      filter(!is.na(DivWin), !is.na(LgWin), !is.na(WSWin)) %>% 
      #ggplot(aes(x = L, y = W, color = as.factor(Rank))) +
      ggplot(aes(x = L, y = W, fill = WSWin, starshape = WSWin)) +
      geom_star(size = 3) +
      facet_grid(
        #WSWin ~ DivWin + LgWin, scales = "fixed", margins = FALSE
        LgWin ~ DivWin, scales = "fixed", margins = FALSE
      ) +
      
      #50% Win Percentage
      geom_segment( 
        aes(x = 40, xend = 100, y = 40, yend = 100),
        inherit.aes = FALSE, linetype = "dotted"
      ) + 
      geom_text(
        aes(x = 100, y = 98, label = "50% Winning Percentage"),
        inherit.aes = FALSE, angle = 43.5,
        color = "gray40", size = 3
      ) +
      
      #110 Number of Games
      geom_segment( 
        aes(x = 70, xend = 45, y = 40, yend = 65),
        inherit.aes = FALSE, linetype = "dashed"
      ) + 
      geom_text(
        aes(x = 70, y = 43, label = "110 Games Played"),
        inherit.aes = FALSE, angle = -43.5,
        color = "gray40", size = 3
      ) +
      
      #160 Number of Games
      geom_segment( 
        aes(x = 105, xend = 50, y = 55, yend = 110),
        inherit.aes = FALSE, linetype = "dashed"
      ) + 
      geom_text(
        aes(x = 59, y = 108, label = "160 Games Played"),
        inherit.aes = FALSE, angle = -43.5,
        color = "gray40", size = 3
      ) +
      scale_x_continuous(n.breaks = 4) +
      scale_y_continuous(n.breaks = 6) +
      ggtitle(
        "Post-Season Performance Against Total Record",
        "1969 - 2019"
      ) +
      xlab("Total Season Losses") +
      ylab("Total Season Wins") +
      labs(
        fill = "World Series Champion",
        starshape = "World Series Champion"
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom"
      ) +
      # scale_fill_gradient(
      #   low = "skyblue1", high = "midnightblue"
      # ) +
      scale_fill_manual(values = c("lightsteelblue3", "gold3")) +
      scale_starshape_manual(values = c(15, 1))
  }
  
  
}

##Earned/Allowed Runs correlation with championships?
{
  runs <- Pitching %>% 
    group_by(yearID, teamID, lgID) %>% 
    summarize(
      ER = sum(ER),
      R = sum(R)
    ) %>% 
    left_join(
      x = ., y = Teams, by = c("yearID", "teamID", "lgID"), suffix = c("", ".y")
    ) %>% 
    select(-contains(".y")) %>% 
    mutate(
      teamYear = paste(teamID, yearID, sep = "\n"),
      Champion = ifelse(DivWin == "Y" | LgWin == "Y" | WSWin == "Y", "Champion", "Not Champion"),
      # Champion.Type = ifelse(DivWin == "Y", "Division",
      #                        ifelse(LgWin == "Y", "League",
      #                               WSWin == "Y", "World Series", "Not Champion")),
      DivWin = ifelse(DivWin == "Y", "Division Winner", "Division Loser"),
      LgWin = ifelse(LgWin == "Y", "League Winner", "League Loser"),
      WSWin = ifelse(WSWin == "Y", "World Series Champion", "Non-Champion")
    ) %>% 
    filter(!is.na(WSWin))
  
  runs %>% 
    ggplot(aes(x = R, y = ER)) +
    #geom_smooth(method = "lm", color = "gray30", se = FALSE) +
    geom_point(color = "gray80") +
    geom_point(
      data = filter(
        runs, LgWin != "League Winner", WSWin != "World Series Champion",
        DivWin == "Division Winner"
      ),
      aes(x = R, y = ER), color = "green4", alpha = .5
    ) +
    geom_point(
      data = filter(runs, LgWin == "League Winner" & WSWin != "World Series Champion"),
      aes(x = R, y = ER), color = "blue", alpha = .5
    ) +
    geom_point(
      data = filter(runs, WSWin == "World Series Champion"),
      aes(x = R, y = ER), color = "red", alpha = .75
    ) +
    scale_x_continuous(
      limits = c(300, 1000), 
      n.breaks = 10
    ) +
    scale_y_continuous(
      limits = c(250, 850),
      n.breaks = 10
    )
    theme_bw()
    
}



##The impact of the Moneyball revolution in 2002 on team payrolls and statistics, as well as player salaries and statistics.
##The impact of the defensive shift on defensive metrics.
##The impact of Bill James' 1977 baseball abstract on statistics and payroll figures.


Salaries %>% head()
Pitching
PitchingPost
Teams %>% View
Parks %>% View

Managers %>% View
