library(strucchange)


#Get salary data and use Yankees as example
{
  data <- salary.by.team %>%
    select(
      yearID, teamID, "salary.total",
      W, G, L
    ) %>% 
    pivot_longer(
      -c(yearID, teamID, W, G, L),
      names_to = "Percentile", values_to = "Salary",
      names_prefix = "salary."
    ) %>% 
    filter(Percentile == "total") %>% 
    group_by(yearID) %>% 
    mutate(
      win.percentage = W / G
    ) %>% 
    filter(teamID == "NYA")
}


#Chow test to determine a structural break in 1996
sctest(
  data$win.percentage ~ data$Salary,
  type = "Chow", point = 12
)

