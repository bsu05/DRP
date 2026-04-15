setwd("/Users/bsu/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/BSM_DATA/DRP")
library(readxl)
library(openxlsx)
library(ggplot2)

date <- as.character(Sys.Date())
wb <- loadWorkbook("StanleyCup.xlsx")

stats <- read_excel("Summary.xlsx")
team_map_names <- c(
  "Colorado Avalanche" = "COL", "Dallas Stars" = "DAL", "Carolina Hurricanes" = "CAR",
  "Buffalo Sabres" = "BUF", "Minnesota Wild" = "MIN", "Tampa Bay Lightning" = "TBL",
  "Pittsburgh Penguins" = "PIT", "Montréal Canadiens" = "MTL", "Boston Bruins" = "BOS",
  "Detroit Red Wings" = "DET", "Columbus Blue Jackets" = "CBJ", "New York Islanders" = "NYI",
  "Ottawa Senators" = "OTT", "Anaheim Ducks" = "ANA", "Philadelphia Flyers" = "PHI",
  "Utah Mammoth" = "UTA", "Edmonton Oilers" = "EDM", "Washington Capitals" = "WSH",
  "Vegas Golden Knights" = "VGK", "New Jersey Devils" = "NJD", "Los Angeles Kings" = "LAK",
  "Florida Panthers" = "FLA", "Seattle Kraken" = "SEA", "Nashville Predators" = "NSH",
  "San Jose Sharks" = "SJS", "Toronto Maple Leafs" = "TOR", "Winnipeg Jets" = "WPG",
  "St. Louis Blues" = "STL", "Chicago Blackhawks" = "CHI", "New York Rangers" = "NYR",
  "Calgary Flames" = "CGY", "Vancouver Canucks" = "VAN"
)
stats2 <- stats[, c("Team", "GP", "W", "L", "OT", "P", "GF", "GA")]
stats2$Code <- team_map_names[stats2$Team]


team_names <- stats2$Code
n_teams <- length(team_names)
team_map <- 1:n_teams
names(team_map) <- team_names
reverse_team_map <- names(team_map)
names(reverse_team_map) <- team_map

stats_mat <- as.matrix(stats2[, c("GP", "GF", "GA", "P")])

SimulateGame <- function(t1, t2, stats){
  OT = 0
  
  league_goala <- mean(stats[, 2] / stats[, 1])
  
  team1att <- stats[t1, 2] / stats[t1, 1] / league_goala
  team1def <- stats[t1, 3] / stats[t1, 1] / league_goala
  
  team2att <- stats[t2, 2] / stats[t2, 1] / league_goala
  team2def <- stats[t2, 3] / stats[t2, 1] / league_goala
  
  team1xG <- team1att * team2def * league_goala
  team2xG <- team2att * team1def * league_goala
  
  team1score <- rpois(1, team1xG)
  team2score <- rpois(1, team2xG)
  
  #implement OT
  if(team1score == team2score){
    OT = 1
    rate1 <- team1xG / 60
    rate2 <- team2xG / 60
    
    team1OT <- rexp(1, rate = rate1)
    team2OT <- rexp(1, rate = rate2)
    
    if(min(team1OT, team2OT) <5){
      if(team1OT < team2OT){
        team1score <- team1score + 1
      }
      else{
        team2score <- team2score + 1
      }
    }
    else{
      coin <- runif(n=1)
      if(coin <= 0.5){
        team1score <- team1score + 1
      }
      else{
        team2score <- team2score + 1
      }
    }
    
  }
  return(team1score<team2score)
}

SimulateSeries <- function(t1, t2){
  win_t1 <- 0
  win_t2 <- 0
  while (win_t1<4 & win_t2<4) {
    #need to figure out way to continuously update stats matrix, cause it's not doing so here
    result <- SimulateGame(t1, t2, stats_mat)
    if(result == FALSE){
      win_t1 <- win_t1 + 1
    }
    else{
      win_t2 <- win_t2 + 1
    }
    
  }
  if(win_t1>win_t2){return(t1)}
  else{return(t2)}

}

matchups <- c("COL","LAK", 
              "DAL","MIN",
              "VGK","UTA",
              "EDM","ANA",
              "BUF","BOS",
              "TBL","MTL",
              "CAR", "OTT",
              "PIT", "PHI"
              )

SimulatePlayoffs <- function(iterations){
  win_counts <- rep(0, n_teams)
  for(i in 1:iterations){
    for(j in 1:14){
      winner <- SimulateSeries(team_map[matchups[2*j - 1]], team_map[matchups[2*j]])
      matchups <- c(matchups, reverse_team_map[winner])
    }
    champion <- SimulateSeries(team_map[matchups[29]], team_map[matchups[30]])
    win_counts[champion] <- win_counts[champion] + 1
    matchups <- c("COL","LAK", 
                  "DAL","MIN",
                  "VGK","UTA",
                  "EDM","ANA",
                  "BUF","BOS",
                  "TBL","MTL",
                  "CAR", "OTT",
                  "PIT", "PHI")
    
  }
  results_df <- data.frame(
    Team = team_names,
    StanleyCupWins = win_counts,
    WinPercentage = (win_counts / iterations) * 100
  )
  
  return(results_df[order(-results_df$StanleyCupWins), ])
}


addWorksheet(wb, date)
writeData(wb, date, SimulatePlayoffs(10000))
saveWorkbook(wb, "StanleyCup.xlsx", overwrite = TRUE)
