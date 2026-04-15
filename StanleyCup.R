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


stats$Code <- team_map_names[stats$Team]

matchups <- c("COL","LAK", 
              "DAL","MIN",
              "VGK","UTA",
              "EDM","ANA",
              "BUF","BOS",
              "TBL","MTL",
              "CAR", "OTT",
              "PIT", "PHI")


playoff_teams <- matchups  # 16 unique teams in order
team_map <- 1:length(playoff_teams)
names(team_map) <- playoff_teams
reverse_team_map <- playoff_teams
names(reverse_team_map) <- team_map

stats_mat <- as.matrix(stats[match(playoff_teams, stats$Code), c("GP", "GF", "GA")])
stats_mat <- cbind(stats_mat, "POGP" = rep(0, 16), "POW" = rep(0,16))
rownames(stats_mat) <- playoff_teams


n_teams <- length(playoff_teams)
team_names <- playoff_teams

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
  return(list(scores = c(team1score, team2score, team1score < team2score), stats = stats))
}

SimulateSeries <- function(t1, t2, stats){
  win_t1 <- 0
  win_t2 <- 0
  while (win_t1<4 & win_t2<4) {
    #need to figure out way to continuously update stats matrix, cause it's not doing so here
    result <- SimulateGame(t1, t2, stats)
    stats <- result$stats
    scores <- result$scores
    stats[t1, 2] <- stats[t1, 2] + scores[1]
    stats[t1, 3] <- stats[t1, 3] + scores[2]
    stats[t2, 2] <- stats[t2, 2] + scores[2]
    stats[t2, 3] <- stats[t2, 3] + scores[1]
    
    stats[t1, 4] <- stats[t1, 4] + 1
    stats[t2, 4] <- stats[t2, 4] + 1
    
    if(result$scores[3] == FALSE){
      win_t1 <- win_t1 + 1
      stats[t1, 5] <- stats[t1, 5] + 1
    }
    else{
      win_t2 <- win_t2 + 1
      stats[t2, 5] <- stats[t2, 5] + 1
    }
    
  }
  if(win_t1>win_t2){winner <- t1}
  else{winner <- t2}
  return(list(winner = winner, stats = stats))

}

SimulatePlayoffs <- function(iterations){
  win_counts <- rep(0, n_teams)
  for(i in 1:iterations){
    current_stats <- stats_mat
    current_matchups <- matchups
    for(j in 1:14){
      winner <- SimulateSeries(team_map[current_matchups[2*j - 1]], 
                               team_map[current_matchups[2*j]],
                               current_stats)
      current_stats <- winner$stats
      current_matchups <- c(current_matchups, reverse_team_map[winner$winner])
    }
    champion <- SimulateSeries(team_map[current_matchups[29]], team_map[current_matchups[30]], current_stats)
    win_counts[champion$winner] <- win_counts[champion$winner] + 1
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
