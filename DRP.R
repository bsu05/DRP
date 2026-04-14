setwd("/Users/bsu/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/BSM_DATA/DRP")

library(readxl)
library(openxlsx)
library(ggplot2)

wb <- loadWorkbook("PresidentsTrophy.xlsx")

stats <- read_excel("Summary.xlsx")
sched <- read_excel("NHLSchedule.xlsx")

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


#switch to today's date (when in Hungary, switch to date-1 when back in US)
date <- as.character(Sys.Date())
sched <- sched[sched[["date"]] > date, ]
sched <- sched[!is.na(sched$team), c("gameId","date", "team", "opponent")]
sched <- sched[!duplicated(sched$gameId),]
sched <- sched[order(sched$date), ]

stats2 <- stats[, c("Team", "GP", "W", "L", "OT", "P", "GF", "GA")]
stats2$Code <- team_map_names[stats2$Team]


team_names <- stats2$Code
n_teams <- length(team_names)
team_map <- 1:n_teams
names(team_map) <- team_names

sched_t1 <- team_map[sched$team]
sched_t2 <- team_map[sched$opponent]

stats_mat <- as.matrix(stats2[, c("GP", "GF", "GA", "P")])

#use poisson distribution to simulate each game
SimulateGame <- function(t1, t2, stats){
  OT = 0
  
  league_goala <- mean(stats[, 2] / stats[, 1])
  
  team1att <- stats[t1, 2] / stats[t1, 1] / league_goala
  team1def <- stats[t1, 3] / stats[t1, 1] / league_goala
  
  team2att <- stats[t2, 2] / stats[t2, 1] / league_goala
  team2def <- stats[t2, 3] / stats[t2, 1] / league_goala
  
  team1xG <- team1att * team2def * league_goala * 1.05
  team2xG <- team2att * team1def * league_goala / 1.05
  
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
  
  return(c(team1score, team2score, OT))
}

#go through entire schedule and generate each game and update the stats2 after each one
GenerateSeason <- function(){
  stats_mat_season <- stats_mat
  
  #iterate through the entire schedule and update the stats of each team
  for (i in 1:length(sched_t1)){
    
    
    team1 <- sched_t1[i]
    team2 <- sched_t2[i]
    #simulate a game
    result <- SimulateGame(team1, team2, stats_mat_season)
    
    stats_mat_season[team1, 1] <- stats_mat_season[team1, 1] + 1
    stats_mat_season[team2, 1] <- stats_mat_season[team2, 1] + 1
    
    #update goals stats
    stats_mat_season[team1, 2] <- stats_mat_season[team1, 2] + result[1]
    stats_mat_season[team1, 3] <- stats_mat_season[team1, 3] + result[2]
    stats_mat_season[team2, 2] <- stats_mat_season[team2, 2] + result[2]
    stats_mat_season[team2, 3] <- stats_mat_season[team2, 3] + result[1]
    
    #update points
    if (result[1] > result[2]){
      stats_mat_season[team1, 4] <- stats_mat_season[team1, 4] + 2
      if(result[3] == 1){
        stats_mat_season[team2, 4] <- stats_mat_season[team2, 4] + 1
      }
    }
    if (result[2] > result[1]){
      stats_mat_season[team2, 4] <- stats_mat_season[team2, 4] + 2
      if(result[3] == 1){
        stats_mat_season[team1, 4] <- stats_mat_season[team1, 4] + 1
      }
    }
    
    
  }
 return(stats_mat_season)
}
GenerateSeason()


PresTrophy <- function(iterations){
  win_counts <- rep(0, n_teams)
  
  for(i in 1:iterations){
    season_results <- GenerateSeason()
    
    winner_index <- which.max(season_results[, 4])
    win_counts[winner_index] <- win_counts[winner_index] + 1
  }
  
  results_df <- data.frame(
    Team = team_names,
    PresTrophyWins = win_counts,
    WinPercentage = (win_counts / iterations) * 100
  )
  
  return(results_df[order(-results_df$PresTrophyWins), ])
}

#important: 27.1s for 10,000 iteration Pres Trophy (before 72 minutes)


addWorksheet(wb, date)
writeData(wb, date, PresTrophy(10000))
saveWorkbook(wb, "PresidentsTrophy.xlsx", overwrite = TRUE)


#add Stanley Cup Playoffs next

stats_atl
stats_met
stats_cen
stats_pac

