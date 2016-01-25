OppPaceWindowAverage = function(days = 100, oneseason = T, data = allseasons, 
                             player = "201149", team = "CHI", opp = "UTA", gamedate = "20151219", season_ID = "22015",
                             removeifless = F)
{
  oppplayer = data[which(data$OPPONENT == opp), ]$PLAYER_ID[1]
  #games to search
  source("~/NBACCDBSS/FeatureFunctions/SelectGamesHelper.R")
  data2 = SelectGames(days, oneseason, data, 
                      oppplayer, gamedate, season_ID,
                      removeifless, F, 1,
                      F, "BOS")
  data3 = data2$GAME_ID
  
  #find all other players in those games
  tmp = data[which(data$GAME_ID %in% data3), ]
  
  teamGames = unique(tmp$GAME_ID)
  possessions = NULL
  drating = NULL
  
  for(game in teamGames)
  {
    playersInGame = which(tmp$GAME_ID == game)
    possessionsInGame = 0
    points = 0
    for(p in playersInGame)
    {
      possessionsInGame = possessionsInGame + tmp$FGA[p] + tmp$TOV[p] + (.44 * tmp$FTA[p]) - tmp$OREB[p]
      points = points + tmp$PTS[p]
    }
    possessions = c(possessions, possessionsInGame)
    orating = c(orating, (points / possessionsInGame))
  }
  
  outp = mean(possessions) / 2
  
  return(outp)
}
