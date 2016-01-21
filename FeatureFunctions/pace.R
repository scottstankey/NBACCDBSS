
PaceWindowAverage = function(days = 10, oneseason = T, data = allseasons, 
                                player = "201149", team = "CHI", gamedate = "20151219", season_ID = "22015"
                                removeifless = F)
{
  #games to search
  source("~/NBACCDBSS/FeatureFunctions/SelectGamesHelper.R")
  data2 = SelectGames(days, oneseason, data, 
                      player, gamedate, season_ID,
                      removeifless, F, 1,
                       F, "BOS")
  data3 = data2$GAME_ID
  
  #find all other players in those games
  tmp = data[which(data$GAME_ID %in% data3), ]
  
  games = unique(tmp$GAME_ID)
  possessions = NULL
  
  for(game in games)
  {
    playersInGame = which(tmp$GAME_ID == game)
    possessionsInGame = 0
    for(p in playersInGame)
    {
      possessionsInGame = possessionsInGame + tmp$FGA[p] + tmp$TOV[p] + (.44 * tmp$FTA[p]) - tmp$OREB[p]
    }
    possessions = c(possessions, possessionsInGame)
  }
  
  outp = mean(possessions) / 2
    
  return(outp)
}

