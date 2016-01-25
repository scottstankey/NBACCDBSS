
teamWindowAverage = function(days = 10, ewmalookback = 5, oneseason = T, data = allseasons, 
                                player = "201149", gamedate = "20151219", season_ID = "22015"
                                removeifless = F, ewma = T)
{
  #games to search
  source("~/NBACCDBSS/FeatureFunctions/SelectGamesHelper.R")
  data2 = SelectGames(days, oneseason, data, 
                      player, gamedate, season_ID,
                      removeifless, F, 1,
                       F, "BOS")
  team = data2$TEAM_ABBREVIATION[1]
  data3 = data2$GAME_ID
  
  #find all other players in those games
  tmp = data[which(data$GAME_ID %in% data3), ]
  
  teamGames = unique(tmp$GAME_ID)
  possessions = NULL
  orating = NULL
  drating = NULL
  
  for(game in teamGames)
  {
    playersInGame = which(tmp$GAME_ID == game)
    possessionsInGame = 0
    points = 0
    oppPoints = 0
    for(p in playersInGame)
    {
      possessionsInGame = possessionsInGame + tmp$FGA[p] + tmp$TOV[p] + (.44 * tmp$FTA[p]) - tmp$OREB[p]
      if(tmp$TEAM_ABBREVIATION[p] == team)
      {
        points = points + tmp$PTS[p]
      }
      else
      {
        oppPoints = oppPoints + tmp$PTS[p]
      }
    }
    possessionsInGame = possessionsInGame /2
    possessions = c(possessions, possessionsInGame)
    orating = c(orating, (points / possessionsInGame))
    drating = c(drating, (oppPoints / possessionsInGame))
  }
  
  if(removeifless == T && nrow(data2) < days){
    outp = NA
  }else if(ewma == T){
    outp = c(EMA(possessions, n = ewmalookback)[days], EMA(orating, n = ewmalookback)[days], EMA(drating, n = ewmalookback)[days])
  }else{
    outp = c(mean(possessions), mean(orating), mean(drating))
  }
    
  return(outp)
}

