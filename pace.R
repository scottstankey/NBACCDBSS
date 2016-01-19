
PaceWindowAverage = function(days = 10, oneseason = T, data = allseasons, 
                                player = "201149", team = "CHI", gamedate = "20151219", season_ID = "22015"
                                removeifless = F)
{
#   inds = intersect(which(data$TEAM_ABBREVIATION == team), which(data$GAME_DATE < gamedate))
#   inds = if(oneseason == T) {intersect(which(substr(data$SEASON_ID,5,5) == year),inds)}
#   tmp = data[inds,]
  
  tmp = data[intersect(which(data$TEAM_ABBREVIATION == team), 
                       which(data$SEASON_ID == season_ID), 
                       which(data$GAME_DATE < gamedate)),]
  
  games = unique(tmp$GAME_ID)
  possessions = NULL
  
  for(game in games)
  {
    playersInGame = which(tmp$GAME_ID == game)
    possessionsInGame = 0
    for(player in playersInGame)
    {
      possessionsInGame = possessionsInGame + tmp$FGA[player] + tmp$TOV[player] + (.44 * tmp$FTA[player])
    }
    possessions = c(possessions, possessionsInGame)
  }
  
  outp = mean(possessions)
    
  return(outp)
}

