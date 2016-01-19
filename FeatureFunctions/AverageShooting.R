AverageShooting = function(days = 10, oneseason = T, data = allseasons, 
                           player = "201149", gamedate = "20151219", season = "2015-16",
                           removeifless = F, ewma = T, ewmalookback = 5, onlyhomeoraway = T, home = 1,
                           oneopponent = F, opponent = "BOS")
{
  ############ Replace
  inds = intersect(which(data$PLAYER_ID == player), which(data$GAME_DATE < gamedate))
  if(oneseason == T)
  {
    inds = intersect(which(data$SEASON_ID == seasonid),inds)
  }
  if(onlyhomeoraway == T){inds = intersect(which(data$HOME == home),inds)}
  if(oneopponent == T){intersect(which(data$OPPONENT == opponent),inds)}
  tmp = data[inds,]
  tmp = tmp[order(tmp$GAME_DATE, decreasing = T),]
}