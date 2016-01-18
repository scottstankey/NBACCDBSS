ScoringWindowAverage = function(days = 10, oneseason = T, data = allseasons, 
                                player = "201149", gamedate = "20151219", 
                                removeifless = F)
{
  inds = intersect(which(data$PLAYER_ID == player), which(data$GAME_DATE < gamedate))
  year = substr(gamedate,4,4)
  inds = if(oneseason == T) {intersect(which(substr(data$SEASON_ID,5,5) == year),inds)}
  tmp = data[inds,]
  tmp = tmp[order(tmp$GAME_DATE, decreasing = T),]
  outp = ifelse((removeifless = T && nrow(tmp) < days),NA,sum(tmp[1:days,"PTS"]) / days)
  return(outp)
}