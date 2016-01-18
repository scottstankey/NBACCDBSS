ScoringWindowAverage = function(days = 10, oneseason = T, data = allseasons, player = "201149", gamedate = "20151219", removeifless = F) {
  inds = intersect(which(data$PLAYER_ID == player), which(data$GAME_DATE < gamedate))
  tmp = data[inds,]
  tmp = tmp[order(tmp$GAME_DATE, decreasing = T),]
  if(removeifless = F &&)
  return(sum(tmp[1:days,"PTS"]) / days)
}