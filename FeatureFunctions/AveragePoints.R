#requires library TTR

ScoringWindowAverage = function(days = 10, oneseason = T, data = allseasons, 
                                player = "201149", gamedate = "20151219", season = "2015-16",
                                removeifless = F, ewma = T, ewmalookback = 5, onlyhomeoraway = T, home = 1,
                                oneopponent = F, opponent = "BOS")
{
  inds = intersect(which(data$PLAYER_ID == player), which(data$GAME_DATE < gamedate))
  year = substr(season,4,4)
  if(oneseason == T)
    {
    inds = intersect(which(substr(data$SEASON_ID,5,5) == year),inds)
    }
  if(onlyhomeoraway == T){inds = intersect(which(data$HOME == home),inds)}
  if(oneopponent == T){intersect(which(data$OPPONENT == opponent),inds)}
  tmp = data[inds,]
  tmp = tmp[order(tmp$GAME_DATE, decreasing = T),]
  outp = ifelse((removeifless == T && nrow(tmp) < days),NA,
                ifelse(ewma == T,EMA(tmp[1:days,"PTS"], n = ewmalookback)[days],
                      sum(tmp[1:days,"PTS"] / days)))
  return(outp)
}
