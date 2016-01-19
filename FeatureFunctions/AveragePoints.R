#requires library TTR

ScoringWindowAverage = function(days = 10, oneseason = T, data = allseasons, 
                                player = "201149", gamedate = "20151219", seasonid = "22015",
                                removeifless = F, ewma = T, ewmalookback = 5, onlyhomeoraway = T, home = 1,
                                oneopponent = F, opponent = "BOS")
{
  source("/Users/scottstankey/GitHub/NBACCDBSS/FeatureFunctions/SelectGamesHelper.R")
  tmp = SelectGames(days, oneseason, data, 
                  player, gamedate, seasonid,
                  removeifless, onlyhomeoraway, home,
                  oneopponent, opponent)
  outp = ifelse((removeifless == T && nrow(tmp) < days),
                NA,
                ifelse(ewma == T,
                       EMA(tmp[1:days,"PTS"], n = ewmalookback)[days],
                       sum(tmp[1:days,"PTS"] / days)))
  return(outp)
}
