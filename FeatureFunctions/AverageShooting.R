AverageShooting = function(days = 10, oneseason = T, data = allseasons, 
                           player = "201149", gamedate = "20151219", seasonid = "22015",
                           removeifless = F, onlyhomeoraway = T, home = 1,
                           oneopponent = F, opponent = "BOS", ewma = F, ewmalookback = 5)
{
  
  source("/Users/scottstankey/GitHub/NBACCDBSS/FeatureFunctions/SelectGamesHelper.R")
  source("/Users/scottstankey/GitHub/NBACCDBSS/dateconv.R")
  tmp = SelectGames(days, oneseason, data, 
                    player, gamedate, seasonid,
                    removeifless, onlyhomeoraway, home,
                    oneopponent, opponent)
if(nrow(tmp) > 0)
{
  

  ptsstat = ifelse((removeifless == T && nrow(tmp) < days),
                NA,
                ifelse(ewma == T,
                       EMA(tmp[,"PTS"], n = ewmalookback)[days],
                       sum(tmp[,"PTS"],na.rm = T) / nrow(tmp)))
  ftsstat = ifelse((removeifless == T && nrow(tmp) < days),
                NA,
                ifelse(ewma == T,
                       EMA(tmp[,"FTM"], n = ewmalookback)[days],
                       sum(tmp[,"FTM"] / nrow(tmp), na.rm = T)))
  ftpercstat = ifelse((removeifless == T && nrow(tmp) < days),
                             NA,
                             #Might want to change this --- not the best calc
                             ifelse(ewma == T,
                                    EMA(tmp[,"FT_PCT"], n = ewmalookback)[days],
                                    (sum(tmp[,"FTM"] / tmp[,"FTA"], na.rm = T)) / nrow(tmp)))
  fgsstat = ifelse((removeifless == T && nrow(tmp) < days),
                              NA,
                              ifelse(ewma == T,
                                     EMA(tmp[,"FGA"], n = ewmalookback)[days],
                                     sum(tmp[,"FGA"] / nrow(tmp), na.rm = T)))
  {fgpercstat = ifelse((removeifless == T && nrow(tmp) < days),
                                 NA,
                                 #Might want to change this --- not the best calc
                                 ifelse(ewma == T,
                                        EMA(tmp[,"FG_PCT"], n = ewmalookback)[days],
                                        sum(tmp[,"FGM"] / tmp[,"FGA"], na.rm = T) / nrow(tmp)))}
  {threefgsstat = ifelse((removeifless == T && nrow(tmp) < days),
                              NA,
                              ifelse(ewma == T,
                                     EMA(tmp[,"FG3A"], n = ewmalookback)[days],
                                     sum(tmp[,"FG3A"], na.rm = T)  / nrow(tmp)))}
  {threefgpercstat = ifelse((removeifless == T && nrow(tmp) < days),
                                 NA,
                                 #Might want to change this --- not the best calc
                                 ifelse(ewma == T,
                                        EMA(tmp[,"FG3_PCT"], n = ewmalookback)[days],
                                        sum(tmp[,"FG3M"] / tmp[,"FG3A"], na.rm = T) / nrow(tmp)))} 
  {rebsstat = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"REB"], n = ewmalookback)[days],
                                          sum(tmp[,"REB"], na.rm = T) / nrow(tmp)))}
  {orebsstat = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"OREB"], n = ewmalookback)[days],
                                          sum(tmp[,"OREB"], na.rm = T) / nrow(tmp)))}
  {drebsstat = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"DREB"], n = ewmalookback)[days],
                                          sum(tmp[,"DREB"], na.rm = T) / nrow(tmp)))}
  {assistsstat = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"AST"], n = ewmalookback)[days],
                                          sum(tmp[,"AST"], na.rm = T) / nrow(tmp)))}
  {tosstat = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"TOV"], n = ewmalookback)[days],
                                          sum(tmp[,"TOV"], na.rm = T) / nrow(tmp)))}
  {stealsstat = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"STL"], n = ewmalookback)[days],
                                          sum(tmp[,"STL"], na.rm = T) / nrow(tmp)))}
  {blocksstat = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"BLK"], n = ewmalookback)[days],
                                          sum(tmp[,"BLK"], na.rm = T) / nrow(tmp)))}
  {pmsstat = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"PLUS_MINUS"], n = ewmalookback)[days],
                                          sum(tmp[,"PLUS_MINUS"], na.rm = T) / nrow(tmp)))}
  {minsstat = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"MIN"], n = ewmalookback)[days],
                                          sum(tmp[,"MIN"], na.rm = T) / nrow(tmp)))}
  {foulsstat = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"PF"], n = ewmalookback)[days],
                                          sum(tmp[,"PF"], na.rm = T) / nrow(tmp)))}
  {EFSstat = ifelse((removeifless == T && nrow(tmp) < days),
                             NA,
                             ifelse(ewma == T,
                                    EMA((tmp[,"FGM"] + .5 * tmp[,"FG3M"]) / tmp[,"FGA"], n = ewmalookback)[days],
                                    sum(((tmp[,"FGM"] + .5 * tmp[,"FG3M"]) / tmp[,"FGA"]), na.rm = T) / nrow(tmp)))}
  {winpercstat = ifelse((removeifless == T && nrow(tmp) < days),
                                NA,
                                length(which(tmp$WL == "W")) / nrow(tmp))}
  #{reststat = dateconv(as.numeric(tmp$GAME_DATE[1])) - dateconv(as.numeric(tmp$GAME_DATE[2]))}
  #{lasttwoweeksstat = (length(which(dateconv(as.numeric(tmp$GAME_DATE[1])) - 
                                              # sapply((as.numeric(tmp$GAME_DATE)), dateconv) < 15)) - 1)}
  outp = c(ptsstat, ftsstat, ftpercstat,
           fgsstat, fgpercstat,
           threefgsstat, threefgpercstat,
           rebsstat, orebsstat, drebsstat, assistsstat, tosstat, 
           stealsstat, blocksstat, pmsstat, minsstat, foulsstat, EFSstat, lasttwoweeksstat)
  return(outp)
} else return (c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
}