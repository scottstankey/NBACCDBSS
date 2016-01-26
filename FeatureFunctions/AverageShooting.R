AverageShooting = function(days = 10, oneseason = T, data = allseasons, 
                           player = "201149", gamedate = "20151219", seasonid = "22015",
                           removeifless = F, onlyhomeoraway = T, home = 1,
                           oneopponent = F, opponent = "BOS", ewma = F, ewmalookback = 5,
                           fts = F, ftperc = F,
                           fgs = F, fgperc = F,
                           threefgs = F, threefgperc = F,
                           rebs = F, orebs = F, drebs = F, assists = F, tos = T, 
                           steals = F, blocks = F, pms = F, mins = F, fouls = F, EFS = F,
                           winperc = F, rest = F, lasttwoweeks = F)
{
  
  source("/Users/scottstankey/GitHub/NBACCDBSS/FeatureFunctions/SelectGamesHelper.R")
  source("/Users/scottstankey/GitHub/NBACCDBSS/FeatureFunctions/dateconv.R")
  tmp = SelectGames(days, oneseason, data, 
                    player, gamedate, seasonid,
                    removeifless, onlyhomeoraway, home,
                    oneopponent, opponent)
  tmp = as.data.frame(tmp)
  tmp[is.na(tmp)] = 0
  tmp[is.nan(tmp)] = 0

  {ftsstat = ifelse((removeifless == T && nrow(tmp) < days),
                NA,
                ifelse(ewma == T,
                       EMA(tmp[,"FTM"], n = ewmalookback)[days],
                       sum(tmp[,"FTM"] / days), na.rm = T))}
  {ftpercstat = ifelse((removeifless == T && nrow(tmp) < days),
                             NA,
                             #Might want to change this --- not the best calc
                             ifelse(ewma == T,
                                    EMA(tmp[,"FT_PCT"], n = ewmalookback)[days],
                                    (sum(tmp[,"FTM"] / tmp[,"FTA"], na.rm = T)) / days)}
  {fgsstat = ifelse((removeifless == T && nrow(tmp) < days),
                              NA,
                              ifelse(ewma == T,
                                     EMA(tmp[,"FGA"], n = ewmalookback)[days],
                                     sum(tmp[,"FGA"] / days, na.rm = T)))}
  {fgpercstat = ifelse((removeifless == T && nrow(tmp) < days),
                                 NA,
                                 #Might want to change this --- not the best calc
                                 ifelse(ewma == T,
                                        EMA(tmp[,"FG_PCT"], n = ewmalookback)[days],
                                        sum(tmp[,"FGM"] / tmp[,"FGA"], na.rm = T) / days))}
  {threefgsstat = ifelse((removeifless == T && nrow(tmp) < days),
                              NA,
                              ifelse(ewma == T,
                                     EMA(tmp[,"FG3A"], n = ewmalookback)[days],
                                     sum(tmp[,"FG3A"], na.rm = T)  / days))}
  {threefgpercstat = ifelse((removeifless == T && nrow(tmp) < days),
                                 NA,
                                 #Might want to change this --- not the best calc
                                 ifelse(ewma == T,
                                        EMA(tmp[,"FG3_PCT"], n = ewmalookback)[days],
                                        sum(tmp[,"FG3M"] / tmp[,"FG3A"], na.rm = T) / days))} 
  {rebsstat = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"REB"], n = ewmalookback)[days],
                                          sum(tmp[,"REB"], na.rm = T) / days))}
  {orebsstat = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"OREB"], n = ewmalookback)[days],
                                          sum(tmp[,"OREB"], na.rm = T) / days))}
  {drebsstat = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"DREB"], n = ewmalookback)[days],
                                          sum(tmp[,"DREB"], na.rm = T) / days))}
  {assistsstat = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"AST"], n = ewmalookback)[days],
                                          sum(tmp[,"AST"], na.rm = T) / days))}
  {tosstat = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"TOV"], n = ewmalookback)[days],
                                          sum(tmp[,"TOV"], na.rm = T) / days))}
  {stealsstat = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"STL"], n = ewmalookback)[days],
                                          sum(tmp[,"STL"], na.rm = T) / days))}
  {blocksstat = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"BLK"], n = ewmalookback)[days],
                                          sum(tmp[,"BLK"], na.rm = T) / days))}
  {pmsstat = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"PLUS_MINUS"], n = ewmalookback)[days],
                                          sum(tmp[,"PLUS_MINUS"], na.rm = T) / days))}
  {minsstat = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"MIN"], n = ewmalookback)[days],
                                          sum(tmp[,"MIN"], na.rm = T) / days))}
  {foulsstat = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"PF"], n = ewmalookback)[days],
                                          sum(tmp[,"PF"], na.rm = T) / days))}
  {EFSstat = ifelse((removeifless == T && nrow(tmp) < days),
                             NA,
                             ifelse(ewma == T,
                                    EMA((tmp[,"FGM"] + .5 * tmp[,"FG3M"]) / tmp[,"FGA"], n = ewmalookback)[days],
                                    sum(((tmp[,"FGM"] + .5 * tmp[,"FG3M"]) / tmp[,"FGA"]), na.rm = T) / days))}
  {winpercstat = ifelse((removeifless == T && nrow(tmp) < days),
                                NA,
                                length(which(tmp$WL == "W")) / nrow(tmp))}
  {reststat = dateconv(as.numeric(tmp$GAME_DATE[1])) - dateconv(as.numeric(tmp$GAME_DATE[2]))}
  {lasttwoweeksstat = (length(which(dateconv(as.numeric(tmp$GAME_DATE[1])) - 
                                               sapply((as.numeric(tmp$GAME_DATE)), dateconv) < 15)) - 1)}
  outp = c(ftsstat, ftpercstat,
           fgsstat, fgpercstat,
           threefgsstat, threefgpercstat,
           rebsstat, orebsstat, drebsstat, assistsstat, tosstat, 
           stealsstat, blocksstat, pmsstat, minsstat, foulsstat, EFSstat,
           winpercstat, reststat, lasttwoweeksstat)
  return(outp)
}