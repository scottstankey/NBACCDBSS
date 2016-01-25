AverageShooting = function(days = 10, oneseason = T, data = allseasons, 
                           player = "201149", gamedate = "20151219", seasonid = "22015",
                           removeifless = F, onlyhomeoraway = T, home = 1,
                           oneopponent = F, opponent = "BOS", ewma = F, ewmalookback = 5,
                           fts = F, ftperc = F,
                           fgs = F, fgperc = F,
                           threefgs = F, threefgperc = F,
                           rebs = F, orebs = F, drebs = F, assists = F, tos = T, 
                           steals = F, blocks = F, pms = F, mins = F, fouls = F, EFS = F)
{
  source("/Users/scottstankey/GitHub/NBACCDBSS/FeatureFunctions/SelectGamesHelper.R")
  tmp = SelectGames(days, oneseason, data, 
                    player, gamedate, seasonid,
                    removeifless, onlyhomeoraway, home,
                    oneopponent, opponent)
  if(fts == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                NA,
                ifelse(ewma == T,
                       EMA(tmp[,"FTM"], n = ewmalookback)[days],
                       sum(tmp[,"FTM"] / days)))}
  if(ftperc == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                             NA,
                             #Might want to change this --- not the best calc
                             ifelse(ewma == T,
                                    EMA(tmp[,"FT_PCT"], n = ewmalookback)[days],
                                    sum(tmp[,"FTM"] / days) / sum(tmp[,"FTA"] / days)))}
  if(fgs == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                              NA,
                              ifelse(ewma == T,
                                     EMA(tmp[,"FGA"], n = ewmalookback)[days],
                                     sum(tmp[,"FGA"] / days)))}
  if(fgperc == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                 NA,
                                 #Might want to change this --- not the best calc
                                 ifelse(ewma == T,
                                        EMA(tmp[,"FG_PCT"], n = ewmalookback)[days],
                                        sum(tmp[,"FGM"] / days) / sum(tmp[,"FGA"] / days)))}
  if(threefgs == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                              NA,
                              ifelse(ewma == T,
                                     EMA(tmp[,"FG3A"], n = ewmalookback)[days],
                                     sum(tmp[,"FG3A"] / days)))}
  if(threefgperc == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                 NA,
                                 #Might want to change this --- not the best calc
                                 ifelse(ewma == T,
                                        EMA(tmp[,"FG3_PCT"], n = ewmalookback)[days],
                                        sum(tmp[,"FG3M"] / days) / sum(tmp[,"FG3A"] / days)))} 
  if(rebs == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"REB"], n = ewmalookback)[days],
                                          sum(tmp[,"REB"] / days)))}
  if(orebs == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"OREB"], n = ewmalookback)[days],
                                          sum(tmp[,"OREB"] / days)))}
  if(drebs == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"DREB"], n = ewmalookback)[days],
                                          sum(tmp[,"DREB"] / days)))}
  if(assists == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"AST"], n = ewmalookback)[days],
                                          sum(tmp[,"AST"] / days)))}
  if(tos == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"TOV"], n = ewmalookback)[days],
                                          sum(tmp[,"TOV"] / days)))}
  if(steals == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"STL"], n = ewmalookback)[days],
                                          sum(tmp[,"STL"] / days)))}
  if(blocks == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"BLK"], n = ewmalookback)[days],
                                          sum(tmp[,"BLK"] / days)))}
  if(pms == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"PLUS_MINUS"], n = ewmalookback)[days],
                                          sum(tmp[,"PLUS_MINUS"] / days)))}
  if(mins == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"MIN"], n = ewmalookback)[days],
                                          sum(tmp[,"MIN"] / days)))}
  if(fouls == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[,"PF"], n = ewmalookback)[days],
                                          sum(tmp[,"PF"] / days)))}
  if(EFS == T){outp = ifelse((removeifless == T && nrow(tmp) < days),
                             NA,
                             ifelse(ewma == T,
                                    EMA((tmp[,"FGM"] + .5 * tmp[,"FG3M"]) / tmp[,"FGA"], n = ewmalookback)[days],
                                    sum(((tmp[,"FGM"] + .5 * tmp[,"FG3M"]) / tmp[,"FGA"]) / days)))}
  #FG + .5 3s /// FG attempted
  return(outp)
}