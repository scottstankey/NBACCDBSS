AverageShooting = function(days = 10, oneseason = T, data = allseasons, 
                           player = "201149", gamedate = "20151219", seasonid = "22015",
                           removeifless = F, onlyhomeoraway = T, home = 1,
                           oneopponent = F, opponent = "BOS", ewma = F, ewmalookback = 5,
                           fts = F, ftperc = F,
                           fgs = F, fgperc = F,
                           threefgs = F, threefgperc = F,
                           rebs = F, orebs = F, drebs = F, assists = F, tos = T, 
                           steals = F, blocks = F, pms = F, mins = F, fouls = F)
{
  source("/Users/scottstankey/GitHub/NBACCDBSS/FeatureFunctions/SelectGamesHelper.R")
  tmp = SelectGames(days, oneseason, data, 
                    player, gamedate, seasonid,
                    removeifless, onlyhomeoraway, home,
                    oneopponent, opponent)
  if(fts == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                NA,
                ifelse(ewma == T,
                       EMA(tmp[1:days,"FTM"], n = ewmalookback)[days],
                       sum(tmp[1:days,"FTM"] / days)))}
  if(ftperc == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                             NA,
                             #Might want to change this --- not the best calc
                             ifelse(ewma == T,
                                    EMA(tmp[1:days,"FT_PCT"], n = ewmalookback)[days],
                                    sum(tmp[1:days,"FTM"] / days) / sum(tmp[1:days,"FTA"] / days)))}
  if(fgs == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                              NA,
                              ifelse(ewma == T,
                                     EMA(tmp[1:days,"FGA"], n = ewmalookback)[days],
                                     sum(tmp[1:days,"FGA"] / days)))}
  if(fgperc == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                 NA,
                                 #Might want to change this --- not the best calc
                                 ifelse(ewma == T,
                                        EMA(tmp[1:days,"FG_PCT"], n = ewmalookback)[days],
                                        sum(tmp[1:days,"FGM"] / days) / sum(tmp[1:days,"FGA"] / days)))}
  if(threefgs == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                              NA,
                              ifelse(ewma == T,
                                     EMA(tmp[1:days,"FG3A"], n = ewmalookback)[days],
                                     sum(tmp[1:days,"FG3A"] / days)))}
  if(threefgperc == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                 NA,
                                 #Might want to change this --- not the best calc
                                 ifelse(ewma == T,
                                        EMA(tmp[1:days,"FG3_PCT"], n = ewmalookback)[days],
                                        sum(tmp[1:days,"FG3M"] / days) / sum(tmp[1:days,"FG3A"] / days)))} 
  if(rebs == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[1:days,"REB"], n = ewmalookback)[days],
                                          sum(tmp[1:days,"REB"] / days)))}
  if(orebs == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[1:days,"OREB"], n = ewmalookback)[days],
                                          sum(tmp[1:days,"OREB"] / days)))}
  if(drebs == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[1:days,"DREB"], n = ewmalookback)[days],
                                          sum(tmp[1:days,"DREB"] / days)))}
  if(assists == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[1:days,"AST"], n = ewmalookback)[days],
                                          sum(tmp[1:days,"AST"] / days)))}
  if(tos == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[1:days,"TOV"], n = ewmalookback)[days],
                                          sum(tmp[1:days,"TOV"] / days)))}
  if(steals == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[1:days,"STL"], n = ewmalookback)[days],
                                          sum(tmp[1:days,"STL"] / days)))}
  if(blocks == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[1:days,"BLK"], n = ewmalookback)[days],
                                          sum(tmp[1:days,"BLK"] / days)))}
  if(pms == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[1:days,"PLUS_MINUS"], n = ewmalookback)[days],
                                          sum(tmp[1:days,"PLUS_MINUS"] / days)))}
  if(mins == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[1:days,"MIN"], n = ewmalookback)[days],
                                          sum(tmp[1:days,"MIN"] / days)))}
  if(fouls == T) {outp = ifelse((removeifless == T && nrow(tmp) < days),
                                   NA,
                                   ifelse(ewma == T,
                                          EMA(tmp[1:days,"PF"], n = ewmalookback)[days],
                                          sum(tmp[1:days,"PF"] / days)))}
  return(outp)
}