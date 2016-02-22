library("TTR")
library("utils")
library("stats")
library("zoo")
library("xts")
library("methods")
library("graphics")
library("grDevices")
library("datasets")
dates = c("2010-11", "2011-12", "2012-13", "2013-14", "2014-15","2015-16")
allseasons = NULL
for(dat in dates)
{
  tmp = NULL
  tmp = read.csv(paste("~/GitHub/NBACCDBSS/box score data/",dat,".csv",sep =""), stringsAsFactors=FALSE)
  allseasons = rbind(allseasons,tmp[,-1])
}
datecutter = function(x) {paste(strsplit(x,"-")[[1]][1],
                                strsplit(x,"-")[[1]][2],
                                strsplit(x,"-")[[1]][3],sep = "")}
allseasons$GAME_DATE = sapply(allseasons$GAME_DATE,datecutter)

homedet = function(x) {return(if (strsplit(x," ")[[1]][2] == "vs.") {1} else {0})}  
home = sapply(allseasons$MATCHUP,homedet)

allseasons = cbind(allseasons, home)

getopp = function(x) {return(strsplit(x," ")[[1]][3])}  
opponent = sapply(allseasons$MATCHUP,getopp)
allseasons = cbind(allseasons, opponent)
allseasons = subset(allseasons, select = -c(TEAM_NAME, MATCHUP, VIDEO_AVAILABLE))

colnames(allseasons)[which(colnames(allseasons) == "HOME")] = "TEAM"
colnames(allseasons)[which(colnames(allseasons) == "home")] = "HOME"
colnames(allseasons)[which(colnames(allseasons) == "opponent")] = "OPPONENT"

write.csv(allseasons, "alldata.csv")

SelectGames = function(days = 10, oneseason = T, data = allseasons, 
                       player = "201149", gamedate = "20151219", seasonid = "22015",
                       removeifless = F, onlyhomeoraway = T, home = 1,
                       oneopponent = F, opponent = "BOS")
{
  inds = intersect(which(data$PLAYER_ID == player), which(data$GAME_DATE < gamedate))
  if(oneseason == T)
  {
    inds = intersect(which(data$SEASON_ID == seasonid),inds)
  }
  if(onlyhomeoraway == T){inds = intersect(which(data$HOME == home),inds)}
  if(oneopponent == T){inds = intersect(which(data$OPPONENT == opponent),inds)}
  tmp = data[inds,]
  tmp = tmp[order(tmp$GAME_DATE, decreasing = T),]
  if(days <= nrow(tmp))
  {
    tmp = tmp[c(1:days),]
  } 
  
  return(tmp)
}






dateconv = function(x) {
  if (length(x) > 0 && !is.na(x)){
    monthvec = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    return(((x - (x %% 1e4))*(1e-4) - 2000)*365 + 
             (sum(monthvec[1:((((x %% 1e4) - (x %% 1e2))*(1e-2)) )]) - monthvec[((((x %% 1e4) - (x %% 1e2))*(1e-2)) ):((((x %% 1e4) - (x %% 1e2))*(1e-2)) )]) +
             (x %% 1e2))
  } else {return(0)}
}


OppWindowAverage = function(days = 100, ewmalookback = 5, oneseason = T, data = allseasons, 
                            player = "201149", opp = "UTA", gamedate = "20151219", season_ID = "22015",
                            removeifless = F, ewma = T)
{
  oppplayer = data[which(data$OPPONENT == opp), ]$PLAYER_ID[1]
  #games to search
  data2 = SelectGames(days, oneseason, data, 
                      oppplayer, gamedate, season_ID,
                      removeifless, F, 1,
                      F, "BOS")
  if(nrow(data2) > 0)
  {
    team = data2$TEAM_ABBREVIATION[1]
    data3 = data2$GAME_ID
    
    #find all other players in those games
    tmp = data[which(data$GAME_ID %in% data3), ]
    
    teamGames = unique(tmp$GAME_ID)
    
    teamMatrix = NULL
    
    for(game in teamGames)
    {
      playersInGame = which(tmp$GAME_ID == game)
      possessionsInGame = 0
      points = 0
      oppPoints = 0
      fieldGoalAttempts = 0
      fieldGoalMakes = 0
      treyBombsAttempts = 0
      treyBombsMakes = 0
      freeThrowsAttempts = 0
      freeThrowsMakes = 0
      orebounds = 0
      drebounds = 0
      assists = 0
      steals = 0
      blocks = 0
      turnovahs = 0
      fouls = 0
      oppfieldGoalAttempts = 0
      oppfieldGoalMakes = 0
      opptreyBombsAttempts = 0
      opptreyBombsMakes = 0
      oppfreeThrowsAttempts = 0
      oppfreeThrowsMakes = 0
      opporebounds = 0
      oppdrebounds = 0
      oppassists = 0
      oppsteals = 0
      oppblocks = 0
      oppturnovahs = 0
      oppfouls = 0
      for(p in playersInGame)
      {
        possessionsInGame = possessionsInGame + tmp$FGA[p] + tmp$TOV[p] + (.44 * tmp$FTA[p]) - tmp$OREB[p]
        if(tmp$TEAM_ABBREVIATION[p] == team)
        {
          points = points + tmp$PTS[p]
          fieldGoalAttempts = fieldGoalAttempts + tmp$FGA[p]
          fieldGoalMakes = fieldGoalMakes + tmp$FGM[p]
          treyBombsAttempts = treyBombsAttempts + tmp$FG3A[p]
          treyBombsMakes = treyBombsMakes + tmp$FG3M[p]
          freeThrowsAttempts = freeThrowsAttempts + tmp$FTA[p]
          freeThrowsMakes = freeThrowsMakes + tmp$FTM[p]
          orebounds = orebounds + tmp$OREB[p]
          drebounds = drebounds + tmp$DREB[p]
          assists = assists + tmp$AST[p]
          steals = steals + tmp$STL[p]
          blocks = blocks + tmp$BLK[p]
          turnovahs = turnovahs + tmp$TOV[p]
          fouls = fouls + tmp$PF[p]
        }
        else
        {
          oppPoints = oppPoints + tmp$PTS[p]
          oppfieldGoalAttempts = oppfieldGoalAttempts + tmp$FGA[p]
          oppfieldGoalMakes = oppfieldGoalMakes + tmp$FGM[p]
          opptreyBombsAttempts = opptreyBombsAttempts + tmp$FG3A[p]
          opptreyBombsMakes = opptreyBombsMakes + tmp$FG3M[p]
          oppfreeThrowsAttempts = oppfreeThrowsAttempts + tmp$FTA[p]
          oppfreeThrowsMakes = oppfreeThrowsMakes + tmp$FTM[p]
          opporebounds = opporebounds + tmp$OREB[p]
          oppdrebounds = oppdrebounds + tmp$DREB[p]
          oppassists = oppassists + tmp$AST[p]
          oppsteals = oppsteals + tmp$STL[p]
          oppblocks = oppblocks + tmp$BLK[p]
          oppturnovahs = oppturnovahs + tmp$TOV[p]
          oppfouls = oppfouls + tmp$PF[p]
        }
      }
      teamVec = c(possessionsInGame, points, oppPoints, fieldGoalAttempts, fieldGoalMakes, treyBombsAttempts,
                  treyBombsMakes, freeThrowsAttempts, freeThrowsMakes, orebounds, drebounds, assists,
                  steals, blocks, turnovahs, fouls, oppfieldGoalAttempts, oppfieldGoalMakes, opptreyBombsAttempts,
                  opptreyBombsMakes, oppfreeThrowsAttempts, oppfreeThrowsMakes, opporebounds, oppdrebounds,
                  oppassists, oppsteals, oppblocks, oppturnovahs, oppfouls, (points / possessionsInGame), (oppPoints / possessionsInGame))
      teamMatrix = rbind(teamMatrix, teamVec)
    }
    
    if(removeifless == T && nrow(data2) < days){
      outp = NA
    }else if(ewma == T){
      outp = NULL
      for(i in 1:ncol(teamMatrix)){
        tmpvar = EMA(teamMatrix[,i], n = ewmalookback)[days]
        outp = c(outp,tmpvar)
      }
    } else{
      outp = colMeans(teamMatrix)
    }
    return(outp)
  } else {return(rep(0,31))}
  
}

AverageShooting = function(days = 10, oneseason = T, data = allseasons, 
                           player = "201149", gamedate = "20151219", seasonid = "22015",
                           removeifless = F, onlyhomeoraway = T, home = 1,
                           oneopponent = F, opponent = "BOS", ewma = F, ewmalookback = 5)
{
  

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
    tmp[which(tmp[,"FTA"] == 0),"FTA"] = 1
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
    tmp[which(tmp[,"FGA"] == 0),"FGA"] = 1
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
tmp[which(tmp[,"FG3A"] == 0),"FG3A"] = 1
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
tmp[which(tmp[,"FGA"] == 0),"FGA"] = 1
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
         stealsstat, blocksstat, pmsstat, minsstat, foulsstat, EFSstat, winpercstat)
return(outp)
  } else return (c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
}


teamWindowAverage = function(days = 10, ewmalookback = 5, oneseason = T, data = allseasons, 
                             player = "201149", gamedate = "20151219", season_ID = "22015",
                             removeifless = F, ewma = T)
{
  
  data2 = SelectGames(days, oneseason, data, 
                      player, gamedate, season_ID,
                      removeifless, F, 1,
                      F, "BOS")
  if(nrow(data2) > 0)
  {
    
    
    team = data2$TEAM_ABBREVIATION[1]
    data3 = data2$GAME_ID
    
    #find all other players in those games
    tmp = data[which(data$GAME_ID %in% data3), ]
    
    teamGames = unique(tmp$GAME_ID)
    
    teamMatrix = NULL
    
    for(game in teamGames)
    {
      playersInGame = which(tmp$GAME_ID == game)
      possessionsInGame = 0
      points = 0
      oppPoints = 0
      fieldGoalAttempts = 0
      fieldGoalMakes = 0
      treyBombsAttempts = 0
      treyBombsMakes = 0
      freeThrowsAttempts = 0
      freeThrowsMakes = 0
      orebounds = 0
      drebounds = 0
      assists = 0
      steals = 0
      blocks = 0
      turnovahs = 0
      fouls = 0
      oppfieldGoalAttempts = 0
      oppfieldGoalMakes = 0
      opptreyBombsAttempts = 0
      opptreyBombsMakes = 0
      oppfreeThrowsAttempts = 0
      oppfreeThrowsMakes = 0
      opporebounds = 0
      oppdrebounds = 0
      oppassists = 0
      oppsteals = 0
      oppblocks = 0
      oppturnovahs = 0
      oppfouls = 0
      for(p in playersInGame)
      {
        possessionsInGame = possessionsInGame + tmp$FGA[p] + tmp$TOV[p] + (.44 * tmp$FTA[p]) - tmp$OREB[p]
        if(tmp$TEAM_ABBREVIATION[p] == team)
        {
          points = points + tmp$PTS[p]
          fieldGoalAttempts = fieldGoalAttempts + tmp$FGA[p]
          fieldGoalMakes = fieldGoalMakes + tmp$FGM[p]
          treyBombsAttempts = treyBombsAttempts + tmp$FG3A[p]
          treyBombsMakes = treyBombsMakes + tmp$FG3M[p]
          freeThrowsAttempts = freeThrowsAttempts + tmp$FTA[p]
          freeThrowsMakes = freeThrowsMakes + tmp$FTM[p]
          orebounds = orebounds + tmp$OREB[p]
          drebounds = drebounds + tmp$DREB[p]
          assists = assists + tmp$AST[p]
          steals = steals + tmp$STL[p]
          blocks = blocks + tmp$BLK[p]
          turnovahs = turnovahs + tmp$TOV[p]
          fouls = fouls + tmp$PF[p]
        }
        else
        {
          oppPoints = oppPoints + tmp$PTS[p]
          oppfieldGoalAttempts = oppfieldGoalAttempts + tmp$FGA[p]
          oppfieldGoalMakes = oppfieldGoalMakes + tmp$FGM[p]
          opptreyBombsAttempts = opptreyBombsAttempts + tmp$FG3A[p]
          opptreyBombsMakes = opptreyBombsMakes + tmp$FG3M[p]
          oppfreeThrowsAttempts = oppfreeThrowsAttempts + tmp$FTA[p]
          oppfreeThrowsMakes = oppfreeThrowsMakes + tmp$FTM[p]
          opporebounds = opporebounds + tmp$OREB[p]
          oppdrebounds = oppdrebounds + tmp$DREB[p]
          oppassists = oppassists + tmp$AST[p]
          oppsteals = oppsteals + tmp$STL[p]
          oppblocks = oppblocks + tmp$BLK[p]
          oppturnovahs = oppturnovahs + tmp$TOV[p]
          oppfouls = oppfouls + tmp$PF[p]
        }
      }
      teamVec = c(possessionsInGame, points, oppPoints, fieldGoalAttempts, fieldGoalMakes, treyBombsAttempts,
                  treyBombsMakes, freeThrowsAttempts, freeThrowsMakes, orebounds, drebounds, assists,
                  steals, blocks, turnovahs, fouls, oppfieldGoalAttempts, oppfieldGoalMakes, opptreyBombsAttempts,
                  opptreyBombsMakes, oppfreeThrowsAttempts, oppfreeThrowsMakes, opporebounds, oppdrebounds,
                  oppassists, oppsteals, oppblocks, oppturnovahs, oppfouls, (points / possessionsInGame), (oppPoints / possessionsInGame))
      teamMatrix = rbind(teamMatrix, teamVec)
    }
    
    if(removeifless == T && nrow(data2) < days){
      outp = NA
    }else if(ewma == T){
      outp = NULL
      for(i in 1:ncol(teamMatrix)){
        tmpvar = EMA(teamMatrix[,i], n = ewmalookback)[days]
        outp = c(outp,tmpvar)
      }
    }else{
      outp = colMeans(teamMatrix)
    }
    
    return(outp)
  } else { return(rep(0,31))}
}



allseasons = as.data.frame(allseasons)
allseasons[is.na(allseasons)] = 0

featuretable = NULL
start = Sys.time()
for(i in 1:100) {
  print(i)
  
  feature1 = AverageShooting(days = 1e7, oneseason = T, data = allseasons, player = allseasons[i,"PLAYER_ID"],
                             gamedate = allseasons[i,"GAME_DATE"], seasonid = allseasons[i,"SEASON_ID"],
                             removeifless = F, onlyhomeoraway = F, home = F, oneopponent = F, opponent = "NO",
                             ewma = F, ewmalookback = 5)
  feature2 = teamWindowAverage(days = 1e7, ewmalookback = 5, oneseason = T, data = allseasons, 
                               player = allseasons[i,"PLAYER_ID"],
                               gamedate = allseasons[i,"GAME_DATE"], season_ID = allseasons[i,"SEASON_ID"],
                               removeifless = F, ewma = F)
  feature3 = OppWindowAverage(days = 1e7, ewmalookback = 5, oneseason = T, data = allseasons, 
                              player = allseasons[i,"PLAYER_ID"], opp = allseasons[i,"OPPONENT"], gamedate = allseasons[i,"GAME_DATE"],
                              season_ID = allseasons[i,"SEASON_ID"],
                              removeifless = F, ewma = F)
  feature4 = AverageShooting(days = 10, oneseason = T, data = allseasons, player = allseasons[i,"PLAYER_ID"],
                             gamedate = allseasons[i,"GAME_DATE"], seasonid = allseasons[i,"SEASON_ID"],
                             removeifless = F, onlyhomeoraway = F, home = F, oneopponent = F, opponent = "NO",
                             ewma = F, ewmalookback = 5)
  feature5 = teamWindowAverage(days = 10, ewmalookback = 5, oneseason = T, data = allseasons, 
                               player = allseasons[i,"PLAYER_ID"],
                               gamedate = allseasons[i,"GAME_DATE"], season_ID = allseasons[i,"SEASON_ID"],
                               removeifless = F, ewma = F)
  feature6 = OppWindowAverage(days = 10, ewmalookback = 5, oneseason = T, data = allseasons, 
                              player = allseasons[i,"PLAYER_ID"], opp = "UTA", gamedate = allseasons[i,"GAME_DATE"],
                              season_ID = allseasons[i,"SEASON_ID"],
                              removeifless = F, ewma = F)
  feature7 = AverageShooting(days = 25, oneseason = F, data = allseasons, player = allseasons[i,"PLAYER_ID"],
                             gamedate = allseasons[i,"GAME_DATE"], seasonid = allseasons[i,"SEASON_ID"],
                             removeifless = T, onlyhomeoraway = F, home = F, oneopponent = F, opponent = "NO",
                             ewma = T, ewmalookback = 10)
  feature8 = teamWindowAverage(days = 25, ewmalookback = 10, oneseason = F, data = allseasons, 
                               player = allseasons[i,"PLAYER_ID"],
                               gamedate = allseasons[i,"GAME_DATE"], season_ID = allseasons[i,"SEASON_ID"],
                               removeifless = T, ewma = T)
  feature9 = OppWindowAverage(days = 25, ewmalookback = 10, oneseason = T, data = allseasons, 
                              player = allseasons[i,"PLAYER_ID"], opp = "UTA", gamedate = allseasons[i,"GAME_DATE"],
                              season_ID = allseasons[i,"SEASON_ID"],
                              removeifless = T, ewma = T)
  allfeaturesmotherfuckkaa = c(feature1, feature2, feature3, 
                               feature4, feature5, feature6,
                               feature7, feature8, feature9)
  
  featuretable = rbind(featuretable,allfeaturesmotherfuckkaa)
}
end = Sys.time()
print(start - end)
write.csv(featuretable, "AverageFromSeasonFeatures.csv")

