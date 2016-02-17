
teamWindowAverage = function(days = 10, ewmalookback = 5, oneseason = T, data = allseasons, 
                                player = "201149", gamedate = "20151219", season_ID = "22015",
                                removeifless = F, ewma = T)
{
<<<<<<< Updated upstream

=======
  #games to search
  #source("~/NBACCDBSS/FeatureFunctions/SelectGamesHelper.R")
>>>>>>> Stashed changes
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

