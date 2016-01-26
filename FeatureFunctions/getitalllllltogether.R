time1 = Sys.time()
  source("/Users/scottstankey/GitHub/NBACCDBSS/FeatureFunctions/SelectGamesHelper.R")
  source("/Users/scottstankey/GitHub/NBACCDBSS/dateconv.R")
  source("/Users/scottstankey/GitHub/NBACCDBSS/FeatureFunctions/team.R")
  source("/Users/scottstankey/GitHub/NBACCDBSS/FeatureFunctions/opponent.R")
  source("/Users/scottstankey/GitHub/NBACCDBSS/FeatureFunctions/AverageShooting.R")
  allseasons = as.data.frame(allseasons)
  allseasons[is.na(allseasons)] = 0
      
  featuretable = NULL
  for(i in 1:100) {

    
  feature1 = AverageShooting(days = 1e7, oneseason = T, data = allseasons, player = allseasons[i,"PLAYER_ID"],
                        gamedate = allseasons[i,"GAME_DATE"], seasonid = allseasons[i,"SEASON_ID"],
                        removeifless = F, onlyhomeoraway = F, home = F, oneopponent = F, opponent = "NO",
                        ewma = F, ewmalookback = 5)
  print("1")
    feature2 = teamWindowAverage(days = 1e7, ewmalookback = 5, oneseason = T, data = allseasons, 
                                 player = allseasons[i,"PLAYER_ID"],
                                 gamedate = allseasons[i,"GAME_DATE"], season_ID = allseasons[i,"SEASON_ID"],
                                 removeifless = F, ewma = F)
  print("2")
    feature3 = OppWindowAverage(days = 1e7, ewmalookback = 5, oneseason = T, data = allseasons, 
                        player = allseasons[i,"PLAYER_ID"], opp = allseasons[i,"OPPONENT"], gamedate = allseasons[i,"GAME_DATE"],
                        season_ID = allseasons[i,"SEASON_ID"],
                        removeifless = F, ewma = F)
  print("3")
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
time2 = Sys.time()
print(time2 - time1)
write.csv(featuretable, "AverageFromSeasonFeatures.csv")
