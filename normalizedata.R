dates = c("2010-11", "2011-12", "2012-13", "2013-14", "2014-15","2015-16")
allseasons = NULL
for(dat in dates)
{
  tmp = NULL
  tmp = read.csv(paste("~/GitHub/NBACCDBSS/box score data/",dat,".csv",sep =""), stringsAsFactors=FALSE)
  allseasons = rbind(allseasons,tmp[,-1])
}

allseasons$GAME_DATE = paste(strsplit(allseasons$GAME_DATE,"-")[[1]][1],
                             strsplit(allseasons$GAME_DATE,"-")[[1]][2],
                             strsplit(allseasons$GAME_DATE,"-")[[1]][3],sep = "")

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



