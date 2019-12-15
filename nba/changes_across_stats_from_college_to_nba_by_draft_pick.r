college <- read.csv("C:/users/nrp0250451/Documents/RStuff/csvs/college_season_cumulations_since_1992.csv", header=TRUE)
nba <- read.csv("C:/users/nrp0250451/Documents/RStuff/csvs/nba_season_cumulations_since_1992.csv", header=TRUE)
draft <- read.csv("C:/users/nrp0250451/Documents/RStuff/csvs/draft_pick_2009_to_2018.csv", header=TRUE)
fixedMean <- function(vector){
  vector <- vector[!is.nan(vector)]
  vector <- vector[!is.infinite(vector)]
  vector <- vector[!is.na(vector)]
  return(mean(vector))
}
college <- college[as.character(college$Season) > as.character("2007-08"),]
keywords <- c(1,2,3,4,5,10,20,30,40,50,60)
answer <- data.frame(Pick=c(),MP=c(),FG=c(),FGA=c(),X2P=c(),X2PA=c(),X3P=c(),X3PA=c(),FT=c(),FTA=c(),ORB=c(),DRB=c(),TRB=c(),AST=c(),STL=c(),BLK=c(),TOV=c(),PF=c(),PTS=c(),GmSc=c())
for (i in 1:length(keywords)) {
  food <- data.frame()
  if (i == 1){
    subset <- draft[draft$Pk == 1,]
    for(q in 1:nrow(subset)){
      draft_player_name <- strsplit(as.character(subset$Player[q]),"\\\\")[[1]][1]
      food <- rbind(food, data.frame(college[grepl(draft_player_name, college$Player),]))
    }
  }
  else if(keywords[i] == 60) {
    subset <- draft[which(draft$Pk > keywords[i-1])[1]:nrow(draft),]
    for(q in 1:nrow(subset)){
      draft_player_name <- strsplit(as.character(subset$Player[q]),"\\\\")[[1]][1]
      food <- rbind(food, data.frame(college[grepl(draft_player_name, college$Player),]))
    }
  }
  else {
    subset <- draft[which(draft$Pk > keywords[i-1])[1]:(which(draft$Pk > keywords[i])[1]-1),]
    for(q in 1:nrow(subset)){
      draft_player_name <- strsplit(as.character(subset$Player[q]),"\\\\")[[1]][1]
      food <- rbind(food, data.frame(college[grepl(draft_player_name, college$Player),]))
    }
  }
  for (q in 1:nrow(food)) {
    player_name <- strsplit(as.character(food$Player[q]),"\\\\")[[1]][1]
    fed <- nba[grepl(player_name,nba$Player),]
    fed <- fed[order(fed$Season),]
    food.player <- college[grepl(player_name,college$Player),]
    food.player <- food.player[order(food.player$Season, decreasing=TRUE),]
    if (nrow(fed) > 0){
      after <- fed[1,10:28]/fed$G
      before <- food.player[1,9:27]/food.player$G
      row <- after/before
      if(!length(row[row > 10]) && !length(row[row < 0.1])){
        answer <- rbind(answer, data.frame(Pick=c(keywords[i]),row))
      }
    }
  }
}
#print(answer)
answer <- aggregate(answer, by=list(answer$Pick), FUN=fixedMean)
answer <- answer[order(answer$Pick, decreasing=FALSE),]
write.csv(answer, file = "./changes_across_stats_from_college_to_nba_by_draft_pick.csv", row.names = FALSE)
