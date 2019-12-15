college <- read.csv("C:/users/nrp0250451/Documents/RStuff/csvs/college_season_cumulations_since_1992.csv", header=TRUE)
nba <- read.csv("C:/users/nrp0250451/Documents/RStuff/csvs/nba_season_cumulations_since_1992.csv", header=TRUE)
fixedMean <- function(vector){
  vector <- vector[!is.nan(vector)]
  vector <- vector[!is.infinite(vector)]
  vector <- vector[!is.na(vector)]
  return(mean(vector))
}
errors <- c()
college <- college[as.character(college$Season) > as.character("2013-14"),]
keywords <- c("G","F","C")
answer <- data.frame(Pos=c(),MP=c(),FG=c(),FGA=c(),X2P=c(),X2PA=c(),X3P=c(),X3PA=c(),FT=c(),FTA=c(),ORB=c(),DRB=c(),TRB=c(),AST=c(),STL=c(),BLK=c(),TOV=c(),PF=c(),PTS=c(),GmSc=c())
for (i in 1:length(keywords)) {
  food <- college[college$Pos == keywords[i],]
  players <- as.character(unique(college$Player))
  totals <- c(rep(c(),19))
  for (q in 1:nrow(food)) {
    player_name <- strsplit(as.character(food$Player[q]),"\\\\")[[1]][1]
    print(q)
    fed <- nba[grepl(player_name,nba$Player),]
    fed <- fed[order(fed$Season),]
    food.player <- college[grepl(player_name,college$Player),]
    food.player <- food.player[order(food.player$Season, decreasing=TRUE),]
    if (nrow(fed) > 0){
      after <- fed[1,10:28]/fed$G
      before <- food.player[1,9:27]/food.player$G
      row <- after/before
      if(!length(row[row > 10]) && !length(row[row < 0.1])){
        answer <- rbind(answer, data.frame(Pos=c(keywords[i]),row))
      }
    }
  }
}
#print(answer)
answer <- aggregate(answer, by=list(answer$Pos), FUN=fixedMean)
answer <- answer[order(answer$GmSc, decreasing=TRUE),]
write.csv(answer, file = "./changes_across_stats_from_college_to_nba_by_position.csv", row.names = FALSE)
