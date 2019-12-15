college <- read.csv("C:/users/nrp0250451/Documents/RStuff/csvs/college_season_cumulations_since_1992.csv", header=TRUE)
nba <- read.csv("C:/users/nrp0250451/Documents/RStuff/csvs/nba_season_cumulations_since_1992.csv", header=TRUE)
keywords <- as.character(unique(college$School))
answer <- data.frame(school=c(),count=c())
for (i in 1:length(keywords)) {
  food <- college[college$School == keywords[i],]
  food <- aggregate(food, by=list(food$Player), FUN=mean)
  food$School <- rep(keywords[i],nrow(food))
  for (q in 1:nrow(food)) {
    player_name <- strsplit(as.character(food[q,1]),"\\\\")[[1]][1]
    fed <- nba[grepl(player_name,nba$Player),]
    if (nrow(fed) > 0){
      if (nrow(answer[answer$school == keywords[i],]) == 0){
        answer <- rbind(answer, data.frame(school=c(keywords[i]),count=c(1)))
      }
      else {
        answer[answer$school == keywords[i],2] <- answer[answer$school == keywords[i],2] + 1
      }
    }
  }
}
answer <- answer[order(answer$count, decreasing=TRUE),]
print(answer)
