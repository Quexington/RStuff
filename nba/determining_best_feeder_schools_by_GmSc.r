college.c <- read.csv('C:/users/nrp0250451/Documents/RStuff/csvs/ccol.csv', header=TRUE)
college.g <- read.csv('C:/users/nrp0250451/Documents/RStuff/csvs/gcol.csv', header=TRUE)
college.f <- read.csv('C:/users/nrp0250451/Documents/RStuff/csvs/fcol.csv', header=TRUE)
nba.c <- read.csv('C:/users/nrp0250451/Documents/RStuff/csvs/cnba.csv', header=TRUE)
nba.g <- read.csv('C:/users/nrp0250451/Documents/RStuff/csvs/gnba.csv', header=TRUE)
nba.f <-  read.csv('C:/users/nrp0250451/Documents/RStuff/csvs/fnba.csv', header=TRUE)
college <- rbind(college.c, college.g, college.f)
nba <- rbind(nba.c, nba.g, nba.f)
keywords <- as.character(unique(college$School))
answer <- data.frame(school=c(),score=c(),count=c())
for (i in 1:length(keywords)) {
  food <- college[college$School == keywords[i],]
  food <- aggregate(food, by=list(food$Player), FUN=mean)
  food$School <- rep(keywords[i],nrow(food))
  for (q in 1:length(food)) {
    player_name <- strsplit(as.character(food[q,1]),"\\\\")[[1]][1]
    fed <- nba[grepl(player_name,nba$Player),]
    fed <- fed[order(fed$Season),]
    if (nrow(fed) > 0 && keywords[i] != "Wake Forest"){
      first_season <- fed[1,29]
      if (nrow(answer[answer$school == keywords[i],]) == 0){
        answer <- rbind(answer, data.frame(school=c(keywords[i]),score=c(first_season),count=c(1)))
      }
      else if(!is.na(first_season)){
        answer[answer$school == keywords[i],2] <- answer[answer$school == keywords[i],2] + first_season
        answer[answer$school == keywords[i],3] <- answer[answer$school == keywords[i],3] + 1
      }
    }
  }
}
answer$score <- answer$score/answer$count
answer <- answer[answer$count > 5,]
answer <- answer[order(answer$score, decreasing=TRUE),]
print(answer)
