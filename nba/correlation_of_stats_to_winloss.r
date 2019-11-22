csv <- read.csv('C:/users/quexi/Documents/RStuff/csvs/advanced_whole_nba_season_stats.csv', header=TRUE)
vector <- c()
for(i in 1946:2018){
  sub.vector <- c(i)
  season <- paste(as.character(i), "-", substring(as.character(i+1),3,4), sep="")
  season.sub <- subset(csv, season == csv[,2])
  #print(season.sub)
  for(i in 9:26){
    index <- i
    sub <- subset(season.sub, !is.na(season.sub[,index]))
    #print(attributes(season.sub)$names[index])
    #print(cor(sub[,8], sub[,index]))
    sub.vector <- c(sub.vector, cor(sub[,8], sub[,index]))
  }
  vector <- c(vector, sub.vector)
}
neo <- matrix(
  vector,
  nrow=73,
  ncol=19,
  byrow=TRUE
)
par(c(1,18))
for(i in 2:19) {
  plot(neo[,1], neo[,i], type="o", xlab="Year", ylab=attributes(csv)$names[i+7])
  abline(lm(neo[,i]~neo[,1]), col="red")
}
