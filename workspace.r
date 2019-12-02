csv <- read.csv('C:/users/nrp0250451/Documents/RStuff/csvs/injury_keywords.csv', header=TRUE)
inj <- read.csv('C:/users/nrp0250451/Documents/RStuff/csvs/injuries.csv', header=TRUE)
players <- read.csv('C:/users/nrp0250451/Documents/RStuff/csvs/basic_per_game_player_stats_2013_2018.csv')
counter <- data.frame(csv[,2],rep(0,length(csv[,1])))
players <- players[,c(1:13,15:16,18:19,21:22,24:30)]
injury.tracker <- data.frame(stringsAsFactors=FALSE)
fixedMean <- function(vector){
  vector <- vector[!is.nan(vector)]
  vector <- vector[!is.infinite(vector)]
  vector <- vector[!is.na(vector)]
  return(mean(vector))
}
for (i in 1:length(counter[,1])){
  for (q in 1:length(inj[,1])){
    if (grepl(counter[i,1],inj[q,6])){
      injur_date = inj[q,2]
      player_name = inj[q,5]
      slice = players[grepl(player_name, players[,2]),]
      slice = slice[order(slice[,5]),]
      for(z in 1:length(slice[,1])){
        if(player_name != "" && player_name != "Carlos Delfino" && player_name != "Yakuba Ouattara" && player_name != "Steve Kerr" && player_name != "Jason Kidd" && player_name != "Harry Giles" && player_name != "Frank Jackson" && player_name != "Kwame Brown" && player_name != "Steve Clifford" && player_name != "Tyronn Lue"){
          if (as.POSIXct(as.character(slice[z,5])) > as.POSIXct(as.character(injur_date))){
            if (z > 5 && (length(slice[,1]) - 5) > 5){
              bool.tracker <- injury.tracker[injury.tracker$player == player_name,]
              bool.tracker <- bool.tracker[bool.tracker$return_date == slice[z,5],]
              bool.tracker <- bool.tracker[bool.tracker$keyword == counter[i,1],]
              if(nrow(bool.tracker) == 0){
                counter[i,2] = counter[i,2] + 1
                first.slice = slice[(z-5):(z-1),]
                second.slice = slice[z:(z+4),]
                first.agg <- aggregate(first.slice, by = list(first.slice[,2]), FUN = mean)
                second.agg <- aggregate(second.slice, by = list(second.slice[,2]), FUN = mean)
                diff <- (second.agg/first.agg)
                adder <- data.frame(keyword=as.character(counter[i,1]),player=as.character(player_name),return_date=as.character(slice[z,5]),diff[,12:26],stringsAsFactors=FALSE)
                injury.tracker <- rbind(injury.tracker, adder)
              }
            }
            break
          }
        }
      }
    }
  }
  print(counter[i,])
}
injury.tracker <- injury.tracker[,c(1,4:18)]
injury.agg <- aggregate(injury.tracker, by = list(injury.tracker$keyword), FUN = fixedMean)
counter <- counter[order(counter[,1]),]
counter <- counter[!is.na(match(counter[,1],injury.agg[,1])),]
injury.agg[,2] <- counter[,2]
write.csv(injury.agg, file='./tester.csv')
