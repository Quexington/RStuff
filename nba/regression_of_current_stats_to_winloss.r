csv <- read.csv('C:/users/quexi/Documents/RStuff/csvs/advanced_whole_nba_season_stats.csv', header=TRUE)
current <- read.csv('C:/users/quexi/Documents/RStuff/csvs/current_nba_season_stats.csv', header=TRUE)
sorted = csv[order(csv$Season, decreasing=TRUE), ]
neo_csv = sorted[31:180, ]
reg = lm(neo_csv[,8]~.,data=neo_csv[,9:40])
print(coefficients)

for(i in 1:30){
  ontrack = current[i,9:26]*82/current[i,5]
  adjusted = c(current[i,1:8],ontrack,current[i,27:40])
  current[i,] = adjusted
}
predictions = predict(reg, newdata=current[,9:40])
answer = data.frame(current[,3],predictions*82)
print(answer[order(answer$predictions, decreasing=TRUE),])
