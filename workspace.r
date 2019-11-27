train <- read.csv('./kaggle_comps/csvs/train.csv')
test <- read.csv('./kaggle_comps/csvs/test.csv')
vector <- c()
for(i in 1:891) {
  vector <- c(vector, substr(train[i,11],1,1))
}
train[,11] <- vector
vector <- c()
for(i in 1:891) {
  vector <- c(vector, as.character(train[i,3]))
}
train[,3] <- vector
vector <- c()
for(i in 1:891) {
  vector <- c(vector, as.character(train[i,2]))
}
train[,2] <- vector
vector <- c()
for(i in 1:891) {
  vector <- c(vector, as.integer(train[i,9]))
}
train[,9] <- vector

vector <- c()
for(i in 1:418) {
  vector <- c(vector, substr(test[i,10],1,1))
}
test[,10] <- vector
vector <- c()
for(i in 1:418) {
  vector <- c(vector, as.character(test[i,2]))
}
test[,2] <- vector
vector <- c()
for(i in 1:418) {
  vector <- c(vector, as.integer(test[i,8]))
}
test[,8] <- vector

results <- train[,2]
factors <- train[,c(3,5:12)]
reg <- lm(results~.,data=factors)
predictions <- predict(reg, newdata=test[,c(2,4:11)])
answer = data.frame(test[,1],predictions)
comp <- answer[order(answer[,2], decreasing=TRUE),][1:160,][,1]

vector <- c()
for (i in 892:1309) {
  if (is.na(match(i,comp))) {
    vector <- c(vector, 0)
  } else {
    vector <- c(vector, 1)
  }
}
ans <- data.frame(c(892:1309),vector)
write.csv(ans, file='./kaggle_comps/csvs/answer.csv', row.names=FALSE)
#print(answer)
