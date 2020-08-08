
data <- read.csv('C:/Users/USER/Desktop/資料科學/R resourse/R Training/titan_train.csv')
head(data)

str(data)

#將資料重新隨機排列
shuffle_index <- sample(x = 1:nrow(data))
data <- data[shuffle_index,]
head(data)



inputData <- inputData[shuffle_index,]
head(inputData)



require(rpart)