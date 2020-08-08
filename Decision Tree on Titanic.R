
data <- read.csv('C:/Users/USER/Desktop/資料科學/R resourse/R Training/titan_train.csv')

require(rpart)

str(data)

# 先把資料區分成 train=0.8, test=0.2 
set.seed(22)
train.index <- sample(x=1:nrow(data), size=ceiling(0.8*nrow(data) ))
train <- data[train.index, ]
test <- data[-train.index, ]


str(train)
str(test)

train$Name <- NULL
train$Cabin<-NULL
train$Ticket<-NULL
str(train)

test$Name <- NULL
test$Cabin<-NULL
test$Ticket<-NULL
str(test)


# CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
cart.model<- rpart(Survived ~. , 
                   data=train,
                   method='class')

cart.model


#作圖
require(rpart.plot) 

prp(cart.model,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    )

install.packages('partykit')
require(partykit)   

rparty.tree <- as.party(cart.model) # 轉換cart決策樹
rparty.tree # 輸出各節點的細部資訊
plot(rparty.tree) 



pred <- predict(object=cart.model, newdata=test, type="class")

# 用table看混淆矩陣
table(real=test$Survived, predict=pred)

#計算Accuracy
confus.matrix <- table(real=test$Survived, predict=pred)
sum(diag(confus.matrix))/sum(confus.matrix) # 對角線的數量/總數量



#修剪樹

# 先觀察未修剪的樹，CP欄位代表樹的成本複雜度參數
printcp(cart.model)
plotcp(cart.model)

# 利用能使決策樹具有最小誤差的CP來修剪樹
prunetree_cart.model <- prune(cart.model, 
                              cp = cart.model$cptable[which.min(cart.model$cptable[,"xerror"]),"CP"]) 

prunetree_cart.model

#重建模型
prunetree_pred <- predict(prunetree_cart.model, newdata=test, type="class")

# 用table看預測的情況
table(real=test$Survived, predict=prunetree_pred)

prunetree_confus.matrix <- table(real=test$Survived, predict=prunetree_pred)
sum(diag(prunetree_confus.matrix))/sum(prunetree_confus.matrix) 



#K-fold Cross-Validation
    
require(caret)
require(e1071)

train_control <- trainControl(method="cv", number=10)

train$Survived<-train$Survived[is.na(train$Survived)==FALSE]
# for(i in train$Survived){
#     if(is.na(i)==TRUE){
#         print("+")
#     }
# }

train$Survived<-as.factor(train$Survived)

train_control.model <- train(Survived~., data=train, method="rpart", 
                             trControl=train_control, 
                             na.action=na.omit)

train_control.model


