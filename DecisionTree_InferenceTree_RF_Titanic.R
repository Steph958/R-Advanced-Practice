
data <- read.csv('C:/Users/USER/Desktop/資料科學/R resourse/R Training/titan_train.csv')
head(data)

str(data)

#將資料重新隨機排列
shuffle_index <- sample(x = 1:nrow(data))
data <- data[shuffle_index,]
head(data)


summary(data)
str(data)


#資料前處理

data$Cabin<-NULL
data$Name<-NULL
data$Ticket<-NULL


library(dplyr)

data$Pclass = factor(data$Pclass, levels = c(1, 2, 3), 
                       labels = c('Upper', 'Middle', 'Lower'))

data$Survived = factor(data$Survived, levels = c(0, 1), 
                         labels = c('No', 'Yes'))

str(data)


#區分訓練資料和測試資料
#要確保兩組資料集中生還比例不要差異太大
ones<-data[which(data$Survived=="Yes"),]
zeros<-data[which(data$Survived=="No"),]

set.seed(100)

ones_train_row<-sample(1:nrow(ones),0.8*nrow(ones))
zeros_train_row<-sample(1:nrow(zeros),0.8*nrow(zeros))

#訓練資料
train_ones<-data[ones_train_row,]
train_zeros<-data[zeros_train_row,]
train<-rbind(train_ones,train_zeros)
#測試資料
test_ones<-data[-ones_train_row,]
test_zeros<-data[-zeros_train_row,]
test<-rbind(test_ones,test_zeros)

dim(train)
dim(test)

#檢查切割完的資料集大小與目標變數的分佈比例
prop.table(table(train$Survived))
prop.table(table(test$Survived))



#建立模型
library(rpart)
library(rpart.plot)

fit <- rpart(formula = Survived ~ ., 
             data = train, 
             method = 'class',   # anova/poisson/class/exp
             control = rpart.control(),  #事前調整/修剪樹
             na.action = na.rpart)   #此為預設值，使用CART演算法中的surrogate variables做預測填補

#作圖
rpart.plot(fit, extra= 106)

#將規則印出
rpart.rules(x=fit,cover=TRUE) #cover則代表該節點觀測資料個數占比

#檢視交叉驗證(cross-validation)的不同cp值(complexity parameter)下的錯誤率
printcp(x=fit)

plotcp(x = fit)



#預測
pred<-predict(object=fit,newdata=test,type='class')
pred


#評估模型
#使用混淆矩陣
tbl<-table(pred,test$Survived)
tbl

# Accuracy
accuracy <- sum(diag(tbl)) / sum(tbl)
accuracy
#[1] 0.8196262


#修剪樹
#事後修剪(post-prune)

# fit$cptable
# fit$cptable[,"xerror"]

#選擇讓交叉驗證中相對誤差改變量最小的cp值
fit_prune<-prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])


# plot the pruned tree 
rpart.plot(fit_prune, extra= 106, tweak = 1.1, shadow.col = "gray", branch.lty = 3, roundint = TRUE)
# branch.lty = 3:樹的高度

#再次預測
pred_prune <- predict(object = fit_prune,newdata = test,type = 'class')
tbl_prune <- table(pred_prune,test$Survived)
tbl_prune

# Accuracy
accuracy <- sum(diag(tbl_prune)) / sum(tbl_prune)
accuracy
#[1] 0.8224299



# K-folds Cross Validation
library(caret)
library(e1071)

#選擇重抽樣的方法
train_control<-trainControl(method="cv",number=10) #k=10

train_control_model<-train(Survived~ ., data=train,
                           method = 'rpart',
                           na.action = na.pass, 
                           trControl = train_control)
train_control_model

# 進行10次交叉驗證的平均正確率為82.29%，與修剪後的樹模型正確率82.24%沒有太大差異（差異百分比在1%以內）。
# 表示模型沒有overfitting的問題。


#將參數na.action調整為na.rpart（使用CART中的代理變數surrogate variables來預測)
train_control_model_2<- train(Survived ~ ., data = train, 
                              method = 'rpart',
                              na.action = na.rpart, 
                              trControl = train_control)
train_control_model_2
# Highest Accuracy = 80.90%




#和其他模型比較之一:條件推論樹(Conditional Inference Tree)
install.packages('party')
library(party)

str(train)

#前處理
train$Sex<-factor(train$Sex)
train$Embarked<-factor(train$Embarked,level=c('C','Q','S'), labels=c(1,2,3))
test$Sex<-factor(test$Sex)
test$Embarked<-factor(test$Embarked,level=c('C','Q','S'), labels=c(1,2,3))
str(train)
str(test)

fit_ctree<-ctree(Survived~ .,data=train)
#ctree演算法na.action預設為na.pass，可以將其改為na.rpart
plot(fit_ctree)

pred_ctree<-predict(object=fit_ctree, newdata=test)
tbl_ctree<-table(predicted=pred_ctree, actuals=test$Survived)
tbl_ctree

#Accuracy
accuracy<-sum(diag(tbl_ctree)/sum(tbl_ctree))
accuracy
#[1] 0.7943925





#和其他模型比較之二:隨機森林(Random Forest)

#由集成學習法建立的模型較能不容易發生過度配適的問題
#對列抽樣，可以部分解決因類別不平衡(Class Imbalance)對預測帶來的問題
#對行抽樣，則可解決部分因共線性(collinearity)對預測造成的問題。

install.packages('randomForest')
library(randomForest)

set.seed(100)
fit_rf<-randomForest(Survived~ ., data=train, 
                     na.action=na.omit,
                     importance=TRUE)  #預設使用FALSE，只有the mean decrease in Gini index
fit_rf

#Accuracy
fit_rf$confusion
tbl_rf<-fit_rf$confusion[,c(1,2)]
accuracy<-sum(diag(tbl_rf)/sum(tbl_rf))
accuracy
#[1] 0.9019264


#每增加每一顆決策樹，整體誤差的改變量
plot(fit_rf)
#發現100多棵樹就OK了
# 
# 誤差為OOB(out-of-bag) Erro Rates
# 當為分類樹時(classification tree)，黑色實線表示整體的OOB error rate，而其他顏色虛線表示各類別的OOB Error Rate。
# 當為回歸樹時(regression tree)，誤差為OOB(out-of-bag) MSE，只會有一條黑色實線代表整體的OOB MSE。



#調整每一樹節點切割時隨機抽樣之變數數量
train_naomit<-na.omit(train)
tuneRF(x=train_naomit[,-2],y=train_naomit[,2])# 第二欄位是Survived

#發現在mtry=2時，誤差最小
#randomForest中類別樹預設的mtry=sqrt(p)，其中p代表x變數的數目
fit_rf$mtry
#[1] 2


#看每個變數的重要性(importance)
# the mean decrease in Gini index
round(importance(fit_rf),2)
varImpPlot(fit_rf)


#預測並評估
pred_rf<-predict(object=fit_rf, newdata=test)
tbl_rf<-table(predicted = pred_rf, actuals=test$Survived)
accuracy<-sum(diag(tbl_rf)/sum(tbl_rf))
accuracy
#[1] 0.8358734
#目前最佳表現














