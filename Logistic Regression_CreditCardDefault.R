
#使用ISLR套件中的信用卡違約資料，目的是要預測客戶是否會拖欠卡債


install.packages('ISLR')
install.packages('gridExtra')

library(ISLR)
library(tibble)
library(ggplot2)
library(gridExtra)

data = as.data.frame(Default)
str(data)
#
# 'data.frame':	10000 obs. of  4 variables:
# $ default: Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
# $ student: Factor w/ 2 levels "No","Yes": 1 2 1 1 1 2 1 2 1 1 ...
# $ balance: num  730 817 1074 529 786 ...
# $ income : num  44362 12106 31767 35704 38463 ...

#balance 代表信用卡結算餘額，通常指的就是當月該付的錢
data$default #反應變數由"'Yes" "No"構成了二元類別型態的資料

# 檢查遺漏值數量
table(is.na(data))


#將資料切分訓練集以及測試集

set.seed(42)

# 先把資料區分成 train=0.8, test=0.2 
train.index = sample(x=1:nrow(data), size=ceiling(0.8*nrow(data) ))
train = data[train.index, ]
test = data[-train.index, ]

str(train)
str(test)


#將反應變數轉成0和1 #本來是Yes和No
train$default = as.numeric(train$default) - 1 
test$default = as.numeric(test$default) - 1 



#在建立模型前資料視覺化

p1 = ggplot(data = data, aes(x = balance)) +   
    geom_density(aes(fill=default,
                     alpha=0.1)) +
    labs(title="Density plot", 
         subtitle="# Balance Distribution")

p2 = ggplot(data = data, aes(x = income)) +   
    geom_density(aes(fill=default,  
                     alpha=0.1)) +
    labs(title="Density plot", 
         subtitle="# Income Distribution")


grid.arrange(p1, p2, nrow = 1)


#建立模型
model_glm = glm(default ~ balance, data = train, family = "binomial")
summary(model_glm)


#預設的情況，predict glm()會使用type = "link"
#未經log函數轉換成log odd值
head(predict(model_glm))

#預測機率需使用type = "response"
head(predict(model_glm, type='response'))


predict(model_glm,type = "link",
        newdata = data.frame(balance = 2300))
#得到2.07721

predict(model_glm,type = "response",
        newdata = data.frame(balance = 2300))
#得到 1 / (1 + e^-2.07721) = 0.8886683


#進行分類
result = predict(model_glm, type='response')
result = ifelse(result > 0.5,1,0)
head(result)


#混淆矩陣confusion matrix
trn_tab = table(predicted = result, actual = train$default)
trn_tab


#使用caret套件來觀察混淆矩陣
## Predicting Test Data
result = predict(model_glm,newdata=test,type='response')
result = ifelse(result > 0.5,1,0)

## Confusion matrix and statistics
library(caret)
confusionMatrix(data=as.factor(result), 
                reference=as.factor(test$default))


#ROC Curve
library("pROC")

test_prob = predict(model_glm, newdata = test, type = "response")

par(pty = "s") #指定畫布繪製成正方形的大小
test_roc = roc(test$default ~ test_prob, 
               plot = TRUE, 
               print.auc = TRUE, 
               legacy.axes=TRUE) #legacy.axes=TRUE 將 x軸改成 1 - Specificity



