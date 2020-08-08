
# 分類樹是用來找尋「最能區分標籤資料類別」的一系列變數
# 迴歸樹(regression tree)則是用來找尋「最能區分目標連續變數相近度」的一系列變數
# 迴歸樹的目標變數是連續型變數

# 單一決策樹模型的結果不穩定度高（high variance），預測能力也較弱
# 多半會搭配使用bootstrap aggregating(or Bagging)(一種集成學習法ensemble learning)


library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)       # bagging

#範例資料則是使用AmesHousing package中的Ames Housing數據。
#將資料分成70%訓練集，30%測試集：

library(AmesHousing)

set.seed(123)
ames_split <- initial_split(AmesHousing::make_ames(), prop = .7)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)

m1<-rpart(formula=Sale_Price ~ .,data=ames_train, method="anova")
m1

rpart.plot(m1)

plotcp(m1) 
# 下x軸為cp or cost complexity parameter (α)
# 上x軸為末梢節點數(number of terminal nodes, |T|)
# y軸為交叉驗證誤差(X-val relative error)
#建議可以接受使用與最小X-val error相距一個標準差以內所對應的Tree Size(|T|)來作為修樹的最佳大小（1-SE rule）


#沒有任何懲罰條件存在，讓決策樹長到最大最完整的話......
# m2 <- rpart(
#     formula = Sale_Price ~ .,
#     data    = ames_train,
#     method  = "anova", 
#     control = list(cp = 0, xval = 10)
# )
# 
# {plotcp(m2)
#     abline(v = 12,lty = "dashed", col = "red")}



#詳細的cp值和對應的X-Val error
m1$cptable


#修剪樹
m3<-rpart(formula=Sale_Price ~ ., data=ames_train,
          method="anova",
          control=list(minsplit=10,#分枝前最小所需資料筆數
                       maxdepth=12,#從root nodes跟節點到terminal nodes葉節點間的最大內部節點數量
                       xval=10))
m3$cptable


#使用grid search的方法
#自動執行與比較不同參數水準值組合的效果
#並依據此來選擇最適合的模型參數設定

#建立grid
hyper_grid<-expand.grid(
    minsplit=seq(5,20,1),#從5到20
    maxdepth=seq(8,15,1)#從8到15
)

head(hyper_grid)
nrow(hyper_grid)
#[1] 128

models<-list()

for(i in 1:nrow(hyper_grid)){
    minsplit<-hyper_grid$minsplit[i]
    max_depth<-hyper_grid$maxdepth[i]
    
    models[[i]]<-rpart(formula=Sale_Price~ .,data=ames_train,
                    method="anova",
                    control=list(minsplit=minsplit, max_depth=max_depth))
}

#warnings()


#每一組模型的最小交叉驗證誤差
get_cp<-function(x){
    min<-which.min(x$cptable[,"xerror"])
    cp<-x$cptable[min,"CP"]
}

get_min_error<-function(x){
    min<-which.min(x$cptable[,"xerror"])
    xerror<-x$cptable[min,"xerror"]
}

hyper_grid%>%
    mutate(cp=purrr::map_dbl(models, get_cp),
           error=purrr::map_dbl(models, get_min_error)
           )%>%
    arrange(error)%>%
    top_n(-5, wt=error)#依據error挑出前五名低的


#重新建模 & 預測
optimal_tree<-rpart(formula=Sale_Price~ .,
                    data=ames_train,
                    method="anova",
                    control=list(minsplit=14, max_depth=10, cp=0.01))

pred<-predict(optimal_tree, newdata=ames_test)
RMSE(pred=pred, obs=ames_test$Sale_Price)
#Root Mean Squared Error
#[1] 39852.01



#Boostrap Aggregation (或稱Bagging)
#整合和平均多組模型的預測結果
#有效降低來自單一模型的變異度，並且避免過度配適overfitting
#主要還是對於具有高變異度的模型較有效果
#一個Bootstrap sample會包含63%(2/3)的訓練資料集，和33%(1/3)不在bootstrapped sample內的out-of-bag (OOB) sample。
#可以透過這個OOB sample來衡量模型的準確度，產生一個自然而然的交叉驗證過程


set.seed(123)
bagging_m1<-bagging(formula=Sale_Price~ .,
                    data=ames_train,
                    coob=TRUE)
bagging_m1
#RMSE: 36991.67

#bagging預設會產生25組bootstrap sample和樹模型

#觀察10~50棵樹:
ntree<-10:50

rmse<-vector(mode="numeric", length=length(ntree)) #40個

for(i in seq_along(ntree)){
    
    set.seed(123) #固定bootstrap亂數結果
    
    model<-bagging(formula=Sale_Price~ .,
                   data=ames_train,
                   coob=TRUE,
                   nbagg=ntree[i])
    
    rmse[i]<-model$err
}

{plot(x=ntree, y=rmse, type="l", lwd=2)
    abline(v=25, col="red", lty="dashed")}
#發現差不多在樹數量為25時誤差水準趨於穩定




#改用caret來進行bagging

ctrl<-trainControl(method="cv",number=10) 
#使用cross validation可以提供強大的真實test error的誤差期望值。(instead of OOB error)

bagging_cv<-train(Sale_Price~ .,
                  data=ames_train,
                  method="treebag",
                  trControl=ctrl,
                  importance=TRUE)

bagging_cv
# RMSE: 35854.02

#看前20名的重要變數
plot(varImp(bagging_cv),20)


#預測並評估:
pred=predict(object=bagging_cv, newdata=ames_test)
RMSE(pred=pred, obs=ames_test$Sale_Price)
#[1] 35357.89
#目前最佳表現

#可以發現cross validation(36477)和套用在test set(out of sample)的估計錯誤率(estimated error)(35263)非常相近
#因為cv在訓練過程中是使用test error進行驗證










