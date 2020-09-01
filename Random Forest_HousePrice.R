
install.packages('rsample')
library(rsample)      # data splitting 

library(randomForest) 

install.packages('ranger')
library(ranger)       # a faster implementation of randomForest

library(caret)        # an aggregator package for performing many machine learning models

install.packages('h2o')
library(h2o)          # an extremely fast java-based platform

library(dplyr)
library(magrittr)

install.packages('AmesHousing') #使用AmesHousing套件中的Ames Housing Data
library(AmesHousing)

# Create training (70%) and test (30%) sets 
# Use set.seed for reproducibility
set.seed(123)
ames_split <- initial_split(data = AmesHousing::make_ames(), prop = .7)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)


# Random Forest
#透過注入更多隨機性到長樹的過程中，最小化樹與樹之間的相關性，進一步降低變異
#Bootstrap
#Split-variable randomization


set.seed(123)

m1<-randomForest(formula=Sale_Price~ .,
                 data=ames_train)

m1

plot(m1)
#模型平均誤差大概在100棵樹時開始趨於穩定

which.min(m1$mse)
# 280

#RMSE
sqrt(m1$mse[which.min(m1$mse)])
#[1] 25135.88  >> 這是使用OOB Error所計算而得的



#若想使用Validation Set:
set.seed(123)

#依據8:2的比例把原本的訓練資料再切成訓練和驗證資料集
valid_split<-initial_split(ames_train, 0.8)

ames_train_v2<-analysis(valid_split) # training data
ames_valid<-assessment(valid_split) # validation data


# 將validation data整理成x_test和y_test，在模型訓練中使用
x_test<-ames_valid[setdiff(names(ames_valid), "Sale_Price")] #把Sale_Price欄位從中拿掉
y_test<-ames_valid$Sale_Price  #Sale_Price欄位

# 在randomForest函數中使用x-test和y-test當作驗證資料集的參數
rf_oob_comp <- randomForest(
    formula = Sale_Price ~ .,
    data = ames_train_v2,
    xtest = x_test,
    ytest = y_test
)

rf_oob_comp
#  Test set MSE: 587397364
sqrt(587397364)
#[1] 24236.28



#比較OOB error & test error
oob<-sqrt(rf_oob_comp$mse)
validation<-sqrt(rf_oob_comp$test$mse)

data.frame(ntrees=1:rf_oob_comp$ntree,
           OOB.error=oob,
           Test.error=validation
           )%>%
    gather(key=metric, value=RMSE, 2:3)%>%
    ggplot(aes(x=ntrees, y=RMSE, color=metric))+
    geom_line()+
    scale_y_continuous(labels=scales::dollar)+
    xlab("Number of trees")



#修剪

#mtry : 每次在決定切割變數時，所隨機抽樣的潛在變數清單數量

features<-setdiff(x=names(ames_train), y="Sale_Price")

set.seed(123) # 固定不同mtry參數值的模型所使用的隨機OOB sample是一樣的

m2<-tuneRF(x=ames_train[features], y=ames_train$Sale_Price,
          mtryStart=5, ntreeTry=500, stepFactor=1.5, improve=0.01, trace=FALSE)
         #mtry從5開始，每間隔相加1.5，所得到到的OOB error，直到OOB error改善的不度不超過1%為止

#結果發現當mtry > 15後OOB開始不再下降(差不多是feature數80的1/5)


plot(m2)





# Full grid search with ranger
#因為randomForest()無法有效的運行大型數據運算

#比較一下所花費的時間
system.time(
    ames_rf<-randomForest(formula=Sale_Price~ .,
                          data=ames_train,
                          ntree=500,
                          mtry=floor(length(features)/5)
                          )
)
#  user  system elapsed 
#  4.65    0.67   25.40 

system.time(
    ames_ranger <- ranger(
        formula   = Sale_Price ~ ., 
        data      = ames_train, 
        num.trees = 500,
        mtry      = floor(length(features) / 5)
    )
)
# user  system elapsed 
# 4.39    0.01    0.81



#建立一個hyperparameters grid
hyper_grid<-expand.grid(mtry=seq(20,30,2),
                        node_size=seq(3,9,by=2),  
                        # nodesize : 末梢(葉)節點最小觀察資料個數
                        sample_size=c(0.55,0.632,0.7,0.8), 
                        # sampsize : 訓練每棵樹模型的樣本數大小，預設是使用63.25%訓練資料集的比例
                        #一般來說，我們校正此樣本大小參數時會使用60-80%的比例
                        COB_RMSE=0)
nrow(hyper_grid)
#[1] 96

for(i in 1:nrow(hyper_grid)){
    
    model <- ranger(formula=Sale_Price~ .,
                    data=ames_train,
                    num.trees=500,
                    mtry=hyper_grid$mtry[i],
                    min.node.size=hyper_grid$node_size[i],
                    sample.fraction=hyper_grid$sample_size[i],
                    seed=123)   #固定隨機亂數種子，讓同樣sample_size參數值所對應的抽樣樣本可以相同
                               #凸顯其他參數變化所帶來的效果
    
    hyper_grid$OOB_RMSE[i]<- sqrt(model$prediction.error)    
}


#我們將結果依序OOB_RMSE由小至大排列，取前十名
hyper_grid %>% 
    dplyr::arrange(OOB_RMSE) %>% 
    head(10)
# 
#     mtry node_size sample_size COB_RMSE OOB_RMSE
# 1    28         3         0.8        0 25477.32
# 2    28         5         0.8        0 25543.14
# 3    28         7         0.8        0 25689.05
# 4    28         9         0.8        0 25780.86
# 5    30         3         0.8        0 25818.27
# 6    24         3         0.8        0 25838.55
# 7    26         3         0.8        0 25839.71
# 8    20         3         0.8        0 25862.25
# 9    30         5         0.8        0 25884.35
# 10   24         5         0.8        0 25895.22
# 
# OOB_RMSE大致落在26K左右
# 最適mtry的值落在所有20~30範圍區間，表示mtry在此區間對於OOB_RMSE沒有太大影響。
# 最適最小節點觀察值數量大約落在3~5
# 最適抽樣比例約為0.8
# 表示抽樣比例高(~80%)和深度較長(葉節點觀測個數大小3~5)的隨機森林成效較好(OOB RMSE)





#試試將類別變數重新編碼為dummy variables是否能提升random forests的預測表現

to_dummy<-dummyVars(formula= ~.,data=ames_train, fullRank=FALSE)

ames_to_dummy<-predict(to_dummy, newdata=ames_train)%>%as.data.frame()

names(ames_to_dummy)<-make.names(names=names(ames_to_dummy), allow_=FALSE)


#建立hyperparameter grid
#並將mtry的區間調整為更大範圍

hyper_grid_2 <- expand.grid(
    mtry = seq(50, 200, by = 25),
    node_size  = seq(3, 9, by = 2),
    sampe_size = c(.55, .632, .70, .80),
    OOB_RMSE  = 0
)

for(i in 1:nrow(hyper_grid_2)){
    model <- ranger(
        formula = Sale.Price ~.,
        data = ames_to_dummy,  #改用虛擬變數
        num.trees = 500, 
        mtry = hyper_grid_2$mtry[i],
        min.node.size = hyper_grid_2$node_size[i], 
        sample.fraction = hyper_grid_2$sampe_size[i],
        seed = 123
    )
    
    hyper_grid_2$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

hyper_grid_2 %>% 
    dplyr::arrange(OOB_RMSE) %>% 
    head(10)
# 
#     mtry node_size sampe_size OOB_RMSE
# 1    75         3        0.8 26829.16
# 2   125         3        0.8 26894.20
# 3    75         5        0.8 26895.56
# 4    75         7        0.8 26905.67
# 5   150         7        0.8 26937.20
# 6   100         3        0.8 26957.88
# 7   150         5        0.8 26986.49
# 8   150         3        0.8 26996.19
# 9    75         9        0.8 27010.19
# 10   50         3        0.8 27020.71
# 
# OOB RMSE 落在27K左右，並沒有比類別變數重新編碼前的26K來得好
# 將類別變數重新編碼成dummy variables無法提升模型成效


#使用目前的最佳參數
#重複執行100次，計算error rate的期望值
OOB_RMSE<-vector(mode="numeric", length=100)

for(i in 1:length(OOB_RMSE)){
    optimal_ranger<-ranger(formula=Sale_Price~ .,
                           data=ames_train,
                           num.trees=500,
                           mtry=28,
                           min.node.size=3,
                           sample.fraction=0.8,
                           importance='impurity'
                  #依據節點不純度(node impurity)的改善幅度來衡量每個變數的重要性
                  #無法透過該變數切割所降低的模型錯誤率，稱作node impurity
                          )
    OOB_RMSE[i]<-sqrt(optimal_ranger$prediction.error)
}

hist(OOB_RMSE, breaks=20)



#將變數重要性結果繪出
options(scipen = -1)
optimal_ranger$variable.importance %>% 
    as.matrix() %>% 
    as.data.frame() %>% 
    add_rownames() %>% 
    `colnames<-`(c("varname","imp")) %>%
    arrange(desc(imp)) %>% 
    top_n(25,wt = imp) %>% 
    ggplot(mapping = aes(x = reorder(varname, imp), y = imp)) +
    geom_col() +
    coord_flip() +
    ggtitle(label = "Top 25 important variables") +
    theme(
        axis.title = element_blank()
    )

