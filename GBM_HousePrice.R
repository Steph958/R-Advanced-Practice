

library(rsample)      # data splitting 

install.packages('gbm')
library(gbm)          # basic implementation

install.packages('xgboost')
library(xgboost)      # a faster implementation of gbm

library(caret)        
library(h2o)          

install.packages('pdp')
library(pdp)          # model visualization

library(ggplot2)    

install.packages('lime')
library(lime)         # model visualization

install.packages('vtreat')
library(vtreat)


library(AmesHousing)
set.seed(123)
ames_split<-initial_split(AmesHousing::make_ames(), prop=0.7)
ames_train<-training(ames_split)
ames_test<-testing(ames_split)


set.seed(123)
system.time(gbm.fit<-gbm(
    formula=Sale_Price~ .,
    distribution="gaussian",
    data=ames_train,
    n.trees=10000, #迭代次數
    interaction.depth=1, #弱模型的切割數
    shrinkage=0.001, #學習率
    cv.folds=5, #交叉驗證
    n.cores=NULL, #使用CPU核心的數量，預設值
    verbose=FALSE))  #程序和成效指標的印出
# #
# user  system elapsed 
# 17.20    1.22   54.23 

print(gbm.fit)
#gbm(formula = Sale_Price ~ ., distribution = "gaussian", 
# data = ames_train, n.trees = 10000, interaction.depth = 1, 
# shrinkage = 0.001, cv.folds = 5, verbose = FALSE, n.cores = NULL)
# A gradient boosted model with gaussian loss function.
# 10000 iterations were performed.
# The best cross-validation iteration was 9998.
# There were 80 predictors of which 47 had non-zero influence.

sqrt(min(gbm.fit$cv.error))
# [1] 29551.99

gbm.perf(object=gbm.fit, plot.it=TRUE, method="cv")
# 最佳迭代次數:9998


# Tuning

# create hyperparameter grid
hyper_grid <- expand.grid(
    shrinkage = c(.01, .1, .3), 
    interaction.depth = c(1, 3, 5), #模型的切割數 
    n.minobsinnode = c(5, 10, 15), # 節點最小觀測值個數
    bag.fraction = c(.65, .8, 1), # 使用隨機梯度下降(<1)
    optimal_trees = 0,               # 儲存最適模型樹的欄位
    min_RMSE = 0                     # 儲存最小均方差的欄位
)


nrow(hyper_grid)
# 81

# 使用train.fraction參數前，先randomize data
random_index <- sample(1:nrow(ames_train), nrow(ames_train))
random_ames_train <- ames_train[random_index, ]

#grid search 
for(i in 1:nrow(hyper_grid)) {

    set.seed(123)
    
    gbm.tune <- gbm(
        formula = Sale_Price ~ .,
        distribution = "gaussian",
        data = random_ames_train,
        n.trees = 5000, # 使用5000個樹模型
        interaction.depth = hyper_grid$interaction.depth[i],
        shrinkage = hyper_grid$shrinkage[i],
        n.minobsinnode = hyper_grid$n.minobsinnode[i],
        bag.fraction = hyper_grid$bag.fraction[i],
        train.fraction = .75, # 使用75%的訓練資料，25%做OOB成效評估/驗證>>產出valid error
        n.cores = NULL,
        verbose = FALSE
    )
    
    hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
    hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

#輸出結果
hyper_grid %>% 
    dplyr::arrange(min_RMSE) %>%
    head(10)
#
#    shrinkage interaction.depth n.minobsinnode bag.fraction optimal_trees min_RMSE
# 1       0.01                 5             10         0.65          4987 23020.69
# 2       0.01                 5             15         0.65          4957 23171.25
# 3       0.01                 5              5         0.65          4981 23331.00
# 4       0.10                 5             10         1.00           637 23403.31
# 5       0.01                 5             10         0.80          4861 23431.07
# 6       0.10                 5              5         0.65           400 23612.14
# 7       0.10                 5              5         0.80           850 23613.96
# 8       0.01                 5             15         0.80          4999 23631.55
# 9       0.01                 5             10         1.00          4995 23672.23
# 10      0.10                 5             15         0.80           757 23697.19



# 根據結果，調整參數區間與數值
hyper_grid_2 <- expand.grid(
    shrinkage = c(.01, .05, .1), # 聚焦更小的學習步伐
    interaction.depth = c(3, 5, 7), #聚焦>1的切割數
    n.minobsinnode = c(5, 7, 10), # 聚焦更小的節點觀測值數量
    bag.fraction = c(.65, .8, 1), # 不變
    optimal_trees = 0,               
    min_RMSE = 0                    
)

# total number of combinations
nrow(hyper_grid_2)

# grid search again
for(i in 1:nrow(hyper_grid_2)) {
    
    set.seed(123)
    
    gbm.tune <- gbm(
        formula = Sale_Price ~ .,
        distribution = "gaussian",
        data = random_ames_train,
        n.trees = 6000,
        interaction.depth = hyper_grid_2$interaction.depth[i],
        shrinkage = hyper_grid_2$shrinkage[i],
        n.minobsinnode = hyper_grid_2$n.minobsinnode[i],
        bag.fraction = hyper_grid_2$bag.fraction[i],
        train.fraction = .75,
        n.cores = NULL, 
        verbose = FALSE
    )
    
    hyper_grid_2$optimal_trees[i] <- which.min(gbm.tune$valid.error)
    hyper_grid_2$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

#輸出結果
hyper_grid_2 %>% 
    dplyr::arrange(min_RMSE) %>%
    head(10)
#
#    shrinkage interaction.depth n.minobsinnode bag.fraction optimal_trees min_RMSE
# 1       0.01                 5              7         0.65          6000 22625.92
# 2       0.01                 7              7         0.65          5778 22865.80
# 3       0.05                 7             10         0.80           883 22921.23
# 4       0.01                 5             10         0.65          6000 22965.49
# 5       0.05                 7              5         0.65          2052 23021.31
# 6       0.01                 7              5         0.65          5509 23072.65
# 7       0.01                 7             10         0.65          4544 23081.38
# 8       0.05                 5             10         0.80          2062 23134.52
# 9       0.05                 5             10         0.65          1505 23165.41
# 10      0.05                 7              5         0.80          2032 23176.33



# training
set.seed(123)

system.time(
    gbm.fit.final<-gbm(
        formula = Sale_Price ~ .,
        distribution = "gaussian",
        data = ames_train,
        n.trees = 6000,
        interaction.depth = 5,
        shrinkage = 0.01,
        n.minobsinnode = 7,
        bag.fraction = .65, 
        train.fraction = 1, # 如果使用<1(xx%)的訓練比例，就會用剩餘的(1-XX%)資料估計OOB誤差
        cv.folds = 4, # 有別於使用OOB估計誤差，我們估計更穩健的CV誤差
        n.cores = NULL, 
        verbose = FALSE
        
    )
)
# #
# user  system elapsed 
# 48.21    3.80  117.31


sqrt(min(gbm.fit.final$cv.error))
# 21973.2



#視覺化

par(mar = c(5, 8, 1, 1))

summary(
    gbm.fit.final, 
    cBars = 10, 
    plotit = TRUE, 
    method = relative.influence, # The function used to compute the relative influence. 亦可使用permutation.test.gbm
    las = 2
)


install.packages('vip')
library(vip)
vip::vip(gbm.fit.final)



#PDPs: 繪製特定變數邊際變動造成的平均目標預測值的變動

gbm.fit.final %>%
    partial(object = .,# A fitted model object of appropriate class (e.g., "gbm", "lm", "randomForest", "train", etc.).
            pred.var = "Gr_Liv_Area", 
            n.trees = gbm.fit.final$n.trees, # 如果是gbm的話，需指定模型所使用樹個數
            grid.resolution = 100) %>%
    # The autplot function can be used to produce graphics based on ggplot2
    autoplot(rug = TRUE, train = ames_train) + # plotPartial()不支援gbm
    scale_y_continuous(labels = scales::dollar) # 使用ggplot基礎繪圖


#檢視沒有繪圖(plot = FALSE)的所回傳的data.frame
partialDf <-partial(
    object = gbm.fit.final,
    pred.var = "Gr_Liv_Area", 
    n.trees = gbm.fit.final$n.trees, 
    grid.resolution = 100,
    train = ames_train)

partialDf

partial(object = gbm.fit.final, 
        train = ames_train, 
        pred.var = 'Gr_Liv_Area',
        n.trees = gbm.fit.final$n.trees, 
        plot = TRUE)



# ICE curves:繪製特定解釋變數邊際變動對所有觀測值的目標數值的變動
# 更能強調結果中的異質性
ice1 <- gbm.fit.final %>%
    partial(
        pred.var = "Gr_Liv_Area", 
        n.trees = gbm.fit.final$n.trees, 
        grid.resolution = 100,
        ice = TRUE # 當ice = TRUE或給定pred.fun參數，會回傳使用newdata替每個觀測值預測的結果
    ) %>%
    autoplot(rug = TRUE, train = ames_train, alpha = .1) + # alpha參數只在ICE curves繪製上有效
    ggtitle("Non-centered") +
    scale_y_continuous(labels = scales::dollar)

ice2 <- gbm.fit.final %>%
    partial(
        pred.var = "Gr_Liv_Area", 
        n.trees = gbm.fit.final$n.trees, 
        grid.resolution = 100,
        ice = TRUE
    ) %>%
    autoplot(rug = TRUE, train = ames_train, alpha = .1, center = TRUE) +
    ggtitle("Centered") +
    scale_y_continuous(labels = scales::dollar)

gridExtra::grid.arrange(ice1, ice2, nrow = 1)



# Lime
model_type.gbm <- function(x, ...) {
    return("regression")
}

predict_model.gbm <- function(x, newdata, ...) {
    pred <- predict(x, newdata, n.trees = x$n.trees)
    return(as.data.frame(pred))
}

local_obs <- ames_test[1:2, ]

# apply LIME
explainer <- lime(
    x=ames_train, 
    model = gbm.fit.final 
)
# 一旦使用lime()創建好了explainer，則可將explainer用作解釋模型作用在新觀察值的結果
explanation <- explain(x = local_obs, 
                       explainer = explainer,
                       n_features = 5 # The number of features to use for each explanation.
)

plot_features(explanation = explanation)



#預測

pred <- predict(gbm.fit.final, 
                n.trees = gbm.fit.final$n.trees, 
                ames_test)

# results
caret::RMSE(pred, ames_test$Sale_Price)
# 20356.06







