

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


#使用vtreat將training & testing資料one-hot encoding編碼

features <- setdiff(names(ames_train), "Sale_Price")

# Create the treatment plan from the training data
treatplan <- vtreat::designTreatmentsZ(ames_train, 
                                       features, 
                                       verbose = FALSE)

# Get the "clean" variable names from the scoreFrame
new_vars <- treatplan %>%
    magrittr::use_series(scoreFrame) %>%        
    dplyr::filter(code %in% c("clean", "lev")) %>% 
    magrittr::use_series(varName)     


# Prepare the training data
features_train <- vtreat::prepare(treatplan, 
                                  ames_train, 
                                  varRestriction = new_vars) %>% as.matrix()

response_train <- ames_train$Sale_Price

# Prepare the test data
features_test <- vtreat::prepare(treatplan, 
                                 ames_test, 
                                 varRestriction = new_vars) %>% as.matrix()

response_test <- ames_test$Sale_Price

str(features_test)
str(features_train)
dim(features_train)
dim(features_test)


set.seed(123)
system.time(
    xgb.fit1<-xgb.cv(
        data=features_train,
        label=response_train,
        nrounds=1000,
        nfold=5,
        objective="reg:linear",
        verbose=0
    )
)

# user  system elapsed 
# 230.53   10.05   40.06 

print(xgb.fit1,verbose = TRUE)

attributes(xgb.fit1)

xgb.fit1$evaluation_log

# get number of trees that minimize error
xgb.fit1$evaluation_log %>%
    dplyr::summarise(
        ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
        rmse.train   = min(train_rmse_mean),
        ntrees.test  = which(test_rmse_mean == min(test_rmse_mean))[1],
        rmse.test   = min(test_rmse_mean)
    )
# #
#   ntrees.train rmse.train ntrees.test rmse.test
# 1          994  0.0488872         161  25435.18


# plot error vs number trees
ggplot(xgb.fit1$evaluation_log) +
    geom_line(aes(iter, train_rmse_mean), color = "red") +
    geom_line(aes(iter, test_rmse_mean), color = "blue")


#設定early stopping
set.seed(123)
xgb.fit2 <- xgb.cv(
    data = features_train,
    label = response_train,
    nrounds = 1000,
    nfold = 5,
    objective = "reg:linear",  
    verbose = 0,               
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
)

# plot error vs number trees
ggplot(xgb.fit2$evaluation_log) +
    geom_line(aes(iter, train_rmse_mean), color = "red") +
    geom_line(aes(iter, test_rmse_mean), color = "blue")


# Tuning

# create parameter list
params <- list(
    eta = .1,        #學習率
    max_depth = 5,  #樹的深度
    min_child_weight = 2, #末梢節點之最小觀測個數
    subsample = .8,        #每棵樹模型所用訓練集的抽樣比例
    colsample_bytree = .9  #每棵樹模型所抽樣的欄位數
)

set.seed(123)

# train model
system.time(
    xgb.fit3 <- xgb.cv(
        params = params,
        data = features_train,
        label = response_train,
        nrounds = 1000,
        nfold = 5,
        objective = "reg:linear", 
        verbose = 0,               
        early_stopping_rounds = 10    
        )
)

# 
#  user  system elapsed 
# 37.13    1.66    7.15 


# assess results
xgb.fit3$evaluation_log %>%
    dplyr::summarise(
        ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
        rmse.train   = min(train_rmse_mean),
        ntrees.test  = which(test_rmse_mean == min(test_rmse_mean))[1],
        rmse.test   = min(test_rmse_mean)
    )
# #
#    ntrees.train rmse.train ntrees.test rmse.test
# 1          188   5728.809         178     23114




#想要執行更大型的search grid，我們可以使用和gbm相同的程序

# create hyperparameter grid
hyper_grid <- expand.grid(
    eta = c(.01, .05, .1, .3),
    max_depth = c(1, 3, 5, 7),
    min_child_weight = c(1, 3, 5, 7),
    subsample = c(.65, .8, 1), 
    colsample_bytree = c(.8, .9, 1),
    optimal_trees = 0,               
    min_RMSE = 0                     
)

nrow(hyper_grid)
# 576

#!!!!!!!!!!（*這段程序耗時，約6小時以上）!!!!!!!!!!!!!

# grid search 
for(i in 1:nrow(hyper_grid)) {
    
    # create parameter list
    params <- list(
        eta = hyper_grid$eta[i],
        max_depth = hyper_grid$max_depth[i],
        min_child_weight = hyper_grid$min_child_weight[i],
        subsample = hyper_grid$subsample[i],
        colsample_bytree = hyper_grid$colsample_bytree[i]
    )
    
    set.seed(123)
    
    # train model
    xgb.tune <- xgb.cv(
        params = params,
        data = features_train,
        label = response_train,
        nrounds = 5000,
        nfold = 5,
        objective = "reg:linear",  
        verbose = 0,               
        early_stopping_rounds = 10 
        )
    
    # add min training error and trees to grid
    hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
    hyper_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
}

hyper_grid %>%
    dplyr::arrange(min_RMSE) %>%
    head(10)




# parameter list for the final model
params <- list(
    eta = 0.01,
    max_depth = 5,
    min_child_weight = 5,
    subsample = 0.65,
    colsample_bytree = 1
)

# train final model
xgb.fit.final <- xgboost(
    params = params,
    data = features_train,
    label = response_train,
    nrounds = 1576,
    objective = "reg:linear",
    verbose = 0
)


#視覺化

#Variable importance(重要性矩陣)
importance_matrix <- xgb.importance(model = xgb.fit.final)
importance_matrix

# variable importance plot
xgb.plot.importance(importance_matrix, top_n = 10, measure = "Gain")

# variable importance plot using 'cover'
xgb.plot.importance(importance_matrix, top_n = 10, measure = "Cover")



# Partial dependence plots
# ICE curves

pdp <- xgb.fit.final %>%
    partial(pred.var = "Garage_Cars", n.trees = 1576, grid.resolution = 100, train = features_train) %>%
    autoplot(rug = TRUE, train = features_train) +
    scale_y_continuous(labels = scales::dollar) +
    ggtitle("PDP")


ice <- xgb.fit.final %>%
    partial(pred.var = "Garage_Cars", n.trees = 1576, grid.resolution = 100, train = features_train, ice = TRUE) %>%
    autoplot(rug = TRUE, train = features_train, alpha = .1, center = TRUE) +
    scale_y_continuous(labels = scales::dollar) +
    ggtitle("ICE")

gridExtra::grid.arrange(pdp, ice, nrow = 1)


# Lime

# 要分析的局部觀察值需要採用與train, test相同的編碼處理程序
local_obs <- ames_test[1:2, ]
local_obs_onehot <- vtreat::prepare(treatplan, 
                                    local_obs, 
                                    varRestriction = new_vars)

# apply LIME
# 將資料投入lime::lime函式時，必須將其從matrix轉換成dataframe
explainer <- lime(data.frame(features_train), xgb.fit.final)
explanation <- explain(local_obs_onehot, explainer, n_features = 5)
plot_features(explanation)



#預測

pred <- predict(xgb.fit.final, features_test)

# results
caret::RMSE(pred, response_test)
# 21862.02



