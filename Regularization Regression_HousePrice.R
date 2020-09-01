
library(rsample)  # data splitting 
library(glmnet)   # implementing regularized regression approaches
library(dplyr)    
library(ggplot2)  

library(AmesHousing)

set.seed(123)
ames_split<-initial_split(data=AmesHousing::make_ames(), prop=0.7, strata="Sale_Price")
ames_train<-training(ames_split)
ames_test<-testing(ames_split)



# 問題一:完全多重共線性
install.packages('Hmisc')
library(Hmisc)

data<-as.data.frame(AmesHousing::make_ames())
#產生metrix用來放correlation & p-value
res<-rcorr(as.matrix(data[,sapply(data, is.numeric)]))

flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat) # Lower and Upper Triangular Part of a Matrix
    data.frame(
        row = rownames(cormat)[row(cormat)[ut]],
        column = rownames(cormat)[col(cormat)[ut]],
        cor  =(cormat)[ut],
        p = pmat[ut]
    )
}

cor_table<-flattenCorrMatrix(res$r, res$P)
head(cor_table)

cor_table%>%
    filter(abs(cor)>0.6)%>%arrange(desc(cor))

cor_table %>% 
    filter(row == "Sale_Price" | column == "Sale_Price") %>% filter(abs(round(cor,digits = 2)) >= 0.5) %>% arrange(desc(cor))


#模型1: 投入兩高相關性變數
lm(Sale_Price ~ Gr_Liv_Area + TotRms_AbvGrd, data = ames_train)

#模型2: 單獨使用Gr_Liv_Area進行回歸的結果。
lm(Sale_Price ~ Gr_Liv_Area, data = ames_train)

#模型3: 單獨使用TotRms_AbvGrd進行回歸的結果。
lm(Sale_Price ~ TotRms_AbvGrd, data = ames_train)
    




# 問題二:解決方案不充分
#特徵個數超過觀測個數


# 問題三:可解釋性


# 替代方案 & 解決辦法
#正規化迴歸

    
    

# Ridge Regression
# L2 Penalty


#分成預測變數矩陣與目標變數，並替目標變數進行log轉換
ames_train_x <- model.matrix(object = Sale_Price ~ ., 
                             data =  ames_train)[, -1] #忽略第一行截距項

ames_train_y <- log(ames_train$Sale_Price)

ames_test_x <- model.matrix(Sale_Price ~ ., ames_test)[, -1]

ames_test_y <- log(ames_test$Sale_Price)

dim(ames_train_x)
# [1] 2053  308


#執行模型
ames_ridge<-glmnet(
    x=ames_train_x,
    y=ames_train_y,
    alpha=0, # ridge
    standardize=TRUE  #預設
    )

plot(ames_ridge, xvar="lambda") #懲罰值


#係數壓縮效果
ames_ridge$lambda
#最小λ值
coef(ames_ridge)[c("Gr_Liv_Area", "TotRms_AbvGrd"), 100]
#最大λ值
coef(ames_ridge)[c("Gr_Liv_Area", "TotRms_AbvGrd"), 1]


# Turning

#執行k-fold cross validation，預設k=10
ames_ridge<-cv.glmnet(
    x=ames_train_x,
    y=ames_train_y,
    alpha=0,
)

plot(ames_ridge)
# Ridge Model不會直接強制讓變數係數變成0
# 圖中的第一和第二條垂直虛線則分別代表：對應最小MSE的λ、對應最小MSE一個標準差內的最大λ
ames_ridge$lambda.min
log(ames_ridge$lambda.min)

ames_ridge$cvm[ames_ridge$lambda==ames_ridge$lambda.1se] #距離最小MSE一個標準差內的MSE值

ames_ridge$lambda.1se
log(ames_ridge$lambda.1se)


#視覺化
ames_ridge_min<-glmnet(
    x=ames_train_x,
    y=ames_train_y,
    alpha=0)

{plot(ames_ridge_min, xvar="lambda")
    abline(v = log(ames_ridge$lambda.1se), col = "red", lty = "dashed")
}
# 紅色垂直虛線表示對應最小MSE一個標準差內λ


#Top 25個具有影響力的變數
coef(ames_ridge, s = "lambda.1se") %>% # 308 x 1 sparse Matrix of class "dgCMatrix"
    as.matrix() %>% 
    as.data.frame() %>% 
    add_rownames(var = "var") %>% 
    `colnames<-`(c("var","coef")) %>%
    filter(var != "(Intercept)") %>%  #剔除截距項
    top_n(25, wt = coef) %>% 
    ggplot(aes(coef, reorder(var, coef))) +
    geom_point() +
    ggtitle("Top 25 influential variables") +
    xlab("Coefficient") +
    ylab(NULL)


# Lasso Regression
# L1 Panalty
#會將係數壓縮至0
# Feature Selection

ames_lasso<-glmnet(
    x=ames_train_x,
    y=ames_train_y,
    alpha=1,
    standardize=TRUE
)

plot(ames_lasso, xvar="lambda")


# Turning
ames_lasso<-cv.glmnet(
    x=ames_train_x,
    y=ames_train_y,
    alpha=1
)

plot(ames_lasso)

#取得最小MSE
min(ames_lasso$cvm)

#對應最小MSE的log(λ)
log(ames_lasso$lambda.min)

#within 1 S.E. of minimum MSE
ames_lasso$cvm[ames_lasso$lambda == ames_lasso$lambda.1se]

#對應到最小MSE一個標準差內的最大log(λ)
ames_lasso$lambda.1se


#視覺化
ames_lasso_min<-glmnet(
    x=ames_train_x,
    y=ames_train_y,
    alpha=1
)

{
plot(ames_lasso_min, xvar="lambda")
abline(v=log(ames_lasso$lambda.min), col="red", lty="dashed")
abline(v=log(ames_lasso$lambda.1se), col="red", lty="dashed")
}



# Elastic Net
#綜合了Ridge Penalty達到有效正規化優勢以及Lasso Penalty能夠進行變數挑選優勢
lasso    <- glmnet(ames_train_x, ames_train_y, alpha = 1.0) 
elastic1 <- glmnet(ames_train_x, ames_train_y, alpha = 0.25) 
elastic2 <- glmnet(ames_train_x, ames_train_y, alpha = 0.75) 
ridge    <- glmnet(ames_train_x, ames_train_y, alpha = 0.0)

par(mfrow = c(2,2), mar = c(4,2,4,2), + 0.1)
plot(lasso, xvar = "lambda", main = "Lasso (Alpha = 1) \n\n")
plot(elastic1, xvar = "lambda", main = "Elastic Net (Alpha = 0.75) \n\n")
plot(elastic2, xvar = "lambda", main = "Elastic Net (Alpha = 0.25) \n\n")
plot(ridge, xvar = "lambda", main = "Ridge (Alpha = 0) \n\n")



# Turning

# maintain the same folds across all models
# coz in Elastic cv.glmnet will run several times
fold_id <- sample(x = 1:10, size = length(ames_train_y), replace = TRUE)

# build an empty tuning grid
tuning_grid<-tibble::tibble(
    alpha=seq(0,1,by=0.1),
    mse_min=NA,
    mse_1se=NA,
    lambda_min=NA,
    lambda_1se=NA
)

# Iteration
for(i in seq_along(tuning_grid$alpha)){
    fit<-cv.glmnet(x=ames_train_x,
                   y=ames_train_y,
                   alpha=tuning_grid$alpha[i],
                   foldid=fold_id)
    
    tuning_grid$mse_min[i]<-fit$cvm[fit$lambda==fit$lambda.min]
    tuning_grid$mse_1se[i]<-fit$cvm[fit$lambda==fit$lambda.1se]
    tuning_grid$lambda_min[i]<-fit$lambda.min
    tuning_grid$lambda_1se[i]<-fit$lambda.1se
}

tuning_grid

# alpha從1變化到0的正確率變化
tuning_grid %>%
    mutate(se = mse_1se - mse_min) %>% # 計算1個SE的距離
    ggplot(aes(alpha, mse_min)) + # 繪製不同alpha參數下，cv所得的最小MSE值
    geom_line(size = 2) +
    geom_ribbon(aes(ymax = mse_min + se, ymin = mse_min - se), alpha = .25) +
    ggtitle("MSE ± one standard error")



# Predicting
cv_lasso<-cv.glmnet(x=ames_train_x,
                    y=ames_train_y,
                    alpha=1)
min(cv_lasso$cvm)

pred<-predict(cv_lasso, newx=ames_test_x, s=cv_lasso$lambda.min)
mean((ames_test_y-pred)^2)


# caret & h2o

library(caret)

train_control <- trainControl(method = "cv", number = 10)

caret_mod <- train(
    x = ames_train_x, 
    y = ames_train_y,
    method = "glmnet", 
    prePro = c("center","scale","zv","nzv"),
    trControl = train_control,
    tuneLength = 10
)

head(caret_mod$results)

