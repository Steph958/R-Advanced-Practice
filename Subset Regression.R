
# R Code for Stepwise Regression


# 這裡拿套件lasso2中的前列腺癌症資料示範
# 依變數是lpsa(Log Prostate Specific Antigen)
# 其他的變數則作為自變數

install.packages("lasso2")

data(Prostate, package="lasso2")

str(Prostate)


# 先把資料區分成 train=0.8, test=0.2 
set.seed(22)
train.index <- sample(x=1:nrow(Prostate), size=ceiling(0.8*nrow(Prostate) ))

train = Prostate[train.index, ]
test = Prostate[-train.index, ]

str(train)
str(test)




# 使用Forward Stepwise Regression ：

# 值得注意的是，其中有一個 scope 參數，是用來描述模型的完整度(如果不設定，預設是會進行 backward)

null = lm(lpsa ~ 1, data = train)  #建立空的線性迴歸(只有截距項)

full = lm(lpsa ~ ., data = train) # 建立上界，也就是完整的線性迴歸

forward.lm = step(null, 
                  # 從空模型開始，一個一個丟變數
                  # 最大不會超過完整的線性迴歸
                  # (一定要加上界 upper=full
                  scope=list(lower=null, upper=full), 
                  direction="forward")


summary(forward.lm)



# Backward Stepwise Regression

# 1. 先建立一個完整的線性迴歸
full = lm(lpsa ~ ., data = train)  

backward.lm = step(full, 
                   # 這裡可以加下界(lower=null)，也可以不加
                   scope = list(upper=full), 
                   direction="backward") 

summary(backward.lm)









# Both Regression
 
# 因此從 null 開始或是從 full 開始都可以，只不過兩者的結果會不一樣(可以思考看看為什麼會這樣)

both_lm_1 <- step(null, scope = list(upper=full), direction="both")

both_lm_2 <- step(full, scope = list(upper=full), direction="both")  


summary(both_lm_1)

summary(both_lm_2)








#預測:

# 比較三個模型(full, forward, backward)的預測效果：


# self-defined 的 R-squared 函式
R_squared <- function(actual, predict){
    mean_of_obs <- rep(mean(actual), length(actual))
    
    SS_tot <- sum((actual - mean_of_obs)^2)  #總變異
    
    SS_reg <- sum((predict - mean_of_obs)^2) #可解釋之變異
    
    #SS_res <- sum((actual - predict)^2)     #不可解釋之變異
    
    R_squared <- SS_reg/SS_tot               #1 - (SS_res/SS_tot)
    R_squared
}


# 直接用 predict()來預測
lm.test = predict(full, test)
forward.test = predict(forward.lm, test)
backward.test = predict(backward.lm, test)

c(R_squared(test$lpsa, lm.test),
  R_squared(test$lpsa, forward.test),
  R_squared(test$lpsa, backward.test)
)





