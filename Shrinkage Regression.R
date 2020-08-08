# R Code for Shrinkage Regression

install.packages("glmnet")

require(glmnet)


data(Prostate, package="lasso2")
str(Prostate)


# 把資料區分成 train=0.8, test=0.2 
set.seed(22)
train.index = sample(x=1:nrow(Prostate),
                     size=ceiling(0.8*nrow(Prostate)))


train = Prostate[train.index, ]
test = Prostate[-train.index, ]

str(train)
str(test)




ridge = glmnet(x = as.matrix(train[, -9]), 
               y = train[, 9], 
               alpha = 0,  #0(Ridge) 或 1(Lasso)
               family = "gaussian")
#若y是連續值，設"gaussian"；
#若y是二元分類，設"binomial"；
#若y是多元分類，設"multinomial"。
ridge



lasso = glmnet(x = as.matrix(train[, -9]), 
               y = train[, 9], 
               alpha = 1,
               family = "gaussian")
lasso




#做圖，X 軸為 lambda(懲罰值) ， Y 軸為各變數的係數值。
par(mfcol = c(1, 2))
plot(lasso, xvar='lambda', main="Lasso")
plot(ridge, xvar='lambda', main="Ridge")






######################################################################################################################################################################

# 找出最佳lambda

#利用 Cross Validation 的手法，驗證在不同 lambda 值下模型的表現

# 然後取殘差最小的(表現最好)模型



cv.lasso = cv.glmnet(x = as.matrix(train[, -9]), 
                     y = train[, 9], 
                     alpha = 1,  # lasso
                     family = "gaussian")

cv.lasso


# 評估每個模型的 cvm(mean cross-validated error)後
# 取最小 cvm 模型所對應的 lambda
best.lambda = cv.lasso$lambda.min
best.lambda
#  0.005795017


# 藍色垂直虛線就是最佳 lambda 的所在位置
# 跟其他線相交的位置就是該變數收縮後的係數
plot(lasso, xvar='lambda', main="Lasso")
abline(v=log(best.lambda), col="blue", lty=5.5 )



# Lasso的變數挑選
# 觀察哪些變數被挑選出來，其係數不為 0的那些
coef(cv.lasso, s = "lambda.min")

#取出這些重要變數的名稱：
select.ind = which(coef(cv.lasso, s = "lambda.min") != 0)


select.ind = select.ind[-1]-1 # remove `Intercept` and 平移剩下的ind
select.ind # 第幾個變數是重要的 


select.varialbes = colnames(train)[select.ind]
select.varialbes


lm(lpsa ~ ., train[, c(select.varialbes, "lpsa")])


#預測:

#先用 glmnet() 建立基本的 Ridge / Lasso 模型
ridge = glmnet(x = as.matrix(train[, -9]), 
               y = train[, 9], 
               alpha = 0, # ridge
               family = "gaussian")

# 用 cv.glmnet() 找出最佳的懲罰值 best.lambda
cv.ridge = cv.glmnet(x = as.matrix(train[, -9]), 
                     y = train[, 9], 
                     alpha = 0,  # ridge
                     family = "gaussian")

best.ridge.lambda = cv.ridge$lambda.min


# 使用 predict()進行預測
ridge.test = predict(ridge,                 #使用的模型
                     s = best.ridge.lambda,  #最佳的Lambda值
                     newx = as.matrix(test[, -9]))  #把test的第9欄(Ipsa)拿掉

# 評估模型
R_squared(test$lpsa, ridge.test)