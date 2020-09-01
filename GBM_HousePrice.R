

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
    n.trees=10000, #���N����
    interaction.depth=1, #�z�ҫ������μ�
    shrinkage=0.001, #�ǲ߲v
    cv.folds=5, #��e����
    n.cores=NULL, #�ϥ�CPU�֤ߪ��ƶq�A�w�]��
    verbose=FALSE))  #�{�ǩM���ī��Ъ��L�X
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
# �̨έ��N����:9998


# Tuning

# create hyperparameter grid
hyper_grid <- expand.grid(
    shrinkage = c(.01, .1, .3), 
    interaction.depth = c(1, 3, 5), #�ҫ������μ� 
    n.minobsinnode = c(5, 10, 15), # �`�I�̤p�[���ȭӼ�
    bag.fraction = c(.65, .8, 1), # �ϥ��H����פU��(<1)
    optimal_trees = 0,               # �x�s�̾A�ҫ������
    min_RMSE = 0                     # �x�s�̤p����t�����
)


nrow(hyper_grid)
# 81

# �ϥ�train.fraction�Ѽƫe�A��randomize data
random_index <- sample(1:nrow(ames_train), nrow(ames_train))
random_ames_train <- ames_train[random_index, ]

#grid search 
for(i in 1:nrow(hyper_grid)) {

    set.seed(123)
    
    gbm.tune <- gbm(
        formula = Sale_Price ~ .,
        distribution = "gaussian",
        data = random_ames_train,
        n.trees = 5000, # �ϥ�5000�Ӿ�ҫ�
        interaction.depth = hyper_grid$interaction.depth[i],
        shrinkage = hyper_grid$shrinkage[i],
        n.minobsinnode = hyper_grid$n.minobsinnode[i],
        bag.fraction = hyper_grid$bag.fraction[i],
        train.fraction = .75, # �ϥ�75%���V�m��ơA25%��OOB���ĵ���/����>>���Xvalid error
        n.cores = NULL,
        verbose = FALSE
    )
    
    hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
    hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

#��X���G
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



# �ھڵ��G�A�վ�Ѽư϶��P�ƭ�
hyper_grid_2 <- expand.grid(
    shrinkage = c(.01, .05, .1), # �E�J��p���ǲߨB��
    interaction.depth = c(3, 5, 7), #�E�J>1�����μ�
    n.minobsinnode = c(5, 7, 10), # �E�J��p���`�I�[���ȼƶq
    bag.fraction = c(.65, .8, 1), # ����
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

#��X���G
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
        train.fraction = 1, # �p�G�ϥ�<1(xx%)���V�m��ҡA�N�|�γѾl��(1-XX%)��Ʀ��pOOB�~�t
        cv.folds = 4, # ���O��ϥ�OOB���p�~�t�A�ڭ̦��p��í����CV�~�t
        n.cores = NULL, 
        verbose = FALSE
        
    )
)
# #
# user  system elapsed 
# 48.21    3.80  117.31


sqrt(min(gbm.fit.final$cv.error))
# 21973.2



#��ı��

par(mar = c(5, 8, 1, 1))

summary(
    gbm.fit.final, 
    cBars = 10, 
    plotit = TRUE, 
    method = relative.influence, # The function used to compute the relative influence. ��i�ϥ�permutation.test.gbm
    las = 2
)


install.packages('vip')
library(vip)
vip::vip(gbm.fit.final)



#PDPs: ø�s�S�w�ܼ�����ܰʳy���������ؼйw���Ȫ��ܰ�

gbm.fit.final %>%
    partial(object = .,# A fitted model object of appropriate class (e.g., "gbm", "lm", "randomForest", "train", etc.).
            pred.var = "Gr_Liv_Area", 
            n.trees = gbm.fit.final$n.trees, # �p�G�Ogbm���ܡA�ݫ��w�ҫ��Ҩϥξ�Ӽ�
            grid.resolution = 100) %>%
    # The autplot function can be used to produce graphics based on ggplot2
    autoplot(rug = TRUE, train = ames_train) + # plotPartial()���䴩gbm
    scale_y_continuous(labels = scales::dollar) # �ϥ�ggplot��¦ø��


#�˵��S��ø��(plot = FALSE)���Ҧ^�Ǫ�data.frame
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



# ICE curves:ø�s�S�w�����ܼ�����ܰʹ�Ҧ��[���Ȫ��ؼмƭȪ��ܰ�
# ���j�յ��G���������
ice1 <- gbm.fit.final %>%
    partial(
        pred.var = "Gr_Liv_Area", 
        n.trees = gbm.fit.final$n.trees, 
        grid.resolution = 100,
        ice = TRUE # ��ice = TRUE�ε��wpred.fun�ѼơA�|�^�Ǩϥ�newdata���C���[���ȹw�������G
    ) %>%
    autoplot(rug = TRUE, train = ames_train, alpha = .1) + # alpha�Ѽƥu�bICE curvesø�s�W����
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
# �@���ϥ�lime()�Ыئn�Fexplainer�A�h�i�Nexplainer�Χ@�����ҫ��@�Φb�s�[��Ȫ����G
explanation <- explain(x = local_obs, 
                       explainer = explainer,
                       n_features = 5 # The number of features to use for each explanation.
)

plot_features(explanation = explanation)



#�w��

pred <- predict(gbm.fit.final, 
                n.trees = gbm.fit.final$n.trees, 
                ames_test)

# results
caret::RMSE(pred, ames_test$Sale_Price)
# 20356.06






