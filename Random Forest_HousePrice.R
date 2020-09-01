
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

install.packages('AmesHousing') #�ϥ�AmesHousing�M�󤤪�Ames Housing Data
library(AmesHousing)

# Create training (70%) and test (30%) sets 
# Use set.seed for reproducibility
set.seed(123)
ames_split <- initial_split(data = AmesHousing::make_ames(), prop = .7)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)


# Random Forest
#�z�L�`�J��h�H���ʨ���𪺹L�{���A�̤p�ƾ�P�𤧶��������ʡA�i�@�B���C�ܲ�
#Bootstrap
#Split-variable randomization


set.seed(123)

m1<-randomForest(formula=Sale_Price~ .,
                 data=ames_train)

m1

plot(m1)
#�ҫ������~�t�j���b100�ʾ�ɶ}�l�ͩ�í�w

which.min(m1$mse)
# 280

#RMSE
sqrt(m1$mse[which.min(m1$mse)])
#[1] 25135.88  >> �o�O�ϥ�OOB Error�ҭp��ӱo��



#�Y�Q�ϥ�Validation Set:
set.seed(123)

#�̾�8:2����ҧ�쥻���V�m��ƦA�����V�m�M���Ҹ�ƶ�
valid_split<-initial_split(ames_train, 0.8)

ames_train_v2<-analysis(valid_split) # training data
ames_valid<-assessment(valid_split) # validation data


# �Nvalidation data��z��x_test�My_test�A�b�ҫ��V�m���ϥ�
x_test<-ames_valid[setdiff(names(ames_valid), "Sale_Price")] #��Sale_Price���q������
y_test<-ames_valid$Sale_Price  #Sale_Price���

# �brandomForest��Ƥ��ϥ�x-test�My-test���@���Ҹ�ƶ����Ѽ�
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



#���OOB error & test error
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



#�װ�

#mtry : �C���b�M�w�����ܼƮɡA���H����˪���b�ܼƲM��ƶq

features<-setdiff(x=names(ames_train), y="Sale_Price")

set.seed(123) # �T�w���Pmtry�ѼƭȪ��ҫ��ҨϥΪ��H��OOB sample�O�@�˪�

m2<-tuneRF(x=ames_train[features], y=ames_train$Sale_Price,
          mtryStart=5, ntreeTry=500, stepFactor=1.5, improve=0.01, trace=FALSE)
         #mtry�q5�}�l�A�C���j�ۥ[1.5�A�ұo��쪺OOB error�A����OOB error�ﵽ�����פ��W�L1%����

#���G�o�{��mtry > 15��OOB�}�l���A�U��(�t���h�Ofeature��80��1/5)


plot(m2)





# Full grid search with ranger
#�]��randomForest()�L�k���Ī��B��j���ƾڹB��

#����@�U�Ҫ�O���ɶ�
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



#�إߤ@��hyperparameters grid
hyper_grid<-expand.grid(mtry=seq(20,30,2),
                        node_size=seq(3,9,by=2),  
                        # nodesize : ����(��)�`�I�̤p�[���ƭӼ�
                        sample_size=c(0.55,0.632,0.7,0.8), 
                        # sampsize : �V�m�C�ʾ�ҫ����˥��Ƥj�p�A�w�]�O�ϥ�63.25%�V�m��ƶ������
                        #�@��ӻ��A�ڭ̮ե����˥��j�p�ѼƮɷ|�ϥ�60-80%�����
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
                    seed=123)   #�T�w�H���üƺؤl�A���P��sample_size�Ѽƭȩҹ�������˼˥��i�H�ۦP
                               #�Y���L�Ѽ��ܤƩұa�Ӫ��ĪG
    
    hyper_grid$OOB_RMSE[i]<- sqrt(model$prediction.error)    
}


#�ڭ̱N���G�̧�OOB_RMSE�Ѥp�ܤj�ƦC�A���e�Q�W
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
# OOB_RMSE�j�P���b26K���k
# �̾Amtry���ȸ��b�Ҧ�20~30�d��϶��A����mtry�b���϶����OOB_RMSE�S���Ӥj�v�T�C
# �̾A�̤p�`�I�[��ȼƶq�j�����b3~5
# �̾A��ˤ�Ҭ���0.8
# ���ܩ�ˤ�Ұ�(~80%)�M�`�׸���(���`�I�[���ӼƤj�p3~5)���H���˪L���ĸ��n(OOB RMSE)





#�ոձN���O�ܼƭ��s�s�X��dummy variables�O�_�ണ��random forests���w�����{

to_dummy<-dummyVars(formula= ~.,data=ames_train, fullRank=FALSE)

ames_to_dummy<-predict(to_dummy, newdata=ames_train)%>%as.data.frame()

names(ames_to_dummy)<-make.names(names=names(ames_to_dummy), allow_=FALSE)


#�إ�hyperparameter grid
#�ñNmtry���϶��վ㬰��j�d��

hyper_grid_2 <- expand.grid(
    mtry = seq(50, 200, by = 25),
    node_size  = seq(3, 9, by = 2),
    sampe_size = c(.55, .632, .70, .80),
    OOB_RMSE  = 0
)

for(i in 1:nrow(hyper_grid_2)){
    model <- ranger(
        formula = Sale.Price ~.,
        data = ames_to_dummy,  #��ε����ܼ�
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
# OOB RMSE ���b27K���k�A�èS�������O�ܼƭ��s�s�X�e��26K�ӱo�n
# �N���O�ܼƭ��s�s�X��dummy variables�L�k���ɼҫ�����


#�ϥΥثe���̨ΰѼ�
#���ư���100���A�p��error rate�������
OOB_RMSE<-vector(mode="numeric", length=100)

for(i in 1:length(OOB_RMSE)){
    optimal_ranger<-ranger(formula=Sale_Price~ .,
                           data=ames_train,
                           num.trees=500,
                           mtry=28,
                           min.node.size=3,
                           sample.fraction=0.8,
                           importance='impurity'
                  #�̾ڸ`�I���«�(node impurity)���ﵽ�T�רӿŶq�C���ܼƪ����n��
                  #�L�k�z�L���ܼƤ��Ωҭ��C���ҫ����~�v�A�٧@node impurity
                          )
    OOB_RMSE[i]<-sqrt(optimal_ranger$prediction.error)
}

hist(OOB_RMSE, breaks=20)



#�N�ܼƭ��n�ʵ��Gø�X
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
