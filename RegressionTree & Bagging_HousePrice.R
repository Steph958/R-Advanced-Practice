
# ������O�Ψӧ�M�u�̯�Ϥ����Ҹ�����O�v���@�t�C�ܼ�
# �j�k��(regression tree)�h�O�Ψӧ�M�u�̯�Ϥ��ؼгs���ܼƬ۪�סv���@�t�C�ܼ�
# �j�k�𪺥ؼ��ܼƬO�s���ܼ�

# ��@�M����ҫ������G��í�w�װ��]high variance�^�A�w����O�]���z
# �h�b�|�f�t�ϥ�bootstrap aggregating(or Bagging)(�@�ض����ǲߪkensemble learning)


library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)       # bagging

#�d�Ҹ�ƫh�O�ϥ�AmesHousing package����Ames Housing�ƾڡC
#�N��Ƥ���70%�V�m���A30%���ն��G

library(AmesHousing)

set.seed(123)
ames_split <- initial_split(AmesHousing::make_ames(), prop = .7)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)

m1<-rpart(formula=Sale_Price ~ .,data=ames_train, method="anova")
m1

rpart.plot(m1)

plotcp(m1) 
# �Ux�b��cp or cost complexity parameter (�\)
# �Wx�b������`�I��(number of terminal nodes, |T|)
# y�b����e���һ~�t(X-val relative error)
#��ĳ�i�H�����ϥλP�̤pX-val error�۶Z�@�ӼзǮt�H���ҹ�����Tree Size(|T|)�ӧ@���׾𪺳̨Τj�p�]1-SE rule�^


#�S�������g�@����s�b�A���M�������̤j�̧��㪺��......
# m2 <- rpart(
#     formula = Sale_Price ~ .,
#     data    = ames_train,
#     method  = "anova", 
#     control = list(cp = 0, xval = 10)
# )
# 
# {plotcp(m2)
#     abline(v = 12,lty = "dashed", col = "red")}



#�ԲӪ�cp�ȩM������X-Val error
m1$cptable


#�װž�
m3<-rpart(formula=Sale_Price ~ ., data=ames_train,
          method="anova",
          control=list(minsplit=10,#���K�e�̤p�һݸ�Ƶ���
                       maxdepth=12,#�qroot nodes��`�I��terminal nodes���`�I�����̤j�����`�I�ƶq
                       xval=10))
m3$cptable


#�ϥ�grid search����k
#�۰ʰ���P������P�ѼƤ��ǭȲզX���ĪG
#�è̾ڦ��ӿ�ܳ̾A�X���ҫ��ѼƳ]�w

#�إ�grid
hyper_grid<-expand.grid(
    minsplit=seq(5,20,1),#�q5��20
    maxdepth=seq(8,15,1)#�q8��15
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


#�C�@�ռҫ����̤p��e���һ~�t
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
    top_n(-5, wt=error)#�̾�error�D�X�e���W�C��


#���s�ؼ� & �w��
optimal_tree<-rpart(formula=Sale_Price~ .,
                    data=ames_train,
                    method="anova",
                    control=list(minsplit=14, max_depth=10, cp=0.01))

pred<-predict(optimal_tree, newdata=ames_test)
RMSE(pred=pred, obs=ames_test$Sale_Price)
#Root Mean Squared Error
#[1] 39852.01



#Boostrap Aggregation (�κ�Bagging)
#��X�M�����h�ռҫ����w�����G
#���ĭ��C�Ӧ۳�@�ҫ����ܲ��סA�åB�קK�L�װt�Aoverfitting
#�D�n�٬O���㦳���ܲ��ת��ҫ������ĪG
#�@��Bootstrap sample�|�]�t63%(2/3)���V�m��ƶ��A�M33%(1/3)���bbootstrapped sample����out-of-bag (OOB) sample�C
#�i�H�z�L�o��OOB sample�ӿŶq�ҫ����ǽT�סA���ͤ@�Ӧ۵M�ӵM����e���ҹL�{


set.seed(123)
bagging_m1<-bagging(formula=Sale_Price~ .,
                    data=ames_train,
                    coob=TRUE)
bagging_m1
#RMSE: 36991.67

#bagging�w�]�|����25��bootstrap sample�M��ҫ�

#�[��10~50�ʾ�:
ntree<-10:50

rmse<-vector(mode="numeric", length=length(ntree)) #40��

for(i in seq_along(ntree)){
    
    set.seed(123) #�T�wbootstrap�üƵ��G
    
    model<-bagging(formula=Sale_Price~ .,
                   data=ames_train,
                   coob=TRUE,
                   nbagg=ntree[i])
    
    rmse[i]<-model$err
}

{plot(x=ntree, y=rmse, type="l", lwd=2)
    abline(v=25, col="red", lty="dashed")}
#�o�{�t���h�b��ƶq��25�ɻ~�t�����ͩ�í�w




#���caret�Ӷi��bagging

ctrl<-trainControl(method="cv",number=10) 
#�ϥ�cross validation�i�H���ѱj�j���u��test error���~�t����ȡC(instead of OOB error)

bagging_cv<-train(Sale_Price~ .,
                  data=ames_train,
                  method="treebag",
                  trControl=ctrl,
                  importance=TRUE)

bagging_cv
# RMSE: 35854.02

#�ݫe20�W�����n�ܼ�
plot(varImp(bagging_cv),20)


#�w���õ���:
pred=predict(object=bagging_cv, newdata=ames_test)
RMSE(pred=pred, obs=ames_test$Sale_Price)
#[1] 35357.89
#�ثe�̨Ϊ��{

#�i�H�o�{cross validation(36477)�M�M�Φbtest set(out of sample)�����p���~�v(estimated error)(35263)�D�`�۪�
#�]��cv�b�V�m�L�{���O�ϥ�test error�i������









