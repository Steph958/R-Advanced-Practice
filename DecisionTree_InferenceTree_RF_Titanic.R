
data <- read.csv('C:/Users/USER/Desktop/��Ƭ��/R resourse/R Training/titan_train.csv')
head(data)

str(data)

#�N��ƭ��s�H���ƦC
shuffle_index <- sample(x = 1:nrow(data))
data <- data[shuffle_index,]
head(data)


summary(data)
str(data)


#��ƫe�B�z

data$Cabin<-NULL
data$Name<-NULL
data$Ticket<-NULL


library(dplyr)

data$Pclass = factor(data$Pclass, levels = c(1, 2, 3), 
                       labels = c('Upper', 'Middle', 'Lower'))

data$Survived = factor(data$Survived, levels = c(0, 1), 
                         labels = c('No', 'Yes'))

str(data)


#�Ϥ��V�m��ƩM���ո��
#�n�T�O��ո�ƶ������٤�Ҥ��n�t���Ӥj
ones<-data[which(data$Survived=="Yes"),]
zeros<-data[which(data$Survived=="No"),]

set.seed(100)

ones_train_row<-sample(1:nrow(ones),0.8*nrow(ones))
zeros_train_row<-sample(1:nrow(zeros),0.8*nrow(zeros))

#�V�m���
train_ones<-data[ones_train_row,]
train_zeros<-data[zeros_train_row,]
train<-rbind(train_ones,train_zeros)
#���ո��
test_ones<-data[-ones_train_row,]
test_zeros<-data[-zeros_train_row,]
test<-rbind(test_ones,test_zeros)

dim(train)
dim(test)

#�ˬd���Χ�����ƶ��j�p�P�ؼ��ܼƪ����G���
prop.table(table(train$Survived))
prop.table(table(test$Survived))



#�إ߼ҫ�
library(rpart)
library(rpart.plot)

fit <- rpart(formula = Survived ~ ., 
             data = train, 
             method = 'class',   # anova/poisson/class/exp
             control = rpart.control(),  #�ƫe�վ�/�װž�
             na.action = na.rpart)   #�����w�]�ȡA�ϥ�CART�t��k����surrogate variables���w�����

#�@��
rpart.plot(fit, extra= 106)

#�N�W�h�L�X
rpart.rules(x=fit,cover=TRUE) #cover�h�N���Ӹ`�I�[����ƭӼƥe��

#�˵���e����(cross-validation)�����Pcp��(complexity parameter)�U�����~�v
printcp(x=fit)

plotcp(x = fit)



#�w��
pred<-predict(object=fit,newdata=test,type='class')
pred


#�����ҫ�
#�ϥβV�c�x�}
tbl<-table(pred,test$Survived)
tbl

# Accuracy
accuracy <- sum(diag(tbl)) / sum(tbl)
accuracy
#[1] 0.8196262


#�װž�
#�ƫ�װ�(post-prune)

# fit$cptable
# fit$cptable[,"xerror"]

#�������e���Ҥ��۹�~�t���ܶq�̤p��cp��
fit_prune<-prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])


# plot the pruned tree 
rpart.plot(fit_prune, extra= 106, tweak = 1.1, shadow.col = "gray", branch.lty = 3, roundint = TRUE)
# branch.lty = 3:�𪺰���

#�A���w��
pred_prune <- predict(object = fit_prune,newdata = test,type = 'class')
tbl_prune <- table(pred_prune,test$Survived)
tbl_prune

# Accuracy
accuracy <- sum(diag(tbl_prune)) / sum(tbl_prune)
accuracy
#[1] 0.8224299



# K-folds Cross Validation
library(caret)
library(e1071)

#��ܭ���˪���k
train_control<-trainControl(method="cv",number=10) #k=10

train_control_model<-train(Survived~ ., data=train,
                           method = 'rpart',
                           na.action = na.pass, 
                           trControl = train_control)
train_control_model

# �i��10����e���Ҫ��������T�v��82.29%�A�P�װū᪺��ҫ����T�v82.24%�S���Ӥj�t���]�t���ʤ���b1%�H���^�C
# ���ܼҫ��S��overfitting�����D�C


#�N�Ѽ�na.action�վ㬰na.rpart�]�ϥ�CART�����N�z�ܼ�surrogate variables�ӹw��)
train_control_model_2<- train(Survived ~ ., data = train, 
                              method = 'rpart',
                              na.action = na.rpart, 
                              trControl = train_control)
train_control_model_2
# Highest Accuracy = 80.90%




#�M��L�ҫ�������@:������׾�(Conditional Inference Tree)
install.packages('party')
library(party)

str(train)

#�e�B�z
train$Sex<-factor(train$Sex)
train$Embarked<-factor(train$Embarked,level=c('C','Q','S'), labels=c(1,2,3))
test$Sex<-factor(test$Sex)
test$Embarked<-factor(test$Embarked,level=c('C','Q','S'), labels=c(1,2,3))
str(train)
str(test)

fit_ctree<-ctree(Survived~ .,data=train)
#ctree�t��kna.action�w�]��na.pass�A�i�H�N��אּna.rpart
plot(fit_ctree)

pred_ctree<-predict(object=fit_ctree, newdata=test)
tbl_ctree<-table(predicted=pred_ctree, actuals=test$Survived)
tbl_ctree

#Accuracy
accuracy<-sum(diag(tbl_ctree)/sum(tbl_ctree))
accuracy
#[1] 0.7943925





#�M��L�ҫ�������G:�H���˪L(Random Forest)

#�Ѷ����ǲߪk�إߪ��ҫ����ण�e���o�͹L�װt�A�����D
#��C��ˡA�i�H�����ѨM�]���O������(Class Imbalance)��w���a�Ӫ����D
#����ˡA�h�i�ѨM�����]�@�u��(collinearity)��w���y�������D�C

install.packages('randomForest')
library(randomForest)

set.seed(100)
fit_rf<-randomForest(Survived~ ., data=train, 
                     na.action=na.omit,
                     importance=TRUE)  #�w�]�ϥ�FALSE�A�u��the mean decrease in Gini index
fit_rf

#Accuracy
fit_rf$confusion
tbl_rf<-fit_rf$confusion[,c(1,2)]
accuracy<-sum(diag(tbl_rf)/sum(tbl_rf))
accuracy
#[1] 0.9019264


#�C�W�[�C�@���M����A����~�t�����ܶq
plot(fit_rf)
#�o�{100�h�ʾ�NOK�F
# 
# �~�t��OOB(out-of-bag) Erro Rates
# �����������(classification tree)�A�¦��u���ܾ��骺OOB error rate�A�Ө�L�C���u���ܦU���O��OOB Error Rate�C
# �����^�k���(regression tree)�A�~�t��OOB(out-of-bag) MSE�A�u�|���@���¦��u�N�����骺OOB MSE�C



#�վ�C�@��`�I���ή��H����ˤ��ܼƼƶq
train_naomit<-na.omit(train)
tuneRF(x=train_naomit[,-2],y=train_naomit[,2])# �ĤG���OSurvived

#�o�{�bmtry=2�ɡA�~�t�̤p
#randomForest�����O��w�]��mtry=sqrt(p)�A�䤤p�N��x�ܼƪ��ƥ�
fit_rf$mtry
#[1] 2


#�ݨC���ܼƪ����n��(importance)
# the mean decrease in Gini index
round(importance(fit_rf),2)
varImpPlot(fit_rf)


#�w���õ���
pred_rf<-predict(object=fit_rf, newdata=test)
tbl_rf<-table(predicted = pred_rf, actuals=test$Survived)
accuracy<-sum(diag(tbl_rf)/sum(tbl_rf))
accuracy
#[1] 0.8358734
#�ثe�̨Ϊ��{













