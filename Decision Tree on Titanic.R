
data <- read.csv('C:/Users/USER/Desktop/��Ƭ��/R resourse/R Training/titan_train.csv')

require(rpart)

str(data)

# �����ưϤ��� train=0.8, test=0.2 
set.seed(22)
train.index <- sample(x=1:nrow(data), size=ceiling(0.8*nrow(data) ))
train <- data[train.index, ]
test <- data[-train.index, ]


str(train)
str(test)

train$Name <- NULL
train$Cabin<-NULL
train$Ticket<-NULL
str(train)

test$Name <- NULL
test$Cabin<-NULL
test$Ticket<-NULL
str(test)


# CART���ҫ��G��s���P�_���ܼ�(Survived)���@Y�A�ѤU���ܼƷ��@X
cart.model<- rpart(Survived ~. , 
                   data=train,
                   method='class')

cart.model


#�@��
require(rpart.plot) 

prp(cart.model,         # �ҫ�
    faclen=0,           # �e�{���ܼƤ��n�Y�g
    fallen.leaves=TRUE, # ����K�H�����覡�e�{
    shadow.col="gray",  # �̤U�����`�I��W���v
    # number of correct classifications / number of observations in that node
    )

install.packages('partykit')
require(partykit)   

rparty.tree <- as.party(cart.model) # �ഫcart�M����
rparty.tree # ��X�U�`�I���ӳ���T
plot(rparty.tree) 



pred <- predict(object=cart.model, newdata=test, type="class")

# ��table�ݲV�c�x�}
table(real=test$Survived, predict=pred)

#�p��Accuracy
confus.matrix <- table(real=test$Survived, predict=pred)
sum(diag(confus.matrix))/sum(confus.matrix) # �﨤�u���ƶq/�`�ƶq



#�װž�

# ���[��װŪ���ACP���N���𪺦��������װѼ�
printcp(cart.model)
plotcp(cart.model)

# �Q�ί�ϨM����㦳�̤p�~�t��CP�ӭװž�
prunetree_cart.model <- prune(cart.model, 
                              cp = cart.model$cptable[which.min(cart.model$cptable[,"xerror"]),"CP"]) 

prunetree_cart.model

#���ؼҫ�
prunetree_pred <- predict(prunetree_cart.model, newdata=test, type="class")

# ��table�ݹw�������p
table(real=test$Survived, predict=prunetree_pred)

prunetree_confus.matrix <- table(real=test$Survived, predict=prunetree_pred)
sum(diag(prunetree_confus.matrix))/sum(prunetree_confus.matrix) 



#K-fold Cross-Validation
    
require(caret)
require(e1071)

train_control <- trainControl(method="cv", number=10)

train$Survived<-train$Survived[is.na(train$Survived)==FALSE]
# for(i in train$Survived){
#     if(is.na(i)==TRUE){
#         print("+")
#     }
# }

train$Survived<-as.factor(train$Survived)

train_control.model <- train(Survived~., data=train, method="rpart", 
                             trControl=train_control, 
                             na.action=na.omit)

train_control.model

