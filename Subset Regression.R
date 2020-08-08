
# R Code for Stepwise Regression


# �o�̮��M��lasso2�����e�C�����g��ƥܽd
# ���ܼƬOlpsa(Log Prostate Specific Antigen)
# ��L���ܼƫh�@�����ܼ�

install.packages("lasso2")

data(Prostate, package="lasso2")

str(Prostate)


# �����ưϤ��� train=0.8, test=0.2 
set.seed(22)
train.index <- sample(x=1:nrow(Prostate), size=ceiling(0.8*nrow(Prostate) ))

train = Prostate[train.index, ]
test = Prostate[-train.index, ]

str(train)
str(test)




# �ϥ�Forward Stepwise Regression �G

# �ȱo�`�N���O�A�䤤���@�� scope �ѼơA�O�ΨӴy�z�ҫ��������(�p�G���]�w�A�w�]�O�|�i�� backward)

null = lm(lpsa ~ 1, data = train)  #�إߪŪ��u�ʰj�k(�u���I�Z��)

full = lm(lpsa ~ ., data = train) # �إߤW�ɡA�]�N�O���㪺�u�ʰj�k

forward.lm = step(null, 
                  # �q�żҫ��}�l�A�@�Ӥ@�ӥ��ܼ�
                  # �̤j���|�W�L���㪺�u�ʰj�k
                  # (�@�w�n�[�W�� upper=full
                  scope=list(lower=null, upper=full), 
                  direction="forward")


summary(forward.lm)



# Backward Stepwise Regression

# 1. ���إߤ@�ӧ��㪺�u�ʰj�k
full = lm(lpsa ~ ., data = train)  

backward.lm = step(full, 
                   # �o�̥i�H�[�U��(lower=null)�A�]�i�H���[
                   scope = list(upper=full), 
                   direction="backward") 

summary(backward.lm)









# Both Regression
 
# �]���q null �}�l�άO�q full �}�l���i�H�A�u���L��̪����G�|���@��(�i�H��Ҭݬݬ�����|�o��)

both_lm_1 <- step(null, scope = list(upper=full), direction="both")

both_lm_2 <- step(full, scope = list(upper=full), direction="both")  


summary(both_lm_1)

summary(both_lm_2)








#�w��:

# ����T�Ӽҫ�(full, forward, backward)���w���ĪG�G


# self-defined �� R-squared �禡
R_squared <- function(actual, predict){
    mean_of_obs <- rep(mean(actual), length(actual))
    
    SS_tot <- sum((actual - mean_of_obs)^2)  #�`�ܲ�
    
    SS_reg <- sum((predict - mean_of_obs)^2) #�i�������ܲ�
    
    #SS_res <- sum((actual - predict)^2)     #���i�������ܲ�
    
    R_squared <- SS_reg/SS_tot               #1 - (SS_res/SS_tot)
    R_squared
}


# ������ predict()�ӹw��
lm.test = predict(full, test)
forward.test = predict(forward.lm, test)
backward.test = predict(backward.lm, test)

c(R_squared(test$lpsa, lm.test),
  R_squared(test$lpsa, forward.test),
  R_squared(test$lpsa, backward.test)
)




