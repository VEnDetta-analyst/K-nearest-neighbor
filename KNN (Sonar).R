#Knn
install.packages("class")
install.packages("caret")
install.packages("mlbench")
install.packages("e1071")

library(class)
library(caret)
require(mlbench)
library(e1071)
library(base)
require(base)
#Data prepartion & exploration
data(Sonar)
head(Sonar)
nrow(Sonar)
ncol(Sonar)
table(Sonar$Class)#to find M & R classes in data
apply(Sonar,2,function(x) sum(is.na(x)))#To check whether data contains any NA in its columns
#taking manually samples and to split into training & test.
seed=123
set.seed(seed)
data=Sonar[sample(nrow(Sonar)),]
bound=floor(0.7*nrow(data))
bound
df_train = data[1:bound, ] 
df_test= data[(bound+1):nrow(data),]
nrow(df_train)
nrow(df_test)

#To check our sample is splitted with same proportion
table(df_train$Class)/nrow(df_train)
table(df_test$Class)/nrow(df_test)

x_train=subset(df_train,select=-Class)
y_train=df_train$Class
x_test=subset(df_test,select=-Class)
y_test=df_test$Class

#Training model on data
model=knn(train=x_train,test=x_test,cl=y_train,k=3)
model
#Evaluate model performance
conf_mat=table(y_test,model)
accuracy=sum(diag(conf_mat))/sum(conf_mat)
accuracy
#To check overfitting/underfitting we use knn.cv which does leave one out cross validation
knn_cv=knn.cv(train=x_train,cl=y_train,k=3)
conf_mat_cv=table(y_train,knn_cv)
accuracy_cv=sum(diag(conf_mat_cv))/sum(conf_mat)
accuracy_cv
accuracy_cv-accuracy
#The difference between the cross-validated accuracy and the test accuracy shows that,  ????=3  leads to overfitting. Perhaps we should change  ????  to lessen the overfitting.
