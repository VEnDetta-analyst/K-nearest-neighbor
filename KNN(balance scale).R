library(class)
library(kknn)
balance_scale <- read.csv("https://ibm.box.com/shared/static/684jzm7e6fbbssg87yc2v4dy53dgkdew.txt", sep = ",")
str(balance_scale)
head(balance_scale)
colnames(balance_scale)=c("Class name","left_weight","left_distance","right_weight","right_distance")
right_prod=balance_scale[,4]*balance_scale[,5]
left_prod=balance_scale[,2]*balance_scale[,3]
difference=right_prod-left_prod
balance_scale=cbind(balance_scale,right_prod,left_prod,difference)

#separating into training & testing data
set.seed(1234)
ind=sample(2,nrow(balance_scale),replace=TRUE,prob=c(0.7,0.3))
bscale.train=balance_scale[ind==1,6:8]
bscale.test=balance_scale[ind==1,6:8]
bscale.trainLabels= balance_scale[ind==1, 1] 
bscale.testLabels= balance_scale[ind==2, 1]
#implementation of k-nearest neighbor for classification
knn_class=knn(train = bscale.train,test = bscale.test,cl=bscale.trainLabels,k=2)
#To find no of incorrectly classified points
correct=which(knn_class==bscale.testLabels,arr.ind = TRUE)
incorrect=which(knn_class==bscale.testLabels,arr.ind= FALSE)
length(incorrect)
#proportion of correctly classified points
proportion_correct = length(correct)/length(bscale.testLabels)
#k nearest neighbor regression and display estimated difference & true difference
knn_reg = kknn(formula = difference ~ ., train=bscale.train, test=bscale.test, k=2)
head(cbind(knn_reg$fitted.values,bscale.test$difference))
#no of incorrectly classified points
incorrect_reg=which(knn_reg$fitted.values!=bscale.test$difference,arr.ind = TRUE)
length(incorrect_reg)
#proportion of correctly classified points or accuracy of model
correct_reg=which(knn_reg$fitted.values==bscale.test$difference,arr.ind=FALSE)
length(correct_reg)/length(bscale.test$difference)
#To find optimal value of k
best_reg=train.kknn(formula = difference~., data=bscale.train, kmax = 8)
best_reg
