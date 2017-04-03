train <- read.csv("data.csv", header = TRUE)
#Label Diagnosis
train$diagnosis1 = as.numeric(train$diagnosis == "M")

#Check NA
head(train)
summary(train)
sapply(train, function(x) {length(which(is.na(x)))})

#Histogram
hist(train$diagnosis1)
M <- hist(subset(train, diagnosis == "M")$radius_mean, breaks = 30)
B <- hist(subset(train, diagnosis == "B")$radius_mean, breaks = 20)
plot(M, col = rgb(0,0,1,1/4), xlim = c(5,30), ylim = c(0,50))
plot(B, col = rgb(1,0,0,1/4), add = T)

M <- hist(subset(train, diagnosis == "M")$texture_mean, breaks = 30)
B <- hist(subset(train, diagnosis == "B")$texture_mean, breaks = 20)
plot(M, col = rgb(0,0,1,1/4), xlim = c(5,35), ylim = c(0,50))
plot(B, col = rgb(1,0,0,1/4), add = T)

M <- hist(subset(train, diagnosis == "M")$perimeter_mean, breaks = 30)
B <- hist(subset(train, diagnosis == "B")$perimeter_mean, breaks = 20)
plot(M, col = rgb(0,0,1,1/4), xlim = c(40,200), ylim = c(0,70))
plot(B, col = rgb(1,0,0,1/4), add = T)

M <- hist(subset(train, diagnosis == "M")$area_mean, breaks = 40)
B <- hist(subset(train, diagnosis == "B")$area_mean, breaks = 20)
plot(M, col = rgb(0,0,1,1/4), xlim = c(0,3000), ylim = c(0,70))
plot(B, col = rgb(1,0,0,1/4), add = T)

M <- hist(subset(train, diagnosis == "M")$smoothness_mean, breaks = 20)
B <- hist(subset(train, diagnosis == "B")$smoothness_mean, breaks = 20)
plot(M, col = rgb(0,0,1,1/4), xlim = c(min(train$smoothness_mean),max(train$smoothness_mean)), ylim = c(0,70))
plot(B, col = rgb(1,0,0,1/4), add = T)

M <- hist(subset(train, diagnosis == "M")$compactness_mean, breaks = 30)
B <- hist(subset(train, diagnosis == "B")$compactness_mean, breaks = 20)
plot(M, col = rgb(0,0,1,1/4), xlim = c(0,max(train$compactness_mean)+0.05), ylim = c(0,70))
plot(B, col = rgb(1,0,0,1/4), add = T)

M <- hist(subset(train, diagnosis == "M")$concavity_mean, breaks = 20)
B <- hist(subset(train, diagnosis == "B")$concavity_mean, breaks = 20)
plot(M, col = rgb(0,0,1,1/4), xlim = c(0,0.5), ylim = c(0,120))
plot(B, col = rgb(1,0,0,1/4), add = T)

M <- hist(subset(train, diagnosis == "M")$concave.points_mean, breaks = 30)
B <- hist(subset(train, diagnosis == "B")$concave.points_mean, breaks = 20)
plot(M, col = rgb(0,0,1,1/4), xlim = c(0,0.25), ylim = c(0,60))
plot(B, col = rgb(1,0,0,1/4), add = T)

M <- hist(subset(train, diagnosis == "M")$symmetry_mean, breaks = 20)
B <- hist(subset(train, diagnosis == "B")$symmetry_mean, breaks = 20)
plot(M, col = rgb(0,0,1,1/4), xlim = c(0.05,0.35), ylim = c(0,60))
plot(B, col = rgb(1,0,0,1/4), add = T)

M <- hist(subset(train, diagnosis == "M")$fractal_dimension_mean, breaks = 20)
B <- hist(subset(train, diagnosis == "B")$fractal_dimension_mean, breaks = 20)
plot(M, col = rgb(0,0,1,1/4), xlim = c(0.04,0.1), ylim = c(0,60))
plot(B, col = rgb(1,0,0,1/4), add = T)

#Better plot
featurePlot(x = train[,3:12], y = train[,2], plot ="density", scales = list(x=list(relation="free"), y=list(relation="free")), auto.key=list(columns=3))
featurePlot(x = train.1[,2:6], y = train.1[,1], plot ="pairs", auto.key = list(column = 3))

#Correlation, choosing top 5 correlated factors
library(corrplot)
cor <- cor(train[,-c(1,2,13:32)],train[,-c(1,2,13:32)])
corrplot(cor, method = "square")
cor1 <- cor(train[,33],train[,3:12])
corrplot(cor(train[,33],train[,3:12]), method = "number")
train.1 <- train[,c(2,3,5,6,9,10,33)]
head(train.1)

#Split Data
Train <-createDataPartition(train.1$diagnosis1, p = 0.7, list = FALSE)
Training <- train.1[Train,]
Testing <- train.1[-Train,]

#Logit Regression
logit <- glm(diagnosis ~ .-diagnosis1, data = Training, family = "binomial")
logit

#ROC plot
library(ROCR)
prob <- predict(logit, newdata = Testing)
pred <- prediction(prob, Testing$diagnosis)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, col = "red")
abline(0,1, col = "gray")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc  #0.9802

#Cross Validation
library(boot)
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
cv.logit <- cv.glm(Training, logit, K=5, cost = cost)
1-cv.logit$delta[1]
plot(cv.logit)

#Accuracy on Test data
pred <- predict(logit, newdata = Testing) >= 0.5
accuracy <- table(pred, Testing[,1])
sum(diag(accuracy))/sum(accuracy) #0.9176

#Decision Tree
library(rpart)
formula <- paste("diagnosis ~ .-diagnosis1")
set.seed(1)
tree.1 <- rpart(formula, Training, method = "class", control = rpart.control(cp=0))

#Cross Validation
library(e1071)
library(caret)
train_control <- trainControl(method="cv", number=5)
model <- train(diagnosis~.-diagnosis1, data=Training.rf, trControl=train_control, method="rpart", tuneLength = 20)
plot(model)

#Choosing CP
printcp(tree.1)
plotcp(tree.1)
bestcp <- tree.1$cptable[which.min(tree.1$cptable[,"xerror"]), "CP"]
tree.pruned <- prune(tree.1, cp = bestcp)
tree.pruned
library(partykit)
rparty.tree <- as.party(tree.pruned)
plot(rparty.tree)

#ROC plot
library(ROCR)
tree.prob <- predict(tree.pruned, newdata = Testing, type = "prob")[,2]
tree.pred <- prediction(tree.prob, Testing$diagnosis)
tree.perf <- performance(tree.pred, measure = "tpr", x.measure = "fpr")
plot(tree.perf, main = "ROC Curve for Decision Forest", col = "red")
abline(0,1, col = "gray")
auc <- performance(tree.pred, measure = "auc")
auc <- auc@y.values[[1]]
auc #0.9564

#Prediction Accuracy
tree.pred <- predict(tree.pruned, Testing, type = "class")
tree.acc <- table(tree.pred, Testing$diagnosis)
sum(diag(tree.acc))/sum(tree.acc) #0.9235

#Random Forest for All Variables
library(randomForest)

#Split Train
Train.rf <-createDataPartition(train.1$diagnosis1, p = 0.7, list = FALSE)
Training.rf <- train[Train.rf,]
Testing.rf <- train[-Train.rf,]

#RF
set.seed(2)
rf <- randomForest(diagnosis~.-diagnosis1-id, data = Training.rf, importance = TRUE, ntree = 2000)
print(rf)
varImpPlot(rf)
importance(rf, type = 1)

#Choosing best randomForest
library(e1071)
tuned.r <- tune(randomForest, train.x = diagnosis ~ .-diagnosis1-id,
                data = Training.rf,
                validation.x = Testing.rf)
best.rf <- tuned.r$best.model

#Accuracy
rf.pred <- predict(best.rf, Testing.rf, type = "class") 
rf.acc <- table(rf.pred, Testing.rf$diagnosis)
sum(diag(rf.acc))/sum(rf.acc) #0.9882

#ROC plot
library(ROCR)
rf.prob <- predict(best.rf, newdata = Testing.rf, type = "prob")[,2]
rf.pred <- prediction(rf.prob, Testing.rf$diagnosis)
rf.perf <- performance(rf.pred, measure = "tpr", x.measure = "fpr")
plot(rf.perf, main = "ROC Curve for Random Forest", col = "red")
abline(0,1, col = "gray")
auc <- performance(rf.pred, measure = "auc")
auc <- auc@y.values[[1]]
auc #0.999

#Cross Validation
library(caret)
ctrl <- trainControl(method = "cv", number = 5)
k <- train(diagnosis ~ .-id-diagnosis1, data = Training.rf, method = "rf", trControl = ctrl, tuneLength = 20)
plot(k)

#Random Forest for All Variables Mean
set.seed(3)
Training.rf.1 <- Training.rf[,-c(1, 13:33)]
Testing.rf.1 <- Testing.rf[,-c(1, 13:33)]
rf.1 <- randomForest(diagnosis~., data = Training.rf.1, importance = TRUE, ntree = 2000)
print(rf.1)
varImpPlot(rf.1)
importance(rf.1, type = 1)

#Choosing Best RF
library(e1071)
tuned.r.1 <- tune(randomForest, train.x = diagnosis ~ .,
                data = Training.rf.1,
                validation.x = Testing.rf.1)
best.rf.1 <- tuned.r.1$best.model

rf.1.pred <- predict(best.rf.1, Testing.rf.1, type = "class") 
rf.1.acc <- table(rf.1.pred, Testing.rf.1$diagnosis)
sum(diag(rf.1.acc))/sum(rf.1.acc) #0.9235

#ROC plot
library(ROCR)
rf.1.prob <- predict(best.rf.1, newdata = Testing.rf.1, type = "prob")[,2]
rf.1.pred <- prediction(rf.1.prob, Testing.rf.1$diagnosis)
rf.1.perf <- performance(rf.1.pred, measure = "tpr", x.measure = "fpr")
plot(rf.1.perf, main = "ROC Curve for Random Forest", col = "red")
abline(0,1, col = "gray")
auc <- performance(rf.1.pred, measure = "auc")
auc <- auc@y.values[[1]]
auc #0.9777

#Cross Validation
library(caret)
ctrl <- trainControl(method = "cv", number = 5)
k <- train(diagnosis ~ ., data = Training.rf.1, method = "rf", trControl = ctrl, tuneLength = 20)
plot(k)

#KNN
library(class)
Training.knn <- Training[, -c(7,1)]
Testing.knn <- Testing[, -c(7,1)]
cl <- as.factor(Training[, 1])
pred.knn <- knn(Training.knn, Testing.knn, cl, k = 1)
table(pred.knn, Testing$diagnosis)
sum(pred.knn == Testing[,1])/dim(Testing)[1] #0.8235

#Cross Validation
library(caret)
ctrl <- trainControl(method = "cv", number = 5)
k <- train(diagnosis ~ ., data = Training[,-c(7)], method = "knn", trControl = ctrl, tuneLength = 20)
plot(k)

#SVM
library(e1071)
Training.svm <- Training[,-c(7)]
Testing.svm <- Testing[, -c(7)]
x <- Training.svm[,-c(1)]
y <- Training.svm[,c(1)]
SVM <- svm(diagnosis ~ .,data = Training.svm)
summary(SVM)
print(SVM)
pred.svm <- predict(SVM, Testing.svm)
table(pred.svm, Testing.svm[,1])
sum(pred.svm == Testing.svm[,1])/dim(Testing.svm)[1] #0.9294

#Cross Validation 1
svm_tune <- tune(svm, train.x=x, train.y=y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
print(svm_tune)
SVM.After <- svm(diagnosis ~ .,data = Training.svm, kernel = "radial", cost = 100, gamma = 2)
summary(SVM.After)
pred.svm.After <- predict(SVM.After, Testing.svm)
table(pred.svm.After, Testing.svm[,1])
sum(pred.svm.After == Testing.svm[,1])/dim(Testing.svm)[1] #0.9118

#Cross Validation 2
library(caret)
library(kernlab)
ctrl <- trainControl(method = "cv", number = 5)
k <- train(diagnosis ~ ., data = Training[,-c(7)], method = "svmRadial", trControl = ctrl, tuneLength = 20)
plot(k)
