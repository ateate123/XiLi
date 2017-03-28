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

#Correlation, choosing top 5 correlated factors
cor <- cor(train[,-c(1,2,13:32)],train[,-c(1,2,13:32)])
corrplot(cor, method = "square")
cor1 <- cor(train[,33],train[,3:12])
corrplot(cor(train[,33],train[,3:12]), method = "number")
train.1 <- train[,c(3,5,6,9,10,33)]
head(train.1)

#Split Data
Train <-createDataPartition(train.1$diagnosis1, p = 0.7, list = FALSE)
Training <- train.1[Train,]
Testing <- train.1[-Train,]

#Logit Regression
logit <- train(diagnosis1 ~ ., data = Training, methon = "glm", family = "binomial")
logit <- glm(diagnosis1 ~ ., data = Training, family = "binomial")

#ROC plot
library(ROCR)
prob <- predict(logit, newdata = Testing)
pred <- prediction(prob, Testing$diagnosis1)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
abline(0,1, col = "red")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

#Cross Validation
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
cv.logit <- cv.glm(Training, logit, K=5, cost = cost)
1-cv.logit$delta[1]

#Accuracy on Test data
pred <- predict(logit, newdata = Testing) >= 0.5
accuracy <- table(pred, Testing[,6])
sum(diag(accuracy))/sum(accuracy)



