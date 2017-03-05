train <- read.csv("train.csv", stringsAsFactors = TRUE)
summary(train)
str(train)
dim(train)
head(train)
colnames(train)
length(unique(train$Pclass))

#Model
sapply(train, function(x) {length(which(is.na(x)))})
keep.row <- sapply(train$Age, function(x) {!is.na(x)}) 
train1 <- train[keep.row, c(-1,-4, -9, -11)] #Delete Id, Name, Ticket, Cabin
sapply(train1, function(x) {length(which(is.na(x)))})
model.1 <- lm(Survived ~ .-Survived, data = train1)
summary(model.1)

#CV
library(glmnet)
ind <- model.matrix(~., train1[,c(-1)])
dep <- train1$Survived
fit <- glmnet(x = ind, y = dep)
plot(fit)
plot(fit, label = T)
cvfit <- cv.glmnet(ind, dep)
plot(cvfit)
cvfit$lambda.1se
coef(cvfit, s = "lambda.1se")

#Cross-validation, alpha select
alpha.choose <- NULL
for (i in c(1:100)){
  cv <- cv.glmnet(ind,dep, alpha = i/100)
  alpha.choose <- c(alpha.choose, cv$lambda.1se)
  if (cv$lambda.1se == min(alpha.choose)){
    i <- i
    }
}
model.2 <- cv.glmnet(ind,dep, alpha = i/100)
model.2$lambda.1se
coef(model.2, s = "lambda.1se")

#test accuracy on training
pred <- predict.cv.glmnet(model.2, ind, s=c("lambda.1se"))
pred.s <- sapply(pred, function(x) {x >= 0.5}) 
sum(pred.s == train1$Survived)/dim(train1)[1] #=0.7787115

#Test accuracy on test data
test <- read.csv("test.csv", stringsAsFactors = TRUE)
sub <- read.csv("gender_submission.csv", stringsAsFactors = TRUE)
keep.row.test <- sapply(test$Age, function(x) {!is.na(x)}) 
test1 <- test[keep.row.test, c(-1, -3, -8, -10)]
keep.row.test.1 <- sapply(test1$Fare, function(x) {!is.na(x)})
test1 <- test1[keep.row.test.1, ]
sub1 <- sub[keep.row.test, ]
sub1 <- sub1[keep.row.test.1, c(-1)] 
test1 <- cbind(Survived = sub1, test1)
ind.test <- model.matrix(~.,test1[,c(-1)]) #Problem with dummy Embarked

EmbarkedC <- test1$Embarked == "C"
ind.test <- cbind(ind.test, EmbarkedC)

pred.test <- predict.cv.glmnet(model.2, ind.test, s=c("lambda.1se"))
pred.s.test <- sapply(pred.test, function(x) {x >= 0.5}) 
sum(pred.s.test == test1$Survived)/dim(test1)[1] #=1 means predictions are all right??





