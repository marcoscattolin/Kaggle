setwd("~/GitHub/Kaggle/PokerRuleInduction/src")
library(dplyr)
library(caret)




# LOAD DATA ---------------------------------------------------------------
train <- read.csv("../input/train.csv")
train <- tbl_df(train)
train <- mutate(train, hand = factor(hand))

suitsix <- seq(1,10,by=2)
rankix <- seq(2,10,by=2)


# CALC MAX MIN ------------------------------------------------------------
train$maxrank <- apply(train[,rankix],1,max)
train$minrank <- apply(train[,rankix],1,min)



# CALC UNIDIMENSIONAL DISTANCES FOR RANKS -------------------------------------------
tmp <- sapply(1:dim(train)[1], function(x) as.vector(dist(t(train[x,rankix]))))
tmp <- tbl_df(as.data.frame(t(tmp)))
colnames(tmp) <- paste0("rank_",colnames(tmp))
train <- cbind(train,tmp)


# CALC UNIDIMENSIONAL DISTANCES FOR SUITS -------------------------------------------
tmp <- sapply(1:dim(train)[1], function(x) as.vector(dist(t(train[x,suitsix]))))
tmp <- tbl_df(as.data.frame(t(tmp)))
colnames(tmp) <- paste0("suit_",colnames(tmp))
train <- cbind(train,tmp)



# TRAIN MODEL -------------------------------------------------------------
grid <- data.frame(mtry = c(18,20))
fitctrl <- trainControl(method="repeatedcv", number=3,repeats=1,verboseIter=T)

set.seed(1234)
rf <- train(data = train[,11:33], hand ~ .,trControl=fitctrl,method="rf",tuneGrid=grid)


# EVAL RESULTS -------------------------------------------------------------
rf
predictions <- predict(rf, newdata=train[,12:33])
confusionMatrix(train$hand, predictions, dnn = c("reference","prediction"))
plot(varImp(rf))
save(rf, file = "./tmp/rf.rda")

