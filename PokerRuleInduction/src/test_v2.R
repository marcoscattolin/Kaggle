setwd("~/GitHub/Kaggle/PokerRuleInduction/src")
library(dplyr)
library(caret)


# LOAD DATA ---------------------------------------------------------------
test <- read.csv("../input/test.csv")
test <- tbl_df(test)

suitsix <- seq(2,11,by=2)
rankix <- seq(3,11,by=2)


# CALC MAX MIN ------------------------------------------------------------
test$maxrank <- apply(test[,rankix],1,max)
test$minrank <- apply(test[,rankix],1,min)



# CALC UNIDIMENSIONAL DISTANCES FOR RANKS -------------------------------------------
tmp <- sapply(1:dim(test)[1], function(x) as.vector(dist(t(test[x,rankix]))))
tmp <- tbl_df(as.data.frame(t(tmp)))
colnames(tmp) <- paste0("rank_",colnames(tmp))
test <- cbind(test,tmp)


# CALC UNIDIMENSIONAL DISTANCES FOR SUITS -------------------------------------------
tmp <- sapply(1:dim(test)[1], function(x) as.vector(dist(t(test[x,suitsix]))))
tmp <- tbl_df(as.data.frame(t(tmp)))
colnames(tmp) <- paste0("suit_",colnames(tmp))
test <- cbind(test,tmp)
save(test,file="./tmp/test_features.rda")
#load("./tmp/test_features.rda")


# EXTRACT RESULTS -------------------------------------------------------------
load("./tmp/rf.rda")
predictions <- predict(rf, newdata=test[,12:33])



# WRITE OUTPUT -----------------------------------------------------------------
out <- data.frame(id = test$id, hand = predictions)
write.table(out, "submission_scattolin.csv", quote=F, sep=",", col.names=T,row.names=F)

