setwd("~/GitHub/Kaggle/PokerRuleInduction/src")
library(dplyr)

# READ CURRENT OUTCOME ----------------------------------------------------
current <- read.csv("../out/submission_scattolin.csv")
current <- tbl_df(current)


# READ BEST SUBMISSION ----------------------------------------------------
best <- read.csv("../submitted_20141208/submission_scattolin.csv")
best <- tbl_df(best)



# COMPARE -----------------------------------------------------------------
table(current$hand,best$hand)
current$besthand <- best$hand
filter(current, hand != besthand)
