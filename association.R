library(conflicted)  
library(tidyverse)
library("arules")
library("arulesViz")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflicts_prefer(arules::setdiff)
library(arulesCBA)

end_table_path <- "D:\\Studium\\data mining\\Projekt\\tabelle_saison_csv"

setwd(end_table_path)

end_table <- list.files(
  path = end_table_path, pattern = "*.csv") %>% map_df(~read_csv(.))

df <- end_table %>%
                      select(-club_rep,-club_name,-season,-games_played,-points)

## Discretize "MonthlyCharges" with respect to "Churn"/"No Churn" label and assign to new column in dataframe
# df$cat <- discretizeDF.supervised(rank ~ ., df[, c('wins', 'rank')], method='mdlp')$wins
print(colnames(df))
df$wins <- discretize(df$wins, method="interval",breaks=18, labels = NULL,    
                           ordered=TRUE, onlycuts=FALSE)

df$ties <- discretize(df$ties, method="interval",breaks=18, labels = NULL,    
                           ordered=TRUE, onlycuts=FALSE) 

df$losses <- discretize(df$losses, method="interval",breaks=18, labels = NULL,    
                           ordered=TRUE, onlycuts=FALSE)

df$goals_scored <- discretize(df$goals_scored, method="interval",breaks=18, labels = NULL,    
                           ordered=TRUE, onlycuts=FALSE)

df$goals_scored_against <- discretize(df$goals_scored_against, method="interval",breaks=18, labels = NULL,    
                           ordered=TRUE, onlycuts=FALSE)  

 df$goal_difference <- discretize(df$goal_difference, method="interval",breaks=18, labels = NULL,    
                           ordered=TRUE, onlycuts=FALSE)                                                     

#print(unique(df$wins))
# Check levels of binned variable
#view(df)

df[sapply(df, is.numeric)] <- lapply(df[sapply(df, is.numeric)], as.factor)


trans <- as(df, "transactions")

itemFrequencyPlot(trans, topN=10,  cex.names=1)

# #sup=0.01 conf=0.03 -> 3 rules
rank_1_rules <- apriori(data=df, 
                        parameter=list (supp=0.01,conf = 0.03), 
                        appearance = list (rhs="rank=1"),
                        minlen=2,
                        maxlen=7)


inspect(rank_1_rules)

#korrelation matrix