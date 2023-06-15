library(conflicted)  
library(tidyverse)
library("arules")
library("arulesViz")
library(rpart)
library(rpart.plot)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

end_table_path <- "D:\\Studium\\data mining\\Projekt"

setwd(end_table_path)

df <- list.files(
  path = end_table_path, pattern = "stats2.csv") %>% map_df(~read_csv(.))

#remove duplicates
df <-  df[!duplicated(df), ]

#remove irrelevant columns
df <- df %>% select(-club,-season,-side,-matchday,-erfolgreiche_saison)

# view(df)

#set seed and create row id for splitting into train and test dataset
set.seed(1)
df$id <- 1:nrow(df)

#use 80% of dataset as training set and 20% as test set 
train <- df %>% dplyr::sample_frac(0.80)
test  <- dplyr::anti_join(df, train, by = 'id')

#train model based on train dataset 
#response variable is end_rank
fit <- rpart(end_rank~., data = train, method = 'anova')

#visualization
rpart.plot(fit, extra = 101)

#use trained model to predict for test dataset
predict_unseen <-predict(fit, test, type = 'vector')

#format the result of prediction
table_mat <- table(test$end_rank, predict_unseen)

 view(table_mat)
