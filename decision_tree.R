library(conflicted)  
library(tidyverse)
library("arules")
library("arulesViz")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

end_table_path <- "D:\\Studium\\data mining\\Projekt\\tabelle_saison_csv"

setwd(end_table_path)

end_table <- list.files(
  path = end_table_path, pattern = "*.csv") %>% map_df(~read_csv(.))

end_table_filtered <- end_table %>%
                      select(-club_rep,-club_name,-season,-games_played,-points)

#view(end_table_filtered)

set.seed(1)

end_table_filtered$id <- 1:nrow(end_table_filtered)

#use 70% of dataset as training set and 30% as test set 
train <- end_table_filtered %>% dplyr::sample_frac(0.80)
test  <- dplyr::anti_join(end_table_filtered, train, by = 'id')

# view(train)
# view(test)

library(rpart)
library(rpart.plot)
fit <- rpart(rank~., data = train, method = 'anova')
rpart.plot(fit, extra = 101)

predict_unseen <-predict(fit, test, type = 'vector')

table_mat <- table(test$rank, predict_unseen)
view(table_mat)