library(tidyverse)
library(corrplot)
library(Hmisc)

end_table_path <- "D:\\Studium\\data mining\\Projekt"
setwd(end_table_path)

df <- list.files(
  path = end_table_path, pattern = "stats2.csv"
) %>% map_df(~ read_csv(.))

# remove duplicates
df <- df[!duplicated(df), ]

# remove irrelevant columns
df <- df %>% select(-club, -season, -side, -matchday, -erfolgreiche_saison)

x <- df %>% select(-end_rank)
res <- rcorr(as.matrix(df),type="spearman") # rcorr() accepts matrices only


# display p-values (rounded to 3 decimals)
print(round(res$P, 3))

#                         end_rank
# scored                    0.000
# attempts                  0.000
# distance                  0.524
# passes                    0.000
# pass_success_rate         0.000
# crosses                   0.076
# crosses_success_rate      0.009
# dribblings                0.000
# dribbling_success_rate    0.000
# possesion                 0.000
# duels_won                 0.000
# duels_won_rate            0.000
# duels_air_won             0.000
# duels_air_won_rate        0.000
# fouls                     0.000
# were_fouled               0.005
# offside                   0.000
# corners                   0.000
# scored_against            0.000

corr<- cor(df, method = c("spearman"))
round(corr,2)
corrplot(corr, method="pie")

