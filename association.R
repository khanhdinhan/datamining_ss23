library(conflicted)
library(tidyverse)
library("arules")
library("arulesViz")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflicts_prefer(arules::setdiff)
library(arulesCBA)
library(data.table)
end_table_path <- "D:\\Studium\\data mining\\Projekt"
library(ggplot2)
setwd(end_table_path)

df <- list.files(
  path = end_table_path, pattern = "stats2.csv"
) %>% map_df(~ read_csv(.))

# remove duplicates
df <- df[!duplicated(df), ]

# remove irrelevant columns
df <- df %>% select(-club, -season, -side, -matchday, -erfolgreiche_saison)

for (name in colnames(df)) {
  df[[name]] <- discretize(df[[name]], method = "interval", breaks = 6, labels = NULL, ordered = TRUE, onlycuts = FALSE)
}

df[sapply(df, is.numeric)] <- lapply(df[sapply(df, is.numeric)], as.factor)

trans <- as(df, "transactions")

# itemFrequencyPlot(trans, topN=10,  cex.names=1)

support <- 0.07
confident <- 0.09

for (name in col_names) {
  uniq_val <- unique(df$end_rank)
  for (val in uniq_val) {
    s <- paste("end_rank=", val, sep = "")
    rules <- apriori(
      data = df,
      parameter = list(supp = support, conf = confident),
      appearance = list(rhs = s),
      minlen = 2,
      maxlen = 10
    )

    c <- paste("Top rules of ", s, sep = "")
    # inspect(rank_1_rules)
    plot(rules, method = "paracoord", main = c)
  }
}
