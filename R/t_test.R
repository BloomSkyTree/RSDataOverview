# Authour: Yuka
# Version: 1.0.0
# Date: 2020-03-12
# TODO: T样本检测


single_t_test <- function(data, true_value){
  result <- t.test(x, mu = true_value)
  p <- result[[3]]
  return(p)
}

pair_t_test <- function(data, colname1, colname2){
  result <- t.test(data[[colname1]], data[[colname2]], paired = T)
  p <- result[[3]]
  return(p)
}

individual_t_test <- function(data, colname1, colname2){
  return(t.test(data[[colname1]], data[[colname2]]))
}

# x <- c(20.99, 20.41, 20.10, 20.00, 20.91, 22.60, 20.99, 20.42, 20.90, 22.99, 23.12, 20.89)
# result <- single_t_test(x, 20.7)
#
# data <- as.data.frame(cbind(Orange$circumference, jitter(Orange$circumference)))
# result <- pair_t_test(data, "V1", "V2")
#
# library(dplyr)
# data.group.1 <- Orange %>% filter(Tree == 1)
# data.group.2 <- Orange %>% filter(Tree == 2)
# data <- as.data.frame(cbind(data.group.1$circumference, data.group.2$circumference))
# individual_t_test(data, "V1", "V2")

