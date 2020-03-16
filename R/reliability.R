# Authour: Yuka
# Version: 1.0.0
# Date: 2020-03-13
# TODO: 信度分析


reliability <- function(data, colnames){
  data <- data[colnames]
  colnames(data) = NULL
  var.num <- length(data)
  dim <- length(data[[1]])

  sum.vector <- c(1 : dim)

  for(i in 1 : dim){
    sum.vector[i] <- sum(data[i, ])
  }

  d.vector <- c(1 : var.num)
  for(i in (1 : var.num)){
    d.vector[i] <- var(data[[i]])
  }

  result <-  var.num /(var.num - 1) * (1 - sum(d.vector[1 : var.num]) / var(sum.vector))
  result
}


# data <- women
# reliability(data, c("height", "weight"))
