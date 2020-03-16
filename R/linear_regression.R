# Authour: Yuka
# Version: 1.0.0
# Date: 2020-03-12
# TODO: 线性回归


linear_regression <- function(data, colname1, colname2){
  fit <- lm(data[colname1] ~ data[colname2], data)
  linear.model <- summary(fit)
  coefficients <- as.data.frame(linear.model['coefficients'][[1]])
  rownames(coefficients) <- c("intercept", "x")
  colnames(coefficients) <- c("estimate", "std_error", "t_value", "p_value")
  coefficients

}

# result <- linear_regression(Orange, 'circumference', 'age')
