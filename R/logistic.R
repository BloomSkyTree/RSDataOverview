# Authour: Yuka
# Version: 1.0.0
# Date: 2020-03-12
# TODO: （二元）逻辑回归分析







binary_logistic <- function(data, y_colname, x_colname_list){
  y <- paste(y_colname, "~")
  x <- paste(x_colname_list, collapse = "+")
  formula <- as.formula(paste(y, x))
  model <- glm(formula, family=binomial(link='logit'), data = data)
  summary <- summary(model)
  result <- as.data.frame(summary[["coefficients"]])
  rownames(result)[1] <- "intercept"
  colnames(result) <- c("estimate", "std_error", "z", "p")
  library(epiDisplay)
  or.list <- as.data.frame(epiDisplay::logistic.display(model, crude = FALSE, simplified = TRUE)$table)
  or.list <- rbind(c(NA, NA, NA, NA), or.list)
  result$or <- or.list$OR
  result$lower95ci <- or.list$lower95ci
  result$upper95ci <- or.list$upper95ci
  return (result)
}

# data <- cats
# data[which(data$Sex == "F"), "sex"] = 0
# data[which(data$Sex == "M"), "sex"] = 1
# x.colname.list <- list('Bwt', 'Hwt')
# result <- binary_logistic(data, 'sex', x.colname.list)


