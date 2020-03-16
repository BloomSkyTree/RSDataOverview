# Authour: Yuka
# Version: 1.0.0
# Date: 2020-03-11
# TODO: 描述性分析

pearson.cortest <- function(data,
                            colname1,
                            colname2,
                            method = "PEARSON"){
  if(method == "PEARSON"){
    result <- cor.test(data[[colname1]], data[[colname2]])
  }
  else if(method == "SPEARMAN"){
    result <- cor.test(data[[colname1]], data[[colname2]], method = "spearman", exact = FALSE)
  }
  estimate <- result$estimate[['cor']]
  p_value <- result$p.value
  library(jsonlite)
  return(toJSON(as.data.frame(t(rbind(estimate, p_value)))))
}



