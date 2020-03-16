# Authour: Yuka
# Version: 1.0.0
# Date: 2020-03-13
# TODO: 描述性分析

to_describe <- function(data){
  result <- psych::describe(data)
  rownames(result) <- colnames(data)
  return(result)
}

# to_describe(Orange)
