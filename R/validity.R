data <- read.csv("D:\\Yuka\\Company\\R\\测试数据.csv")





validity <- function(data, n_factors){
  library(psych)
  correlations <- cor(data) # 计算变量相关系数矩阵
  suggest <- fa.parallel(correlations,n.obs = 18, fa = "both",
                         n.iter = 100, main = "平行分析碎石图")$nfact

  result <- factanal(data, n_factors)
  # 提取4个因子，不旋转
  return(result)

}

# n.factors <- 4
# result <- validity(data, n.factors)
# loadings <- result[['loadings']]
# factors.names <- c(1:n.factors)
# for(i in 1 : n.factors){
#   factors.names[i] <- paste("factor", toString(i))
# }
# m <- matrix(loadings[1:48], ncol(data), n.factors, dimnames = list(colnames(data), factors.names))
# loadings.data.frame <- as.data.frame(m)
