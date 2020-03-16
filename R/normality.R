# Authour: Yuka
# Version: 1.0.0
# Date: 2020-03-11
# TODO: 正态性检测
# Notice：如果样本量大于50，则应该使用Kolmogorov-Smirnov 检验结果，
#         反之则使用Shapiro-Wilk 检验的结果。
#         如果P值大于0.05，则说明具有正态性特质，反之则说明数据没有正态性特质。



test_of_normality <- function(data, colname){
  if(length(data) > 50){
    result <- ks.test(jitter(data[colname]), "pnorm")
    result <- result$p.value
  }
  else{
    result <- shapiro.test(data[colname])
    result <- result$p.value
  }
  return(result)
}


# test_of_normality(Orange, "circumference")
