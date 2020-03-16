# Authour: Yuka
# Version: 1.0.0
# Date: 2020-03-13
# TODO: 方差分析


# 不同颜色的饮料的销量数据(样本容量相同,结果显著)
data=rbind(
  data.frame(color="nocolor" ,sale=c(26.5,28.7,25.1,29.1,27.2)),
  data.frame(color="pink" ,sale=c(31.2,28.3,30.8,27.9,29.6)),
  data.frame(color="orange" ,sale=c(27.9,25.1,28.5,24.2,26.5)),
  data.frame(color="green" ,sale=c(30.8,29.6,32.4,31.7,32.8))
)


anova <- function(data, y_colname, x_colname){
  formula <- as.formula(paste(y_colname, '~', x_colname))
  fit <- aov(formula, data = data)
  result <- summary(fit)[[1]][-2, ]
  colnames(result) <- c("df", "sum_sq", "mean_sq", "f", "p")
  return(result)
}

# result <- anova(data, 'sale', 'color')

