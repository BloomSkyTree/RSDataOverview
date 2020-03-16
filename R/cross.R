# Authour: Yuka
# Version: 1.0.0
# Date: 2020-03-13
# TODO: 交叉分析

cross_analysis <- function(data, handled = FALSE, method = "chisq"){
  if(!handled){
    data <- table(data)
  }
  if(method == "chisq"){
    chisq.result <- chisq.test(data)
    x2 <- chisq.result[['statistic']]
    names(x2) <- NULL
    result <- as.data.frame(t(c("x2" = x2, "p" = chisq.result[['p.value']])))
    return(result)
  }
  else if(method == "fisher"){
    fisher.result <- fisher.test(data)
    x2 <- fisher.result[['statistic']]
    names(x2) <- NULL
    result <- as.data.frame(t(c("p" = chisq.result[['p.value']])))
    return(result)
  }
}




#cross_analysis(data[, -1], method = "fisher")

# 以R自带的数据集Orange为例
# 数据处理，将不同年龄、周长的橘树数据分别按照其数值进行分类
# data <- Orange
#
# classify.circumference <- function(element){
#   if(element < 50) element <- 'small'
#   else if(element < 100) element <- 'medium'
#   else element <- 'big'
# }
#
# #tapply(data['circumference'], data['age'], FUN = map.function)
# circumference <-data['circumference']
# result <- sapply(circumference, classify.circumference)
# data['circumference'] <- result
#
#
# classify.age <- function(element){
#   if(element < 300) element <- 'young'
#   else if (element < 800) element <- 'middle'
#   else element <- 'old'
# }
#
# age <- data['age']
# result <- sapply(age, classify.age)
# data['age'] <- result
#
# # 分类完成，去除原数据中不必要的列，然后进行频度统计
# # 得到树龄-周长的二元表，形如下：
# #         small medium  big
# # young   N11   N12     N13
# # middle  N21   N22     N23
# # old     N31   N32     N33
# result <- table(data[, -1])
#
# # 卡方检测
#
# chisq.result <- chisq.test(result)
# print(paste('两个变量间有关的可信度为：' , (1 - chisq.result[['p.value']]) * 100, '%'))
# # 费舍检测
# fisher.result <- fisher.test(result)
# print(paste('两个变量间有关的可信度为：' , (1 - fisher.result[['p.value']]) * 100, '%'))
