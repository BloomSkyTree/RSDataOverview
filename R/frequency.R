# Authour: Yuka
# Version: 1.0.0
# Date: 2020-03-11
# TODO: 频度分析


frequency.analysis <- function(data){
  frequency <- table(data)
  element <- rownames(frequency)
  probability <- prop.table(frequency)
  FPTable <- rbind(element, frequency, probability)
  FPTable <- as.data.frame(t(FPTable))

  library(jsonlite)
  return(toJSON(FPTable))
}

# #读取数据
# data.path <- "D:\\Yuka\\Company\\R\\test data\\test.csv"
# colname <- "point"
# data <- read.csv(data.path, header = T)
# print(frequency.analysis(data[['point']]))


