#安装包
#install.packages("CMplot")
library("CMplot")
#示例数据。要绘制 SNP 密度图，仅仅需要三列即可：
#a. 第一列是 SNP 名称
#b. 第二列是染色体
#c. 第三列是 SNP 的位置
#d. 第四列开始为不同性状的P值
#setwd("./")
#读入数据
#data <- read.table("root.sign.txt",header=T)
# users can personally set the windowsize and the max of legend by:
# bin.size=1e6
# bin.max=N
# memo: add a character to the output file name.

# CMplot(
#   data, plot.type="d",  bin.size=1e6, col=c("darkgreen", "yellow", "red"),
#   file="jpg", memo="Fig1", dpi=1200, file.output=TRUE, verbose=TRUE
# )
data1 <- read.table("root.sign.txt",header=T)
data2 <- read.table("weight.sign.txt",header=T)
data3 <- read.table("slice.sign.txt",header=T)


CMplot(
  data1, plot.type="d",  bin.size=1e6, col=c("darkgreen", "yellow", "red"),
  file="pdf", memo="root.SNP", dpi=600, file.output=TRUE, verbose=TRUE
)

CMplot(
  data2, plot.type="d",  bin.size=1e6, col=c("darkgreen", "yellow", "red"),
  file="pdf", memo="weight.SNP", dpi=600, file.output=TRUE, verbose=TRUE
)
CMplot(
  data3, plot.type="d",  bin.size=1e6, col=c("darkgreen", "yellow", "red"),
  file="pdf", memo="slice.SNP", dpi=600, file.output=TRUE, verbose=TRUE
)
