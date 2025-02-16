rm(list=ls())  
setwd('./')
expr=read.delim('root.txt',header=T,row.names = 1,sep = "\t",check.names=FALSE) #读取表达矩阵
#计算r和p值，x 必须是矩阵。如果 x 是 DataFrame 可以用 as.matrix 转换。 
expr <- expr[,-1]
n<-as.matrix(expr)
library(Hmisc)
#rcorr(x, type="pearson")
p=rcorr(n, type="pearson")
#p1=rcorr(n, type="spearman")
write.table(p[["r"]],file = "pearson_r_root_traits.txt",sep = "\t")
write.table(p[["P"]],file = "pearson_Pvalue_root_traits.txt",sep = "\t")


library(pheatmap)
library(RColorBrewer)
# 设置配色
cc = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))
#colorRampPalette生成渐变色

pdf("root_prarson_heatmap.pdf", height = 5, width = 6)
pheatmap(p[["r"]],main = "root traits",
           na_col = "grey",color=cc(200),
           cluster_rows =F,cluster_cols = F,border_color = "white",
           treeheight_col = 15,treeheight_row = 15,
           show_colnames=T,show_rownames = T,
           #         cellwidth = 20, cellheight = 20,
           display_numbers=F,legend=T,angle = 315) 
dev.off()
png("root_prarson_heatmap.png", height = 500, width = 600)
pheatmap(p[["r"]],main = "root traits",
         na_col = "grey",color=cc(200),
         cluster_rows =F,cluster_cols = F,border_color = "white",
         treeheight_col = 15,treeheight_row = 15,
         show_colnames=T,show_rownames = T,
         #         cellwidth = 20, cellheight = 20,
         display_numbers=F,legend=T,angle = 315) 
dev.off()


#####weight
rm(list=ls())  

expr=read.delim('Weight.txt',header=T,sep = "\t",row.names = 1,check.names=FALSE) 
#计算r和p值，x 必须是矩阵。如果 x 是 DataFrame 可以用 as.matrix 转换。 
expr <- expr[,-1]
n<-as.matrix(expr)
library(Hmisc)
#rcorr(x, type="pearson")
p=rcorr(n, type="pearson")
#p1=rcorr(n, type="spearman")
write.table(p[["r"]],file = "pearson_r_weight_traits.txt",sep = "\t")
write.table(p[["P"]],file = "pearson_Pvalue_weight_traits.txt",sep = "\t")


library(pheatmap)
library(RColorBrewer)
# 设置配色
cc = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))
#colorRampPalette生成渐变色

pdf("weight_prarson_heatmap.pdf", height = 5, width = 6)
pheatmap(p[["r"]],main = "weight traits",
         na_col = "grey",color=cc(200),
         cluster_rows =F,cluster_cols = F,border_color = "white",
         treeheight_col = 15,treeheight_row = 15,
         show_colnames=T,show_rownames = T,
         #         cellwidth = 20, cellheight = 20,
         display_numbers=F,legend=T,angle = 315) 
dev.off()
png("weight_prarson_heatmap.png", height = 500, width = 600)
pheatmap(p[["r"]],main = "weight traits",
         na_col = "grey",color=cc(200),
         cluster_rows =F,cluster_cols = F,border_color = "white",
         treeheight_col = 15,treeheight_row = 15,
         show_colnames=T,show_rownames = T,
         #         cellwidth = 20, cellheight = 20,
         display_numbers=F,legend=T,angle = 315) 
dev.off()


#####slice
rm(list=ls())  

expr=read.delim('slice.txt',header=T,sep = "\t",row.names = 1,check.names=FALSE,stringsAsFactors = F) 
#计算r和p值，x 必须是矩阵。如果 x 是 DataFrame 可以用 as.matrix 转换。 
expr <- expr[,-1]
## 删掉标准差为0的行
#expr = expr[apply(expr, 1, function(x) sd(x)!=0),] 

# #转换成数值型
expr <- as.data.frame(lapply(expr, as.numeric))
n<-as.matrix(expr)
library(Hmisc)
#rcorr(x, type="pearson")
p=rcorr(n, type="pearson")
#p1=rcorr(n, type="spearman")
write.table(p[["r"]],file = "pearson_r_slice_traits.txt",sep = "\t")
write.table(p[["P"]],file = "pearson_Pvalue_slice_traits.txt",sep = "\t")


library(pheatmap)
library(RColorBrewer)
# 设置配色
cc = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))
#colorRampPalette生成渐变色

pdf("slice_prarson_heatmap.pdf", height = 10, width = 12)
pheatmap(p[["r"]],main = "weight traits",
         na_col = "grey",color=cc(200),
         cluster_rows =F,cluster_cols = F,border_color = "white",
         treeheight_col = 15,treeheight_row = 15,
         show_colnames=T,show_rownames = T,
         #         cellwidth = 20, cellheight = 20,
         display_numbers=F,legend=T,angle = 315) 
dev.off()
png("slice_prarson_heatmap.png", height = 1000, width = 1200)
pheatmap(p[["r"]],main = "weight traits",
         na_col = "grey",color=cc(200),
         cluster_rows =F,cluster_cols = F,border_color = "white",
         treeheight_col = 15,treeheight_row = 15,
         show_colnames=T,show_rownames = T,
         #         cellwidth = 20, cellheight = 20,
         display_numbers=F,legend=T,angle = 315) 
dev.off()
