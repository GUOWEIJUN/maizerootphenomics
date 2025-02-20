local({
  r <- getOption("repos")
  r["CRAN"] <- "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"
  options(repos = r)
  Bioc <- getOption("Bioc_mirror")
  Bioc["Bioc_mirror"] <- "https://mirrors.ustc.edu.cn/bioc/"
  options(Bioc_mirror=Bioc)
})

biocPackages <- c(
  "stringi",  # handl character
  "GEOquery", # get GEO dataset
  "limma",    # differentiation analysis
  "ggfortify","ggplot2","pheatmap","ggstatsplot","VennDiagram",  # visulazation
  "clusterProfiler","Org.Hs.eg.db",  "enrichplot",                            # gene annotation
  "devtools"                                        # for github installation
)

## install packages
source("https://bioconductor.org/biocLite.R")
lapply(biocPackages,
       function(biocPackage){
         if(!require(biocPackage, character.only = T)){
           CRANpackages <- BiocManager::available()
           if(biocPackage %in% rownames(CRANpackages)){
             install.packages(biocPackage)
           }else{
             BiocManager::install(biocPackage,ask = F,update = F)
           }
         }
       }
)

# 载入R包
if (T) {
  rm(list = ls())
  library(edgeR)
  library(DESeq2)
  library(FactoMineR)
  library(factoextra)
  library(clusterProfiler)
  library(org.Hs.eg.db)
  library(org.Mm.eg.db)
  library(stringr)
  library(stringi)
  library(tidyverse)
  library(ggplot2)
  library(patchwork)
  library(pheatmap)
  library(VennDiagram)
  library(RColorBrewer)
  library(patchwork)
  library(ggplotify)
  library(reshape2)
  library(ggpubr)
  library(corrplot)
}
rm(list = ls())
options(stringsAsFactors = F)

gset <- read.table("Alltissues.csv", sep = ",", header = T)

#相关性分析
corr <- cor(gset[,2:37], method = 'spearman')
corrplot(corr, type = 'upper', tl.col = 'black', order = 'hclust', tl.srt = 45, addCoef.col = 'white')	
corrplot(corr,order = "AOE",addCoef.col = "grey",
         col.lim = c(-1,1))
write.table(corr,file = "corr.csv",sep =",", 
            row.names =TRUE, col.names =TRUE, quote =TRUE)

gene.datExpr <- log2(gset[,2:37]+1)
gene.datExpr$ID <-  str_split_fixed(gset$gene,"\\.",2)[,1]

rownames(gene.datExpr)<-gene.datExpr[,37]
gene.datExpr$median <- apply(gene.datExpr[,1:36],1,median)
gene.datExpr <- gene.datExpr[gene.datExpr$median>1,]
gene.datExpr <- gene.datExpr[order(gene.datExpr$ID,gene.datExpr$median,decreasing = T),]# 按照基因名、中位数大小排序
gene.datExpr=gene.datExpr[!duplicated(gene.datExpr$ID),]# 只保留相同symbol中中位数最大的探针

row.names
class(gene.datExpr1)
data <- melt(gene.datExpr[,1:36])
group_list1 <- c("B73_1","B73_2","B73_3","zmmax1b_1","zmmax1b_2","zmmax1b_3")
table(group_list1)
exprSet <- gene.datExpr[,1:36]

p1<- ggplot(data,aes(x=variable,y=value,fill=variable))+
  # stat_boxplot(geom = "errorbar",    # 添加误差线
  #  width=0.3)+
  geom_boxplot(alpha = 1,              # 透明度
               outlier.color = "black" # 外点颜色
  )+
  theme_bw()+                          # 白色主题
  theme(
    axis.text.x = element_text(angle = 90,
                               vjust = 0.5
    )       # x轴刻度改为倾斜90度，防止名称重叠
  )+
  geom_signif(                         # 添加显著性标签
    comparisons=list(c("CK1","HS1"),c("CK2","HS2"),c("CK3","HS3")), # 选择你想在哪组上添加标签
    step_increase = 0.1,
    test="t.test",                     # "t 检验，比较两组（参数）" = "t.test","Wilcoxon 符号秩检验，比较两组（非参数）" = "wilcox.test"
    map_signif_level=F                 # 标签样式F为数字，T为*号
  )
## 02绘制PCA图
group=rep(c("B73","zmmax1b"),each=3)
group_list1=factor(group,levels = c("B73","zmmax1b"))
dat <- gene.datExpr[,1:6]
dat <- as.data.frame(t(dat))
dat <- na.omit(dat)
dat$group_list <- group_list1
dat_pca <- PCA(dat[,-ncol(dat)], graph = FALSE)#画图仅需数值型数据，去掉最后一列的分组信息
p2 <- fviz_pca_ind(dat_pca,
                   geom.ind = "point", # 只显示点，不显示文字
                   col.ind = dat$group_list, # 用不同颜色表示分组
                   palette = c("#00AFBB", "#E7B800","#E16A86","green"),
                   addEllipses = T, # 是否圈起来，少于4个样圈不起来
                   legend.title = "Groups") + theme_bw()
p2
###

if(T){
  ###  limma do DEG
  library("limma")
  group_list <- group_list1[c(4:6,1:3)]
  design <- model.matrix(~0 + factor(group_list))
  colnames(design) <- levels(factor(group_list))
  rownames(design) <- colnames(gene.datExpr)[c(4:6,1:3)]
  contrast.matrix <- makeContrasts(zmmax1b-B73,levels=design)
  contrast.matrix
  colnames(design) <- c("zmmax1b", "B73")
  # start the DE analysis
  fit <- lmFit(gene.datExpr[,c(4:6,1:3)],design)
  fit2 <- contrasts.fit(fit,contrast.matrix)
  fit2 <- eBayes(fit2)
  nrDEG <- topTable(fit2,coef=1,n=Inf)
  write.table(nrDEG,file = "zmmax1b-DEG.csv",sep =",", 
              row.names =TRUE, col.names =TRUE, quote =TRUE)
  head(nrDEG)
}


### volcano
if(T){
  library(ggplot2)
  logFC_cutoff <- with(nrDEG,mean(abs(logFC))+0.2*sd(abs(logFC)))
  logFC_cutoff <- 1
  #logFC_cutoff=1.5
  nrDEG$change <- as.factor(ifelse(nrDEG$P.Value<0.05 & abs(nrDEG$logFC) > logFC_cutoff,
                                   ifelse(nrDEG$logFC > logFC_cutoff,"up","down"),"not"))
  
  write.table(nrDEG,file = "zmmax1b-2-DEG.csv",sep =",", 
              row.names =TRUE, col.names =TRUE, quote =TRUE)
  this_title <- paste0('Cutoff for logFC is ', round(logFC_cutoff,1),
                       "\n number of UP gene is ", nrow(nrDEG[nrDEG$change=="up",]),
                       "\n number of DOWN gene is ", nrow(nrDEG[nrDEG$change=="down",]))
  ggplot(data = nrDEG,aes(x=logFC,y=-log10(P.Value),color=change))+
    geom_point(alpha=0.4,size=1.75)+
    theme_set(theme_bw(base_size = 15))+
    xlab("log2 fold change")+ylab("-log10 p-value")+
    ggtitle(this_title)+
    theme(plot.title = element_text(size=15,hjust = 0.5))+
    scale_colour_manual(values = c("blue","black","red"))
  ggsave('volcano.pdf');dev.off()
}

## heatmap
if(T){
  library("pheatmap")
  annotation_col <- data.frame(SampleType=group_list)
  rownames(annotation_col) = colnames(gene.datExpr[,1:6])
  #choose_gene <- c(rownames(nrDEG[order(nrDEG$logFC),])[1:2000],row.names(nrDEG[order(-nrDEG$logFC),])[1:2000])
  #choose_matrix <- gene.datExpr[choose_gene,]
  
  choose_matrix <- gene.datExpr[row.names(nrDEG[nrDEG$change %in% c("up","down"),]),]
  n=t(scale(t(choose_matrix)))
  #n=t(scale(t(gene.datExpr))) # all the dataset
  n[n>2]=2 
  n[n< -2]= -2
  n[1:4,1:4]
  color=colorRampPalette(c("green","black","red"))(1000)
  pheatmap(n, annotation_col = annotation_col, cluster_cols = F,
           color = color,
           show_rownames = F, show_colnames = F,
           annotation_legend = T, filename = "export/heatmap_channged.pdf")  ##
}

## cluster anno
if(T){
  library(clusterProfiler)
  library(org.Mm.eg.db)
  OrgDb=org.Mm.eg.db
  df <- bitr(rownames(nrDEG),fromType = "ENSEMBL",toType = "SYMBOL",OrgDb = org.At.tair.db)
  head(df)
  nrDEG$ENSEMBL <- row.names(nrDEG)
  nrDEG <- merge(nrDEG,df, by ="ENSEMBL"); 
  df2 <- bitr(nrDEG$ENSEMBL,fromType = "ENSEMBL",toType = "ENTREZID",OrgDb = org.Hs.eg.db)
  head(df2)
  nrDEG <-merge(nrDEG,df2,by="ENSEMBL",all.x=T)
  head(nrDEG)
  save(nrDEG,file = "data/nrDEG.Rdata")
  
}

if(T){
  gene_up = nrDEG[nrDEG$change=="up",'ENTREZID']
  gene_down = nrDEG[nrDEG$change=="down",'ENTREZID']
  gene_diff=c(gene_up,gene_down)
  gene_all=as.character(nrDEG[ ,'ENTREZID'] )
  data(geneList, package="DOSE") 
  head(geneList)
  boxplot(geneList)
  boxplot(nrDEG$logFC)
  
  geneList=nrDEG$logFC
  names(geneList)=nrDEG$ENTREZID
  geneList=sort(geneList,decreasing = T)
}

## note that the downregulated gene only 18, so the enrichment would not be signifigant.
# detailed plot
if(T){
  source('kegg_go_up_down.R')
  pro = 'siSUZ_NC'
  run_kegg(gene_up,gene_down,pro='siSUZ_NC')
  # 非常耗时,而且要翻墙。
  # run_go(gene_up,gene_down,pro='siSUZ_NC')
}

# GO in one file
if(T){
  go <- enrichGO(gene_up, 
                 OrgDb = "org.Hs.eg.db", 
                 ont           = "all" ,
                 pAdjustMethod = "BH",
                 pvalueCutoff  = 0.05,
                 qvalueCutoff  = 0.99,
                 readable      = TRUE) 
  library(ggplot2)
  library(stringr)
  barplot(go, split="ONTOLOGY")+ facet_grid(ONTOLOGY~., scale="free") 
  barplot(go, split="ONTOLOGY",font.size =10)+ 
    facet_grid(ONTOLOGY~., scale="free") + 
    scale_x_discrete(labels=function(x) str_wrap(x, width=50))+
    ggsave('export/gene_up_GO_all_barplot.pdf') 
  
  go <- enrichGO(gene_down, 
                 OrgDb = "org.Hs.eg.db", 
                 ont           = "all" ,
                 pAdjustMethod = "BH",
                 pvalueCutoff  = 0.05,
                 qvalueCutoff  = 0.99,
                 readable      = TRUE) 
  barplot(go, split="ONTOLOGY",font.size =10)+ 
    facet_grid(ONTOLOGY~., scale="free") + 
    scale_x_discrete(labels=function(x) str_wrap(x, width=50))+
    ggsave('export/gene_down_GO_all_barplot.pdf')
}
write.table(gene.datExpr,"data.txt", sep = "\t", row.names = T,quote=F)
write.table(nrDEG,"DEG.txt", sep = "\t", row.names = T,quote=F)

#!/usr/bin/R
# Count to FPKM
rm(list = ls())#删除目前工作目录的变量
library(xlsx)
library(readxl)
ann<- gset[,3:30]#读取基因文件
input<- gset[,1:2]#自己在Excel中把网盘里的txt文件基因和长度共两列提取出来
library(dplyr)
merge<-gset#根据基因那列进行合并
merge <- na.omit(merge)#删除错误值行
write.csv(merge,file = "merge.csv",sep = "\t")#读出文件，直接往下运行也许
#count to TPM
mycounts<-read.csv("S1-28-counts.csv")
head(mycounts)
rownames(mycounts)<-mycounts[,1]

head(mycounts)#最后一列Length是基因长度

#TPM caluator
kb <- mycounts$Length / 1000
kb
countdata <- mycounts[,3:30]
rpk <- countdata / kb
rpk
tpm <- t(t(rpk)/colSums(rpk) * 1000000)
head(tpm)
write.table(tpm,file="S1-28.tpm.xls",sep="\t",quote=F)
#count to FPKM
fpkmhe <- t(t(rpk)/colSums(countdata) * 10^6) 
head(fpkmhe)
write.table(fpkmhe,file="S1-28-FPKM.xls",sep="\t",quote=F)
###单样本的转录组差异分析
rm(list = ls())
library(edgeR)
library(DESeq2)
library(FactoMineR)
library(factoextra)
library(clusterProfiler)
library(org.Hs.eg.db)
library(org.Mm.eg.db)
library(stringr)
library(stringi)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(pheatmap)
library(VennDiagram)
library(RColorBrewer)
library(patchwork)
library(ggplotify)
library(ggpubr)

rawcount <- read.table("tpm.csv",sep = ",",header = T)
colnames(rawcount)
rawcount[1:4,1:4]
rawcount=rawcount[,1:4]
keep <- rowSums(rawcount[,2:4]>0) >= floor(0.75*ncol(rawcount[,2:4]))
filter_count <- rawcount[keep,] #获得filter_count矩阵
express_cpm <- log2(cpm(filter_count[,2:4])+ 1)
express_cpm[1:4,1:4] #获得cpm矩阵
colnames(express_cpm)
#2.获取分组信息
#根据列的信息，提取分组信息
colnames(rawcount) 

group=rep(c("Ctl","KD","OE"),each=1)
group #直接用部位作为group

group_list=factor(group,levels = c("Ctl","KD","OE"))
table(group_list)#检查一下组别数量
## 01绘制整体表达的箱线图
exprSet <- express_cpm
class(exprSet)
## [1] "matrix" "array"
## [1] "matrix" "array"
data <- data.frame(expression=c(exprSet),
                   sample=rep(colnames(exprSet),each=nrow(exprSet)))
compaired <- list(c("C","OE"),c("C","KD"))
p1 <- ggplot(data = data,aes(x=sample,y=expression,fill=sample))+ geom_boxplot() + theme(axis.text.x = element_text(angle = 90)) + 
  xlab(NULL) + ylab("log2(CPM+1)")+
  theme_bw()+ 
  geom_signif(comparisons = compaired, step_increase = 0.1,map_signif_level = T,test = t.test)
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5))
p1
dat <- express_cpm
dat <- as.data.frame(t(dat))
dat <- na.omit(dat)
dat$group_list <- group_list
dat_pca <- PCA(dat[,-ncol(dat)], graph = FALSE)#画图仅需数值型数据，去掉最后一列的分组信息
p2 <- fviz_pca_ind(dat_pca,
                   geom.ind = "point", # 只显示点，不显示文字
                   col.ind = dat$group_list, # 用不同颜色表示分组
                   palette = c("red", "green","blue"),
                   addEllipses = T, # 是否圈起来，少于4个样圈不起来
                   legend.title = "Groups") + theme_bw()
p1+p2
filter_count2 <- filter_count[,c(2,4)] #调整成正常在前；疾病在后
head(filter_count2)
a=str_split(rownames(filter_count1),"\\.",simplify = T)[,1]
rownames(filter_count)=filter_count[,1]
rownames(filter_count1)=filter_count[,1]


exprSet <- filter_count2
dim(exprSet)
library(DESeq2)

exprSet = filter_count[,c(2,4)]

group=rep(c("Ctl","OE"),each=1)
group
group_list=factor(group,levels = c("Ctl","OE"))
table(group_list)
group_list=group_list[1:2]

#加载包
library(edgeR) 
#设置分组信息
exprSet <- DGEList(counts = exprSet, group = group_list)
bcv = 0.1#设置bcv为0.1
et <- exactTest(exprSet, dispersion=bcv^2)
DEG_edgeR2=as.data.frame(topTags(et, n = nrow(exprSet$counts)))
head(DEG_edgeR2)
write.csv(DEG_edgeR2, "SHP-OE.csv",row.names = T, quote = FALSE)
fc_cutoff <- 2
pvalue <- 0.01
DEG_edgeR2$regulated <- "normal"
loc_up <- intersect(which(DEG_edgeR2$logFC>log2(fc_cutoff)),
                    which(DEG_edgeR2$PValue<pvalue))
loc_down <- intersect(which(DEG_edgeR2$logFC < (-log2(fc_cutoff))),
                      which(DEG_edgeR2$PValue<pvalue))
DEG_edgeR2$regulated[loc_up] <- "up"
DEG_edgeR2$regulated[loc_down] <- "down" 
table(DEG_edgeR2$regulated)
library(EnhancedVolcano)
EnhancedVolcano(DEG_edgeR2,
                lab =rownames(DEG_edgeR2),
                x = 'logFC',
                y = 'FDR')
cg=c("Pvalb","Cox7a1","Cox6a2")
deg_fc=DEG_edgeR[cg,]
head(deg_fc)