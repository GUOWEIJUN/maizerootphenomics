
library(ggpubr)

########root
data <- read.delim("data.txt",header = T,sep = "\t")

p1 <- ggboxplot(data, "group", "L", fill ="group", 
               #palette =c("#41476b","#675478","#9e6374","#c67b6f","#de9b71","#efbc82","#fbdfa2"),
               #add =c("boxplot"),add.params=list(fill ="white"),
               ylab = "Length of total roots (cm)",xlab = "lines") + 
  stat_compare_means(method = "t.test",paired = F)  + theme_bw()

p2 <- ggboxplot(data, "group", "PA", fill ="group", 
               #palette =c("#41476b","#675478","#9e6374","#c67b6f","#de9b71","#efbc82","#fbdfa2"),
               #add =c("boxplot"),add.params=list(fill ="white"),
               ylab = "Project area of total roots (cm2)",xlab = "lines") + 
  stat_compare_means(method = "t.test",paired = F)  + theme_bw()
p3 <- ggboxplot(data, "group", "SA", fill ="group", 
               #palette =c("#41476b","#675478","#9e6374","#c67b6f","#de9b71","#efbc82","#fbdfa2"),
               #add =c("boxplot"),add.params=list(fill ="white"),
               ylab = "Surface area of total roots (cm2)",xlab = "lines") + 
  stat_compare_means(method = "t.test",paired = F)  + theme_bw()
p4 <- ggboxplot(data, "group", "AD", fill ="group", 
               #palette =c("#41476b","#675478","#9e6374","#c67b6f","#de9b71","#efbc82","#fbdfa2"),
               #add =c("boxplot"),add.params=list(fill ="white"),
               ylab = "Average diameter of total roots (mm)",xlab = "lines") + 
  stat_compare_means(method = "t.test",paired = F)  + theme_bw()

p5 <- ggboxplot(data, "group", "RV", fill ="group", 
               #palette =c("#41476b","#675478","#9e6374","#c67b6f","#de9b71","#efbc82","#fbdfa2"),
               #add =c("boxplot"),add.params=list(fill ="white"),
               ylab = "RootVolume of total roots (cm3)",xlab = "lines") + 
  stat_compare_means(method = "t.test",paired = F)  + theme_bw()

p6 <- ggboxplot(data, "group", "Ti", fill ="group", 
               #palette =c("#41476b","#675478","#9e6374","#c67b6f","#de9b71","#efbc82","#fbdfa2"),
               #add =c("boxplot"),add.params=list(fill ="white"),
               ylab = "Tips of total roots",xlab = "lines") + 
  stat_compare_means(method = "t.test",paired = F)  + theme_bw()

p7 <- ggboxplot(data, "group", "Fo", fill ="group", 
               #palette =c("#41476b","#675478","#9e6374","#c67b6f","#de9b71","#efbc82","#fbdfa2"),
               #add =c("boxplot"),add.params=list(fill ="white"),
               ylab = "Forks of total roots",xlab = "lines") + 
  stat_compare_means(method = "t.test",paired = F)  + theme_bw()

p8 <- ggboxplot(data, "group", "Cr", fill ="group", 
               #palette =c("#41476b","#675478","#9e6374","#c67b6f","#de9b71","#efbc82","#fbdfa2"),
               #add =c("boxplot"),add.params=list(fill ="white"),
               ylab = "Cross of total roots",xlab = "lines") + 
  stat_compare_means(method = "t.test",paired = F)  + theme_bw()


p <- ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,nrow = 2,ncol = 4,common.legend = T)

pdf("root.boxplot.pdf",height = 5,width = 10)
p
dev.off()
png("root.boxplot.png",height = 500,width = 1000)
p
dev.off()

