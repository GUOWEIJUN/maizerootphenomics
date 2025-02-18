
library(ggpubr)

data <- read.delim("./Weight.txt",header = T,sep = "\t")

p1 <- ggbarplot(data,x = "group", y = "WFS", 
          add = c("mean_se"), #,"jitter"
          color = "group",fill = "group", palette =c("#78b5cc","#e0987e"),
          position = position_dodge(0.8)) +
  stat_compare_means(aes(group = group), label = "p.signif") + #ylim(0,8) +
  ylab("Weight of fresh shoot (g)") + 
  theme_bw()

p2 <- ggbarplot(data,x = "group", y = "WDS", 
          add = c("mean_se"),
          color = "group",fill = "group", palette =c("#78b5cc","#e0987e"),
          position = position_dodge(0.8)) +
  stat_compare_means(aes(group = group), label = "p.signif") + #ylim(0,1) +
  ylab("Weight of dry shoot (g)") + 
  theme_bw()
p3 <- ggbarplot(data,x = "group", y = "WFR", 
          add = c("mean_se"),
          color = "group",fill = "group", palette =c("#78b5cc","#e0987e"),
          position = position_dodge(0.8)) +
  stat_compare_means(aes(group = group), label = "p.signif") + ylim(0,2) +
  ylab("Weight of fresh root (g)") + 
  theme_bw()
p4 <- ggbarplot(data,x = "group", y = "WDR", 
          add = c("mean_se"),
          color = "group",fill = "group", palette =c("#78b5cc","#e0987e"),
          position = position_dodge(0.8)) +
  stat_compare_means(aes(group = group), label = "p.signif") + #ylim(0,0.15) +
  ylab("Weight of dry root (g)") + 
  theme_bw()
p5 <- ggbarplot(data,x = "group", y = "WF", 
          add = c("mean_se"),
          color = "group",fill = "group", palette =c("#78b5cc","#e0987e"),
          position = position_dodge(0.8)) +
  stat_compare_means(aes(group = group), label = "p.signif") + #ylim(0,6) +
  ylab("Weight of fresh seedlings (g)") + 
  theme_bw()
p6 <- ggbarplot(data,x = "group", y = "Biomass", 
          add = c("mean_se"),
          color = "group",fill = "group", palette =c("#78b5cc","#e0987e"),
          position = position_dodge(0.8)) +
  stat_compare_means(aes(group = group), label = "p.signif") + #ylim(0,1) +
  ylab("Weight of dry seedlings (g)") + 
  theme_bw()
p7 <- ggbarplot(data,x = "group", y = "WC", 
          add = c("mean_se"),
          color = "group",fill = "group", palette =c("#78b5cc","#e0987e"),
          position = position_dodge(0.8)) +
  stat_compare_means(aes(group = group), label = "p.signif") + #ylim(0,5) +
  ylab("Water content (g)") + 
  theme_bw()

p <- ggarrange(p1,p2,p3,p4,p5,p6,p7,nrow = 2,ncol = 4,common.legend = T)

pdf("weight.barplot.pdf",height = 5,width = 10)
p
dev.off()
png("weight.barplot.png",height = 500,width = 1000)
p
dev.off()

########root
data <- read.delim("./root.txt",header = T,sep = "\t")

p1 <- ggbarplot(data, "group", "L", fill ="group", 
               palette =c("#78b5cc","#e0987e"),
               add = c("mean_se"),
               ylab = "Length of total roots (cm)") + 
  stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()

p2 <- ggbarplot(data, "group", "PA", fill ="group", 
               palette =c("#78b5cc","#e0987e"),
               add = c("mean_se"),
               ylab = "Project area of total roots (cm2)") + 
  stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p3 <- ggbarplot(data, "group", "SA", fill ="group", 
               palette =c("#78b5cc","#e0987e"),
               add = c("mean_se"),
               ylab = "Surface area of total roots (cm2)") + 
  stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p4 <- ggbarplot(data, "group", "AD", fill ="group", 
               palette =c("#78b5cc","#e0987e"),
               add = c("mean_se"),
               ylab = "Average diameter of total roots (cm)") + 
  stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()

p5 <- ggbarplot(data, "group", "RV", fill ="group", 
               palette =c("#78b5cc","#e0987e"),
               add = c("mean_se"),
               ylab = "RootVolume of total roots (cm3)") + 
  stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()

p6 <- ggbarplot(data, "group", "Ti", fill ="group", 
               palette =c("#78b5cc","#e0987e"),
               add = c("mean_se"),
               ylab = "Tips of total roots") + 
  stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()

p7 <- ggbarplot(data, "group", "Fo", fill ="group", 
               palette =c("#78b5cc","#e0987e"),
               add = c("mean_se"),
               ylab = "Forks of total roots") + 
  stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()

p8 <- ggbarplot(data, "group", "Cr", fill ="group", 
               palette =c("#78b5cc","#e0987e"),
               add = c("mean_se"),
               ylab = "Cross of total roots") + 
  stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()

p9 <- ggbarplot(data, "group", "L_PR", fill ="group", 
               palette =c("#78b5cc","#e0987e"),
               add = c("mean_se"),
               ylab = "Length of primary roots (cm)") + 
  stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()

p10 <- ggbarplot(data, "group", "PA_PR", fill ="group", 
               palette =c("#78b5cc","#e0987e"),
               add = c("mean_se"),
               ylab = "Project area of primary roots (cm2)") + 
  stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p11 <- ggbarplot(data, "group", "SA_PR", fill ="group", 
               palette =c("#78b5cc","#e0987e"),
               add = c("mean_se"),
               ylab = "Surface area of primary roots (cm2)") + 
  stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p12 <- ggbarplot(data, "group", "AD_PR", fill ="group", 
               palette =c("#78b5cc","#e0987e"),
               add = c("mean_se"),
               ylab = "Average diameter of primary roots (cm)") + 
  stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()

p13 <- ggbarplot(data, "group", "RV_PR", fill ="group", 
               palette =c("#78b5cc","#e0987e"),
               add = c("mean_se"),
               ylab = "RootVolume of primary roots (cm3)") + 
  stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()

p14 <- ggbarplot(data, "group", "Ti_PR", fill ="group", 
               palette =c("#78b5cc","#e0987e"),
               add = c("mean_se"),
               ylab = "Tips of primary roots") + 
  stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()

p15 <- ggbarplot(data, "group", "Fo_PR", fill ="group", 
               palette =c("#78b5cc","#e0987e"),
               add = c("mean_se"),
               ylab = "Forks of primary roots") + 
  stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()

p16 <- ggbarplot(data, "group", "Cr_PR", fill ="group", 
               palette =c("#78b5cc","#e0987e"),
               add = c("mean_se"),
               ylab = "Cross of primary roots") + 
  stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()

p <- ggarrange(p1,p2,p3,p5,p6,p7,p8,p9,p10,p11,p13,p14,p15,p16,nrow = 4,ncol = 4,common.legend = T)

pdf("root.barplot.pdf",height = 10,width = 10)
p
dev.off()
png("root.barplot.png",height = 1500,width = 1500)
p
dev.off()

#P4和12单独输出

p <- ggarrange(p4,p12,nrow = 1,ncol = 2,common.legend = T)

pdf("root-DM.barplot.pdf",height = 4,width = 4)
p
dev.off()
png("root-DM.barplot.png",height = 400,width = 400)
p
dev.off()

#####slice

data <- read.delim("./slice.txt",header = T,sep = "\t")

p1    <- ggbarplot(data, "group", "DPRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Diameter of primary root maturation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p2    <- ggbarplot(data, "group", "SAPRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Surface area of primary root maturation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p3    <- ggbarplot(data, "group", "SDAPRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Stele diameter of primary root maturation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p4    <- ggbarplot(data, "group", "SSAPRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Stele surface area of primary root maturation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p5    <- ggbarplot(data, "group", "CDPRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Cortex diameter of primary root maturation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p6    <- ggbarplot(data, "group", "CSAPRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Cortex surface area of primary root maturation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p7    <- ggbarplot(data, "group", "XNPRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem number of primary root maturation zone") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p8    <- ggbarplot(data, "group", "XADPRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem average diameter of primary root maturation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p9    <- ggbarplot(data, "group", "XASAPRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem average surface area of primary root maturation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p10   <- ggbarplot(data, "group", "PNPRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem number of primary root maturation zone") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p11   <- ggbarplot(data, "group", "PADPRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem average diameter of primary root maturation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p12   <- ggbarplot(data, "group", "PASAPRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem average surface area of primary root maturation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p13   <- ggbarplot(data, "group", "DPRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Diameter of primary root meristematic zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p14   <- ggbarplot(data, "group", "SAPRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Surface area of primary root meristematic zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p15   <- ggbarplot(data, "group", "SDAPRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Stele diameter of primary root meristematic zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p16   <- ggbarplot(data, "group", "SSAPRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Stele surface area of primary root meristematic zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p17   <- ggbarplot(data, "group", "CDPRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Cortex diameter of primary root meristematic zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p18   <- ggbarplot(data, "group", "CSAPRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Cortex surface area of primary root meristematic zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p19   <- ggbarplot(data, "group", "XNPRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem number of primary root meristematic zone") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p20   <- ggbarplot(data, "group", "XADPRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem average diameter of primary root meristematic zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p21   <- ggbarplot(data, "group", "XASAPRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem average surface area of primary root meristematic zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p22   <- ggbarplot(data, "group", "PNPRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem number of primary root meristematic zone") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p23   <- ggbarplot(data, "group", "PADPRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem average diameter of primary root meristematic zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p24   <- ggbarplot(data, "group", "PASAPRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem average surface area of primary root meristematic zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p25   <- ggbarplot(data, "group", "DPREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Diameter of primary root elongation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p26   <- ggbarplot(data, "group", "SAPREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Surface area of primary root elongation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p27   <- ggbarplot(data, "group", "SDAPREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Stele diameter of primary root elongation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p28   <- ggbarplot(data, "group", "SSAPREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Stele surface area of primary root elongation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p29   <- ggbarplot(data, "group", "CDPREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Cortex diameter of primary root elongation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p30   <- ggbarplot(data, "group", "CSAPREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Cortex surface area of primary root elongation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p31   <- ggbarplot(data, "group", "XNPREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem number of primary root elongation zone ") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p32   <- ggbarplot(data, "group", "XADPREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem average diameter of primary root elongation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p33   <- ggbarplot(data, "group", "XASAPREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem average surface area of primary root elongation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p34   <- ggbarplot(data, "group", "PNPREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem number of primary root elongation zone") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p35   <- ggbarplot(data, "group", "PADPREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem average diameter of primary root elongation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p36   <- ggbarplot(data, "group", "PASAPREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem average surface area of primary root elongation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p37   <- ggbarplot(data, "group", "DLRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Diameter of lateral roots maturation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p38   <- ggbarplot(data, "group", "SALRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Surface area of lateral roots maturation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p39   <- ggbarplot(data, "group", "SDALRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Stele diameter of lateral roots maturation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p40   <- ggbarplot(data, "group", "SSALRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Stele surface area of lateral roots maturation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p41   <- ggbarplot(data, "group", "CDLRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Cortex diameter of lateral roots maturation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p42   <- ggbarplot(data, "group", "CSALRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Cortex surface area of lateral roots maturation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p43   <- ggbarplot(data, "group", "XNLRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem number of lateral roots maturation zone") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p44   <- ggbarplot(data, "group", "XADLRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem average diameter of lateral roots maturation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p45   <- ggbarplot(data, "group", "XASALRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem average surface area of lateral roots maturation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p46   <- ggbarplot(data, "group", "PNLRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem number of lateral roots maturation zone") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p47   <- ggbarplot(data, "group", "PADLRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem average diameter of lateral roots maturation zone(cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p48   <- ggbarplot(data, "group", "PASALRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem average surface area of lateral roots maturation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p49   <- ggbarplot(data, "group", "DLRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Diameter of lateral roots meristematic zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p50   <- ggbarplot(data, "group", "SALRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Surface area of lateral roots meristematic zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p51   <- ggbarplot(data, "group", "SDALRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Stele diameter of lateral roots meristematic zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p52   <- ggbarplot(data, "group", "SSALRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Stele surface area of lateral roots meristematic zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p53   <- ggbarplot(data, "group", "CDLRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Cortex diameter of lateral roots meristematic zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p54   <- ggbarplot(data, "group", "CSALRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Cortex surface area of lateral roots meristematic zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p55   <- ggbarplot(data, "group", "XNLRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem number of lateral roots meristematic zone") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p56   <- ggbarplot(data, "group", "XADLRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem average diameter of lateral roots meristematic zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p57   <- ggbarplot(data, "group", "XASALRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem average surface area of lateral roots meristematic zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p58   <- ggbarplot(data, "group", "PNLRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem number of lateral roots meristematic zone") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p59   <- ggbarplot(data, "group", "PADLRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem average diameter of lateral roots meristematic zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p60   <- ggbarplot(data, "group", "PASALRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem average surface area of lateral roots meristematic zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p61   <- ggbarplot(data, "group", "DLREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Diameter of lateral roots elongation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p62   <- ggbarplot(data, "group", "SALREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Surface area of lateral roots elongation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p63   <- ggbarplot(data, "group", "SDALREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Stele diameter of lateral roots elongation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p64   <- ggbarplot(data, "group", "SSALREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Stele surface area of lateral roots elongation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p65   <- ggbarplot(data, "group", "CDLREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Cortex diameter of lateral roots elongation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p66   <- ggbarplot(data, "group", "CSALREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Cortex surface area of lateral roots elongation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p67   <- ggbarplot(data, "group", "XNLREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem number of lateral roots elongation zone") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p68   <- ggbarplot(data, "group", "XADLREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem average diameter of lateral roots elongation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p69   <- ggbarplot(data, "group", "XASALREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem average surface area of lateral roots elongation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p70   <- ggbarplot(data, "group", "PNLREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem number of lateral roots elongation zone") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p71   <- ggbarplot(data, "group", "PADLREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem average diameter of lateral roots elongation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p72   <- ggbarplot(data, "group", "PASALREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem average surface area of lateral roots elongation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p73   <- ggbarplot(data, "group", "DCRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Diameter of crown roots maturation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p74   <- ggbarplot(data, "group", "SACRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Surface area of crown roots maturation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p75   <- ggbarplot(data, "group", "SDACRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Stele diameter of crown roots maturation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p76   <- ggbarplot(data, "group", "SSACRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Stele surface area of crown roots maturation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p77   <- ggbarplot(data, "group", "CDCRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Cortex diameter of crown roots maturation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p78   <- ggbarplot(data, "group", "CSACRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Cortex surface area of crown roots maturation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p79   <- ggbarplot(data, "group", "XNCRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem number of crown roots maturation zone") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p80   <- ggbarplot(data, "group", "XADCRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem average diameter of crown roots maturation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p81   <- ggbarplot(data, "group", "XASACRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem average surface area of crown roots maturation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p82   <- ggbarplot(data, "group", "PNCRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem number of crown roots maturation zone ") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p83   <- ggbarplot(data, "group", "PADCRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem average diameter of crown roots maturation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p84   <- ggbarplot(data, "group", "PASACRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem average surface area of crown roots maturation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p85   <- ggbarplot(data, "group", "DCRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Diameter of crown roots meristematic zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p86   <- ggbarplot(data, "group", "SACRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Surface area of crown roots meristematic zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p87   <- ggbarplot(data, "group", "SDACRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Stele diameter of crown roots meristematic zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p88   <- ggbarplot(data, "group", "SSACRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Stele surface area of crown roots meristematic zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p89   <- ggbarplot(data, "group", "CDCRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Cortex diameter of crown roots meristematic zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p90   <- ggbarplot(data, "group", "CSACRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Cortex surface area of crown roots meristematic zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p91   <- ggbarplot(data, "group", "XNCRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem number of crown roots meristematic zone") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p92   <- ggbarplot(data, "group", "XADCRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem average diameter of crown roots meristematic zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p93   <- ggbarplot(data, "group", "XASACRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem average surface area of crown roots meristematic zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p94   <- ggbarplot(data, "group", "PNCRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem number of crown roots meristematic zone") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p95   <- ggbarplot(data, "group", "PADCRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem average diameter of crown roots meristematic zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p96   <- ggbarplot(data, "group", "PASACRMZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem average surface area of crown roots meristematic zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p97   <- ggbarplot(data, "group", "DCREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Diameter of crown roots elongation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p98   <- ggbarplot(data, "group", "SACREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Surface area of crown roots elongation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p99   <- ggbarplot(data, "group", "SDACREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Stele diameter of crown roots elongation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p100  <- ggbarplot(data, "group", "SSACREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Stele surface area of crown roots elongation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p101  <- ggbarplot(data, "group", "CDCREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Cortex diameter of crown roots elongation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p102  <- ggbarplot(data, "group", "CSACREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Cortex surface area of crown roots elongation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p103  <- ggbarplot(data, "group", "XNCREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem number of crown roots elongation zone") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p104  <- ggbarplot(data, "group", "XADCREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem average diameter of crown roots elongation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p105  <- ggbarplot(data, "group", "XASACREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem average surface area of crown roots elongation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p106  <- ggbarplot(data, "group", "PNCREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem number of crown roots elongation zone") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p107  <- ggbarplot(data, "group", "PADCREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem average diameter of crown roots elongation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p108  <- ggbarplot(data, "group", "PASACREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem average surface area of crown roots elongation zone (cm2)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()


# pdf("slice.pdf",width = 12,height = 6)
# ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,nrow = 2,ncol = 6,common.legend = T)
# dev.off()

P1 <- ggarrange(p1,p2,p3,p4,p5,p6,p8,p9,p10,p11,p12,#p7,
          p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,
          p25,p26,p27,p28,p29,p30,p31,p32,p33,p34,p36,#,p35
          nrow = 6,ncol = 6,common.legend = T)


pdf("slice_pr.pdf",width = 16,height = 24)
P1
dev.off()
png("slice_pr.png",width = 1200,height = 1200)
P1
dev.off()

P2 <- ggarrange(p37,p38,p39,p40,p41,p42,p43,p44,p45,p46,p47,p48,
                p49,p50,p51,p52,p53,p54,p55,p56,p57,p58,p59,p60,
                p61,p62,p63,p64,p65,p66,p67,p68,p69,p70,p71,p72,
                nrow = 6,ncol = 6,common.legend = T)

pdf("slice_LR.pdf",width = 16,height = 24)
P2
dev.off()
png("slice_LR.png",width = 1200,height = 1200)
P2
dev.off()


P3 <- ggarrange(p73,p74,p75,p76,p77,p78,p79,p80,p81,p82,p83,p84,
                p85,p86,p87,p88,p89,p90,p91,p92,p93,p94,p95,p96,
                p97,p98,p99,p100,p101,p102,p103,p104,p105,p106,p107,p108,
                nrow = 6,ncol = 6,common.legend = T)

pdf("slice_CR.pdf",width = 16,height = 24)
P3
dev.off()
png("slice_CR.png",width = 1200,height = 1200)
P3
dev.off()

##
data <- read.delim("./root.txt",header = T,sep = "\t")

p4 <- ggbarplot(data, "group", "AD", fill ="group", 
               palette =c("#78b5cc","#e0987e"),
               add = c("mean_se"),
               ylab = "Average diameter of total roots (cm)") + 
  stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()
p12 <- ggbarplot(data, "group", "AD_PR", fill ="group", 
               palette =c("#78b5cc","#e0987e"),
               add = c("mean_se"),
               ylab = "Average diameter of primary roots (cm)") + 
  stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()

data1 <- read.delim("./slice.txt",header = T,sep = "\t")

#"Xylem number of primary root maturation zone"
p7    <- ggbarplot(data1, "group", "XNPRMaZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Xylem number of primary root maturation zone") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()

#"Phloem average diameter of primary root elongation zone (cm)"
p35   <- ggbarplot(data1, "group", "PADPREZ", fill ="group", palette =c("#78b5cc","#e0987e"),add = c("mean_se"),ylab = "Phloem average diameter of primary root elongation zone (cm)") + stat_compare_means(aes(group = group), label = "p.signif")  + theme_bw()

p <- ggarrange(p4,p12,p7,p35,nrow = 2,ncol = 2,common.legend = T)

pdf("Figur2.barplot.pdf",height = 8,width = 4)
p
dev.off()
png("Figur2.bar.png",height = 800,width = 400)
p
dev.off()
