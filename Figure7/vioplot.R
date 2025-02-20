
#setwd("D:\\guowj\\share\\haplotype\\SNP\\boxplot")
#install.packages("ggpubr")

library(ggplot2)
library(ggpubr)
# data <- read.table("PH.txt", sep = "\t" ,header = T)
# 
# # Add box plot
# 
# data$Hap<-factor(data$Hap,levels =c("Hap1","Hap2","Hap3","Hap4","Hap5","Hap6","Hap7","Hap8"))
# 
# #PH
# pdf("PH_8hap.pdf", w=10, h=5)
# ggviolin(data, x = "Hap", y = "PH", fill = "Hap",ylab = "Plant height (cm)",
#          add = "boxplot",add.params = list(fill="white")) + theme_bw()
# dev.off()





#多重比较
# LSD法（Fisher’s Least Significant Difference）
# LSD法检验处微小的差异，比较方便的是直接得出显著行标记，不需人工标记
#install.packages("agricolae")
library("agricolae")


#GW
oneway<-aov(data$PH~data$Hap,data = data)
anova(oneway)
out <- LSD.test(oneway,"data$Hap",p.adj="none")
out

# $groups
# data$PH groups
# Hap6 179.0893      a
# Hap4 176.9188      a
# Hap1 176.4598      a
# Hap8 173.6579     ab
# Hap7 171.3170     ab
# Hap3 170.1772     ab
# Hap2 169.4052     ab
# Hap5 168.5967     ab




#20230403
#新增穗位高、百粒重
data1 <- read.table("trait.txt", sep = "\t" ,header = T)

# Add box plot

data1$Hap<-factor(data1$Hap,levels =c("Hap1","Hap2","Hap3","Hap4","Hap5","Hap6"))

#PH
# pdf("PH_8hap.pdf", w=10, h=5)
# ggviolin(data, x = "Hap", y = "PH", fill = "Hap",ylab = "Plant height (cm)",
#          add = "boxplot",add.params = list(fill="white")) + theme_bw()
# dev.off()
ggviolin(data1, x = "Hap", y = "Length", fill = "Hap",ylab = "Length (cm)",
         add = "boxplot",add.params = list(fill="white")) + theme_bw()
         
p1 <- ggboxplot(data1, x = "Hap", y = "Length", fill = "Hap",ylab = "Root length (cm)") + theme_bw()
p2 <- ggboxplot(data1, x = "Hap", y = "ProjArea", fill = "Hap",ylab = "ProjArea (cm2)") + theme_bw()
p3 <- ggboxplot(data1, x = "Hap", y = "SurfArea", fill = "Hap",ylab = "SurfArea (cm2)") + theme_bw()
p4 <- ggboxplot(data1, x = "Hap", y = "AvgPiam", fill = "Hap",ylab = "Root average diameter (cm)") + theme_bw()
p5 <- ggboxplot(data1, x = "Hap", y = "RootVolume", fill = "Hap",ylab = "RootVolume") + theme_bw() 
p6 <- ggboxplot(data1, x = "Hap", y = "Tips", fill = "Hap",ylab = "Tips") + theme_bw()
p7 <- ggboxplot(data1, x = "Hap", y = "Forks", fill = "Hap",ylab = "Forks") + theme_bw()
p8 <- ggboxplot(data1, x = "Hap", y = "Crossings", fill = "Hap",ylab = "Crossings") + theme_bw() 

p11 <- ggboxplot(data1, x = "Hap", y = "Length_PR", fill = "Hap",ylab = "length of primary roots (cm)") + theme_bw()
p12 <- ggboxplot(data1, x = "Hap", y = "ProjArea_PR", fill = "Hap",ylab = "ProjArea of primary roots  (cm2)") + theme_bw()
p13 <- ggboxplot(data1, x = "Hap", y = "SurfArea_PR", fill = "Hap",ylab = "SurfArea of primary roots  (cm2)") + theme_bw()
p14 <- ggboxplot(data1, x = "Hap", y = "AvgPiam_PR", fill = "Hap",ylab = "Average diameter of primary roots  (cm)") + theme_bw()
p15 <- ggboxplot(data1, x = "Hap", y = "RootVolume_PR", fill = "Hap",ylab = "RootVolume of primary roots") + theme_bw() 
p16 <- ggboxplot(data1, x = "Hap", y = "Tips_PR", fill = "Hap",ylab = "Tips of primary roots") + theme_bw()
p17 <- ggboxplot(data1, x = "Hap", y = "Forks_PR", fill = "Hap",ylab = "Forks of primary roots") + theme_bw()
p18 <- ggboxplot(data1, x = "Hap", y = "Crossings_PR", fill = "Hap",ylab = "Crossings of primary roots") + theme_bw() 

pdf("Hap_boxplot_root.pdf", w=6, h=12)
ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol = 1,nrow = 8,common.legend = T)
dev.off()

pdf("Hap_boxplot_PR.pdf", w=6, h=12)
ggarrange(p11,p12,p13,p14,p15,p16,p17,p18,ncol = 1,nrow = 8,common.legend = T)
dev.off()




###
#violin

p1 <- ggviolin(data1, x = "Hap", y = "Length", fill = "Hap",ylab = "Root length (cm)",add = "boxplot",add.params = list(fill="white")) + theme_bw()
p2 <- ggviolin(data1, x = "Hap", y = "ProjArea", fill = "Hap",ylab = "ProjArea (cm2)",add = "boxplot",add.params = list(fill="white")) + theme_bw()
p3 <- ggviolin(data1, x = "Hap", y = "SurfArea", fill = "Hap",ylab = "SurfArea (cm2)",add = "boxplot",add.params = list(fill="white")) + theme_bw()
p4 <- ggviolin(data1, x = "Hap", y = "AvgPiam", fill = "Hap",ylab = "Root average diameter (cm)",add = "boxplot",add.params = list(fill="white")) + theme_bw()
p5 <- ggviolin(data1, x = "Hap", y = "RootVolume", fill = "Hap",ylab = "RootVolume",add = "boxplot",add.params = list(fill="white")) + theme_bw() 
p6 <- ggviolin(data1, x = "Hap", y = "Tips", fill = "Hap",ylab = "Tips",add = "boxplot",add.params = list(fill="white")) + theme_bw()
p7 <- ggviolin(data1, x = "Hap", y = "Forks", fill = "Hap",ylab = "Forks",add = "boxplot",add.params = list(fill="white")) + theme_bw()
p8 <- ggviolin(data1, x = "Hap", y = "Crossings", fill = "Hap",ylab = "Crossings",add = "boxplot",add.params = list(fill="white")) + theme_bw() 

p11 <- ggviolin(data1, x = "Hap", y = "Length_PR", fill = "Hap",ylab = "length of primary roots (cm)",add = "boxplot",add.params = list(fill="white")) + theme_bw()
p12 <- ggviolin(data1, x = "Hap", y = "ProjArea_PR", fill = "Hap",ylab = "ProjArea of primary roots (cm2)",add = "boxplot",add.params = list(fill="white")) + theme_bw()
p13 <- ggviolin(data1, x = "Hap", y = "SurfArea_PR", fill = "Hap",ylab = "SurfArea of primary roots (cm2)",add = "boxplot",add.params = list(fill="white")) + theme_bw()
p14 <- ggviolin(data1, x = "Hap", y = "AvgPiam_PR", fill = "Hap",ylab = "Average diameter of primary roots  (cm)",add = "boxplot",add.params = list(fill="white")) + theme_bw()
p15 <- ggviolin(data1, x = "Hap", y = "RootVolume_PR", fill = "Hap",ylab = "RootVolume of primary roots",add = "boxplot",add.params = list(fill="white")) + theme_bw() 
p16 <- ggviolin(data1, x = "Hap", y = "Tips_PR", fill = "Hap",ylab = "Tips of primary roots",add = "boxplot",add.params = list(fill="white")) + theme_bw()
p17 <- ggviolin(data1, x = "Hap", y = "Forks_PR", fill = "Hap",ylab = "Forks of primary roots",add = "boxplot",add.params = list(fill="white")) + theme_bw()
p18 <- ggviolin(data1, x = "Hap", y = "Crossings_PR", fill = "Hap",ylab = "Crossings of primary roots",add = "boxplot",add.params = list(fill="white")) + theme_bw() 

pdf("Hap_ggviolin_root.pdf", w=6, h=12)
ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol = 1,nrow = 8,common.legend = T)
dev.off()

pdf("Hap_ggviolin_PR.pdf", w=6, h=12)
ggarrange(p11,p12,p13,p14,p15,p16,p17,p18,ncol = 1,nrow = 8,common.legend = T)
dev.off()

###
library("agricolae")
#PH
oneway<-aov(data1$Length~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out
# $groups
# data1$Length groups
# Hap1     389.4111      a
# Hap3     328.5276     ab
# Hap5     325.2850     ab
# Hap2     320.2428      b
# Hap4     305.9910      b
# Hap6     278.5100      b

oneway<-aov(data1$ProjArea~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out
# $groups
# data1$ProjArea groups
# Hap1       19.64946      a
# Hap4       17.68048     ab
# Hap5       17.53833     ab
# Hap2       17.33725     ab
# Hap3       15.78200      b
# Hap6       14.91125      b
oneway<-aov(data1$SurfArea~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out

# $groups
# data1$SurfArea groups
# Hap1       61.51824      a
# Hap4       55.54381     ab
# Hap5       55.10167     ab
# Hap2       54.46800     ab
# Hap3       49.58080      b
# Hap6       46.83875      b

oneway<-aov(data1$AvgPiam~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out

# $groups
# data1$AvgPiam groups
# Hap4     0.5909524      a
# Hap6     0.5575000     ab
# Hap2     0.5567500     ab
# Hap5     0.5475000     ab
# Hap3     0.5468000     ab
# Hap1     0.5239189      b

oneway<-aov(data1$RootVolume~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out
# $groups
# data1$RootVolume groups
# Hap4        0.8309524      a
# Hap1        0.7986486      a
# Hap2        0.7547500      a
# Hap5        0.7350000      a
# Hap3        0.6560000      a
# Hap6        0.6412500      a

oneway<-aov(data1$Tips~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out
# $groups
# data1$Tips groups
# Hap1   640.7026      a
# Hap3   532.8932      a
# Hap4   523.4133      a
# Hap2   518.6337      a
# Hap5   478.6808      a
# Hap6   397.2700      a

oneway<-aov(data1$Forks~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out
# $groups
# data1$Forks groups
# Hap1   1253.1265      a
# Hap3   1178.8336     ab
# Hap5    943.3475     ab
# Hap2    925.8125      b
# Hap4    911.7457      b
# Hap6    687.5000      b

oneway<-aov(data1$Crossings~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out
# $groups
# data1$Crossings groups
# Hap1       136.36836      a
# Hap3       118.44760     ab
# Hap2       105.86200     ab
# Hap4        96.41905      b
# Hap5        95.14500      b
# Hap6        63.55375      b
# 

####pr

oneway<-aov(data1$Length_PR~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out

# Analysis of Variance Table
# 
# Response: data1$Length_PR
# Df  Sum Sq Mean Sq F value  Pr(>F)  
# data1$Hap   5  106329   21266   2.782 0.01915 *
#   Residuals 174 1330049    7644                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# $groups
# data1$Length_PR groups
# Hap1        179.0665      a
# Hap3        162.9428     ab
# Hap5        136.6583     ab
# Hap4        132.8267      b
# Hap6        123.6375      b
# Hap2        123.4040      b

oneway<-aov(data1$ProjArea_PR~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out
# Analysis of Variance Table
# 
# Response: data1$ProjArea_PR
# Df  Sum Sq Mean Sq F value  Pr(>F)  
# data1$Hap   5  162.15  32.431  2.6166 0.02612 *
#   Residuals 174 2156.58  12.394                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# $groups
# data1$ProjArea_PR groups
# Hap1          8.368243      a
# Hap3          7.190800     ab
# Hap5          7.185833     ab
# Hap4          6.857143     ab
# Hap6          6.113750     ab
# Hap2          6.054000      b

oneway<-aov(data1$SurfArea_PR~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out

# Analysis of Variance Table
# 
# Response: data1$SurfArea_PR
# Df  Sum Sq Mean Sq F value Pr(>F)  
# data1$Hap   5  1614.6  322.91  2.4753  0.034 *
#   Residuals 174 22698.8  130.45                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# $groups
# data1$SurfArea_PR groups
# Hap1          26.24081      a
# Hap3          23.56080     ab
# Hap5          21.88500     ab
# Hap4          21.43571     ab
# Hap6          19.20750     ab
# Hap2          18.97025      b

oneway<-aov(data1$AvgPiam_PR~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out
# $groups
# data1$AvgPiam_PR groups
# Hap4        0.5552381      a
# Hap6        0.5287500     ab
# Hap5        0.5233333     ab
# Hap3        0.5232000     ab
# Hap2        0.5230000     ab
# Hap1        0.4900000      b

oneway<-aov(data1$RootVolume_PR~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out
# $groups
# data1$RootVolume_PR groups
# Hap1           0.3145946      a
# Hap4           0.2957143     ab
# Hap5           0.2775000     ab
# Hap3           0.2728000     ab
# Hap6           0.2537500     ab
# Hap2           0.2390000      b

oneway<-aov(data1$Tips_PR~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out


# Response: data1$Tips_PR
# Df  Sum Sq Mean Sq F value  Pr(>F)  
# data1$Hap   5  254771   50954  2.6181 0.02605 *
#   Residuals 174 3386383   19462                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# $groups
# data1$Tips_PR groups
# Hap1      267.8918      a
# Hap3      245.6068     ab
# Hap4      206.1352     ab
# Hap2      195.7040      b
# Hap5      166.4292      b
# Hap6      163.9175      b

oneway<-aov(data1$Forks_PR~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out
# Analysis of Variance Table
# 
# Response: data1$Forks_PR
# Df   Sum Sq Mean Sq F value  Pr(>F)  
# data1$Hap   5  2233429  446686  2.5309 0.03066 *
#   Residuals 174 30709653  176492                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# $groups
# data1$Forks_PR groups
# Hap3       608.7000      a
# Hap1       543.3511      a
# Hap4       382.5400     ab
# Hap5       356.7358     ab
# Hap2       327.7208      b
# Hap6       296.5838      b

oneway<-aov(data1$Crossings_PR~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out
# Analysis of Variance Table
# 
# Response: data1$Crossings_PR
# Df Sum Sq Mean Sq F value  Pr(>F)  
# data1$Hap   5  31919  6383.8  2.7462 0.02054 *
#   Residuals 171 397504  2324.6                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# $groups
# data1$Crossings_PR groups
# Hap1           71.86055      a
# Hap3           66.33480     ab
# Hap5           50.78500    abc
# Hap4           45.75300     bc
# Hap2           41.78128      c
# Hap6           40.84500      c



##################
#weight

data1 <- read.table("weight.txt", sep = "\t" ,header = T)

# Add box plot

data1$Hap<-factor(data1$Hap,levels =c("Hap1","Hap2","Hap3","Hap4","Hap5","Hap6"))


p1 <- ggboxplot(data1, x = "Hap", y = "WFS", fill = "Hap",ylab = "Weight of fresh shoot (g)") + theme_bw()
p2 <- ggboxplot(data1, x = "Hap", y = "WDS", fill = "Hap",ylab = "Weight of dry shoot (g)") + theme_bw()
p3 <- ggboxplot(data1, x = "Hap", y = "WFR", fill = "Hap",ylab = "Weight of fresh root (g)") + theme_bw()
p4 <- ggboxplot(data1, x = "Hap", y = "WDR", fill = "Hap",ylab = "Weight of dry root (g)") + theme_bw()
p5 <- ggboxplot(data1, x = "Hap", y = "WF", fill = "Hap",ylab = "Weight of fresh seedlings (g)") + theme_bw() 
p6 <- ggboxplot(data1, x = "Hap", y = "Biomass", fill = "Hap",ylab = "Weight of dry seedlings (g)") + theme_bw()
p7 <- ggboxplot(data1, x = "Hap", y = "WC", fill = "Hap",ylab = "Water content (g)") + theme_bw()

pdf("Hap_boxplot_weight.pdf", w=6, h=12)
ggarrange(p1,p2,p3,p4,p5,p6,p7,ncol = 1,nrow = 8,common.legend = T)
dev.off()




###
#violin
p11 <- ggviolin(data1, x = "Hap", y = "WFS", fill = "Hap",ylab = "Weight of fresh shoot (g)",add = "boxplot",add.params = list(fill="white")) + theme_bw()
p12 <- ggviolin(data1, x = "Hap", y = "WDS", fill = "Hap",ylab = "Weight of dry shoot (g)",add = "boxplot",add.params = list(fill="white")) + theme_bw()
p13 <- ggviolin(data1, x = "Hap", y = "WFR", fill = "Hap",ylab = "Weight of fresh root (g)",add = "boxplot",add.params = list(fill="white")) + theme_bw()
p14 <- ggviolin(data1, x = "Hap", y = "WDR", fill = "Hap",ylab = "Weight of dry root (g)",add = "boxplot",add.params = list(fill="white")) + theme_bw()
p15 <- ggviolin(data1, x = "Hap", y = "WF", fill = "Hap",ylab = "Weight of fresh seedlings (g)",add = "boxplot",add.params = list(fill="white")) + theme_bw() 
p16 <- ggviolin(data1, x = "Hap", y = "Biomass", fill = "Hap",ylab = "Weight of dry seedlings (g)",add = "boxplot",add.params = list(fill="white")) + theme_bw()
p17 <- ggviolin(data1, x = "Hap", y = "WC", fill = "Hap",ylab = "Water content (g)",add = "boxplot",add.params = list(fill="white")) + theme_bw()

pdf("Hap_ggviolin_weight.pdf", w=6, h=12)
ggarrange(p11,p12,p13,p14,p15,p16,p17,ncol = 1,nrow = 8,common.legend = T)
dev.off()


##显著性检验
oneway<-aov(data1$WFS~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out

groups
# data1$WFS groups
# Hap2  1.914000      a
# Hap1  1.881279      a
# Hap4  1.798294      a
# Hap3  1.659292      a
# Hap6  1.614625      a
# Hap5  1.569818      a

oneway<-aov(data1$WDS~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out
# $groups
# data1$WDS groups
# Hap4 0.4991176      a
# Hap3 0.4696667      a
# Hap2 0.4521081      a
# Hap5 0.4463636      a
# Hap1 0.4174412      a
# Hap6 0.1731250      a

oneway<-aov(data1$WFR~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out
# $groups
# data1$WFR groups
# Hap6  1.363500      a
# Hap1  1.234471      a
# Hap5  1.172091      a
# Hap3  1.096917      a
# Hap2  1.094730      a
# Hap4  1.031765      a
oneway<-aov(data1$WDR~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out

# $groups
# data1$WDR groups
# Hap2 0.10827027      a
# Hap4 0.10417647      a
# Hap1 0.10352941      a
# Hap3 0.09954167      a
# Hap5 0.09609091      a
# Hap6 0.08937500      a
oneway<-aov(data1$WF~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out

# $groups
# data1$WF groups
# Hap1 3.115735      a
# Hap2 3.008676      a
# Hap4 2.830118      a
# Hap6 2.824787      a
# Hap3 2.756208      a
# Hap5 2.741909      a

oneway<-aov(data1$Biomass~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out
# $groups
# data1$Biomass groups
# Hap4     0.6032941      a
# Hap3     0.5691667      a
# Hap2     0.5603514      a
# Hap5     0.5424545      a
# Hap1     0.5210147      a
# Hap6     0.2626250      a
oneway<-aov(data1$WC~data1$Hap,data = data1)
anova(oneway)
out <- LSD.test(oneway,"data1$Hap",p.adj="none")
out
# $groups
# data1$WC groups
# Hap1 2.594750      a
# Hap6 2.593775      a
# Hap2 2.448378      a
# Hap4 2.226882      a
# Hap5 2.199455      a
# Hap3 2.186958      a

