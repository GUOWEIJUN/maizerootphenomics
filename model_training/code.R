library(doParallel)
cl=makeCluster(16)
registerDoParallel(cl)
library(caret)
library(ggpubr)
library(ggplot2)
library(plyr)
library(ipred)
######################################################模型训练
matr=read.table(file="matr.txt",header=T)
dataSet=read.table(file="dataSet.txt",header=T)
for(i in 1:ncol(matr)){
  n=colnames(matr)[i]
  na=formula(paste0(n,"~."))
  Set=as.data.frame(dataSet)
  Set[,109]=matr[,i]
  colnames(Set)[colnames(Set) == 'V109'] =n
  party=createDataPartition(Set[,109],p=0.8,list=F,times=1)
  Set_train=Set[party,]
  Set_test=Set[-party,]
  fitControl=trainControl(method="repeatedcv",number = 10,repeats = 1)
  model1=na %>% train(data=Set_train,method="treebag",trControl=fitControl,linout=TRUE)
  write.table(varImp(model1$finalModel),file = paste0(n,".varImp.txt"),sep = "\t")
  saveRDS(model1,file = paste0(n,"_treebag_model.model")) 
  pred1=predict(model1$finalModel,Set_test)
  test2=as.data.frame(cbind(pred1,Set_test[,109]))
  colnames(test2)=c("pred2",n)
  xvs=test2[,2]
  yvs=test2[,1]
  xplot=ggplot(test2,aes(x=xvs,y=pred2))+geom_point(color="#00AFBB")+
    stat_smooth(method="lm",se=TRUE,level=0.99,colour="#00AFBB")+
    stat_cor(method = "pearson")+
    theme_bw()+xlab(n)
  ggsave(xplot,file=paste0(n,"_treebag_cor.pdf"))
  write.table(test2,file=paste0(n,"_predictData.txt"),quote = F,col.names = TRUE)
  write.table(postResample(pred = pred1,
                           obs = Set_test[,109]),file =paste0(n,"_treebag.postResample.txt") ,sep = "\t")
  saveRDS(model1$finalModel,file = paste0(n,"_treebag.model")) 
  write.table(model1$resample,file = paste0(n,"_treebag.resample.txt"))   
}
######################################################绘制样本贡献度图

library(RColorBrewer)
library(ggplot2)
display.brewer.all()
mycol<- brewer.pal(n=4,name="GnBu")

#ProjArea
PA="ProjArea"
imp=read.table(file="ProjArea.varImp.txt")
imp$Trait=rownames(imp)
a=rev(order(imp$Overall))
b=imp[a,]
b$Trait=factor(b$Trait,levels = rownames(b))
b$label=c(rownames(b)[1:25],rep(NA,83))
CPRP=ggplot(b,aes(x=Trait,y=Overall,group=1,label=label))+geom_line(col="#7BCCC4",size=2)+theme_bw()+theme(panel.grid=element_blank())+
  geom_text(size=1)
ggsave(CPRP,file=paste0(PA,"_varImp.pdf"),width = 6,height=4)
#创建变量
PA5=as.character(b[1:5,2])
PA10=as.character(b[1:10,2])
PA15=as.character(b[1:15,2])
PA20=as.character(b[1:20,2])
PA25=as.character(b[1:25,2])
PA30=as.character(b[1:30,2])
PA35=as.character(b[1:35,2])


#RootVolume
RVP="RootVolume_PR"
imp=read.table(file="RootVolume_PR.varImp.txt")
imp$Trait=rownames(imp)
a=rev(order(imp$Overall))
b=imp[a,]
b$Trait=factor(b$Trait,levels = rownames(b))
b$label=c(rownames(b)[1:25],rep(NA,83))
CPRP=ggplot(b,aes(x=Trait,y=Overall,group=1,label=label))+geom_line(col="#7BCCC4",size=2)+theme_bw()+theme(panel.grid=element_blank())+
  geom_text(size=1)
ggsave(CPRP,file=paste0(RVP,"_varImp.pdf"),width = 6,height=4)
#创建变量
RVP5=as.character(b[1:5,2])
RVP10=as.character(b[1:10,2])
RVP15=as.character(b[1:15,2])
RVP20=as.character(b[1:20,2])
RVP25=as.character(b[1:25,2])
RVP30=as.character(b[1:30,2])
RVP35=as.character(b[1:35,2])
#########################################################取贡献度靠前的数据看预测相关性
cal_model_cor5=function(i,y){
  n=colnames(matr_s)[i]
  na=formula(paste0(n,"~."))
  Set=qiepian_s
  Set[,6]=matr_s[,i]
  colnames(Set)[colnames(Set) == 'V6'] =n
  party=createDataPartition(Set[,6],p=0.8,list=F,times=1)
  Set_train=Set[party,]
  Set_test=Set[-party,]
  
  fitControl=trainControl(method="repeatedcv",number = 10,repeats = 1)
  model1=na %>% train(data=Set_train,method="treebag",trControl=fitControl,linout=TRUE)
  #write.table(varImp(model1$finalModel,scale=TRUE),file = paste0(n,".varImp.txt"),sep = "\t")
  pred1=predict(model1,Set_test)
  Set_all=as.data.frame(cbind(pred1,Set_test[,6]))
  colnames(Set_all)=c("pred1",n)
  xvs=Set_all[,2]
  xplot=ggplot(Set_all,aes(x=xvs,y=pred1))+
    geom_point(color="#00AFBB")+
    stat_smooth(method="lm",level=0.99,colour="#00AFBB")+stat_cor(method = "pearson")+
    theme_bw()+xlab(n)
  ggsave(xplot,file=paste0(n,"_5_cor.pdf"))
}
cal_model_cor10=function(i,y){
  n=colnames(matr_s)[i]
  na=formula(paste0(n,"~."))
  Set=qiepian_s
  Set[,11]=matr_s[,i]
  colnames(Set)[colnames(Set) == 'V11'] =n
  party=createDataPartition(Set[,11],p=0.8,list=F,times=1)
  Set_train=Set[party,]
  Set_test=Set[-party,]
  
  fitControl=trainControl(method="repeatedcv",number = 10,repeats = 1)
  model1=na %>% train(data=Set_train,method="treebag",trControl=fitControl,linout=TRUE)
  #write.table(varImp(model1$finalModel,scale=TRUE),file = paste0(n,".varImp.txt"),sep = "\t")
  pred1=predict(model1,Set_test)
  Set_all=as.data.frame(cbind(pred1,Set_test[,11]))
  colnames(Set_all)=c("pred1",n)
  xvs=Set_all[,2]
  xplot=ggplot(Set_all,aes(x=xvs,y=pred1))+
    geom_point(color="#00AFBB")+
    stat_smooth(method="lm",level=0.99,colour="#00AFBB")+stat_cor(method = "pearson")+
    theme_bw()+xlab(n)
  ggsave(xplot,file=paste0(n,"_10_cor.pdf"))
}
cal_model_cor15=function(i,y){
  n=colnames(matr_s)[i]
  na=formula(paste0(n,"~."))
  Set=qiepian_s
  Set[,16]=matr_s[,i]
  colnames(Set)[colnames(Set) == 'V16'] =n
  party=createDataPartition(Set[,16],p=0.8,list=F,times=1)
  Set_train=Set[party,]
  Set_test=Set[-party,]
  
  fitControl=trainControl(method="repeatedcv",number = 10,repeats = 1)
  model1=na %>% train(data=Set_train,method="treebag",trControl=fitControl,linout=TRUE)
  #write.table(varImp(model1$finalModel,scale=TRUE),file = paste0(n,".varImp.txt"),sep = "\t")
  pred1=predict(model1,Set_test)
  Set_all=as.data.frame(cbind(pred1,Set_test[,16]))
  colnames(Set_all)=c("pred1",n)
  xvs=Set_all[,2]
  xplot=ggplot(Set_all,aes(x=xvs,y=pred1))+
    geom_point(color="#00AFBB")+
    stat_smooth(method="lm",level=0.99,colour="#00AFBB")+stat_cor(method = "pearson")+
    theme_bw()+xlab(n)
  ggsave(xplot,file=paste0(n,"_15_cor.pdf"))
}
cal_model_cor20=function(i,y){
  n=colnames(matr_s)[i]
  na=formula(paste0(n,"~."))
  Set=qiepian_s
  Set[,21]=matr_s[,i]
  colnames(Set)[colnames(Set) == 'V21'] =n
  party=createDataPartition(Set[,21],p=0.8,list=F,times=1)
  Set_train=Set[party,]
  Set_test=Set[-party,]
  
  fitControl=trainControl(method="repeatedcv",number = 10,repeats = 1)
  model1=na %>% train(data=Set_train,method="treebag",trControl=fitControl,linout=TRUE)
  #write.table(varImp(model1$finalModel,scale=TRUE),file = paste0(n,".varImp.txt"),sep = "\t")
  pred1=predict(model1,Set_test)
  Set_all=as.data.frame(cbind(pred1,Set_test[,21]))
  colnames(Set_all)=c("pred1",n)
  xvs=Set_all[,2]
  xplot=ggplot(Set_all,aes(x=xvs,y=pred1))+
    geom_point(color="#00AFBB")+
    stat_smooth(method="lm",level=0.99,colour="#00AFBB")+stat_cor(method = "pearson")+
    theme_bw()+xlab(n)
  ggsave(xplot,file=paste0(n,"_20_cor.pdf"))
}
cal_model_cor25=function(i,y){
  n=colnames(matr_s)[i]
  na=formula(paste0(n,"~."))
  Set=qiepian_s
  Set[,26]=matr_s[,i]
  colnames(Set)[colnames(Set) == 'V26'] =n
  party=createDataPartition(Set[,26],p=0.8,list=F,times=1)
  Set_train=Set[party,]
  Set_test=Set[-party,]
  
  fitControl=trainControl(method="repeatedcv",number = 10,repeats = 1)
  model1=na %>% train(data=Set_train,method="treebag",trControl=fitControl,linout=TRUE)
  #write.table(varImp(model1$finalModel,scale=TRUE),file = paste0(n,".varImp.txt"),sep = "\t")
  pred1=predict(model1,Set_test)
  Set_all=as.data.frame(cbind(pred1,Set_test[,26]))
  colnames(Set_all)=c("pred1",n)
  xvs=Set_all[,2]
  xplot=ggplot(Set_all,aes(x=xvs,y=pred1))+
    geom_point(color="#00AFBB")+
    stat_smooth(method="lm",level=0.99,colour="#00AFBB")+stat_cor(method = "pearson")+
    theme_bw()+xlab(n)
  ggsave(xplot,file=paste0(n,"_25_cor.pdf"))
}
cal_model_cor30=function(i,y){
  n=colnames(matr_s)[i]
  na=formula(paste0(n,"~."))
  Set=qiepian_s
  Set[,31]=matr_s[,i]
  colnames(Set)[colnames(Set) == 'V31'] =n
  party=createDataPartition(Set[,31],p=0.8,list=F,times=1)
  Set_train=Set[party,]
  Set_test=Set[-party,]
  
  fitControl=trainControl(method="repeatedcv",number = 10,repeats = 1)
  model1=na %>% train(data=Set_train,method="treebag",trControl=fitControl,linout=TRUE)
  #write.table(varImp(model1$finalModel,scale=TRUE),file = paste0(n,".varImp.txt"),sep = "\t")
  pred1=predict(model1,Set_test)
  Set_all=as.data.frame(cbind(pred1,Set_test[,31]))
  colnames(Set_all)=c("pred1",n)
  xvs=Set_all[,2]
  xplot=ggplot(Set_all,aes(x=xvs,y=pred1))+
    geom_point(color="#00AFBB")+
    stat_smooth(method="lm",level=0.99,colour="#00AFBB")+stat_cor(method = "pearson")+
    theme_bw()+xlab(n)
  ggsave(xplot,file=paste0(n,"_30_cor.pdf"))
}
cal_model_cor35=function(i,y){
  n=colnames(matr_s)[i]
  na=formula(paste0(n,"~."))
  Set=qiepian_s
  Set[,36]=matr_s[,i]
  colnames(Set)[colnames(Set) == 'V36'] =n
  party=createDataPartition(Set[,36],p=0.8,list=F,times=1)
  Set_train=Set[party,]
  Set_test=Set[-party,]
  
  fitControl=trainControl(method="repeatedcv",number = 10,repeats = 1)
  model1=na %>% train(data=Set_train,method="treebag",trControl=fitControl,linout=TRUE)
  #write.table(varImp(model1$finalModel,scale=TRUE),file = paste0(n,".varImp.txt"),sep = "\t")
  pred1=predict(model1,Set_test)
  Set_all=as.data.frame(cbind(pred1,Set_test[,36]))
  colnames(Set_all)=c("pred1",n)
  xvs=Set_all[,2]
  xplot=ggplot(Set_all,aes(x=xvs,y=pred1))+
    geom_point(color="#00AFBB")+
    stat_smooth(method="lm",level=0.99,colour="#00AFBB")+stat_cor(method = "pearson")+
    theme_bw()+xlab(n)
  ggsave(xplot,file=paste0(n,"_35_cor.pdf"))
}

matr_s=matr
Set=as.data.frame(dataSet)
{
  qiepian_s=Set[,colnames(Set) %in% PA5]
  cal_model_cor5(1)
  qiepian_s=Set[,colnames(Set) %in% PA10]
  cal_model_cor10(1)
  qiepian_s=Set[,colnames(Set) %in% PA15]
  cal_model_cor15(1)
  qiepian_s=Set[,colnames(Set) %in% PA20]
  cal_model_cor20(1)
  qiepian_s=Set[,colnames(Set) %in% PA25]
  cal_model_cor25(1)
  qiepian_s=Set[,colnames(Set) %in% PA30]
  cal_model_cor30(1)
  qiepian_s=Set[,colnames(Set) %in% PA35]
  cal_model_cor35(1)
}#repeat 5 times
Set=as.data.frame(dataSet)
{
  qiepian_s=Set[,colnames(Set) %in% RVP5]
  cal_model_cor5(2)
  qiepian_s=Set[,colnames(Set) %in% RVP10]
  cal_model_cor10(2)
  qiepian_s=Set[,colnames(Set) %in% RVP15]
  cal_model_cor15(2)
  qiepian_s=Set[,colnames(Set) %in% RVP20]
  cal_model_cor20(2)
  qiepian_s=Set[,colnames(Set) %in% RVP25]
  cal_model_cor25(2)
  qiepian_s=Set[,colnames(Set) %in% RVP30]
  cal_model_cor30(2)
  qiepian_s=Set[,colnames(Set) %in% RVP35]
  cal_model_cor35(2)
}#

#write output cor in a text file
PA=read.table(file="PA.txt",row.names = 1,header=TRUE)
library(plotrix)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
display.brewer.all()
mycol<- brewer.pal(n=4,name="Set3")
RAC=apply(PA[1:7,1:5],1,function(x){
  std.error(x)
})
PA$SE=RAC
PA$mean=rowMeans(PA[,1:3])
PA$head=rownames(PA)
PA$head=factor(PA$head,levels = c(5,10,15,20,25,30,35))
PA_bar=ggplot(PA,aes(x=head,y=mean))+
  geom_bar(position=position_dodge(), stat="identity",fill="#8DD3C7")+
  geom_line(group=1,color="#FB8072",size=1)+
  geom_point(size=0.1)+
  geom_errorbar(aes(ymin=mean,ymax=mean+SE),
               width=.2, # 设置误差线的宽度 
               position=position_dodge(.9))
ggsave(PA_bar,file="PA_bar.pdf",width=6.5,height=4)

Set=as.data.frame(dataSet)
{
  qiepian_s=Set[,colnames(Set) %in% RVP5]
  corre5=c()
  pre5=c()
  for(zx in 1:25){
    i=2
    n=colnames(matr_s)[i]
    na=formula(paste0(n,"~."))
    Set=qiepian_s
    Set[,6]=matr_s[,i]
    colnames(Set)[colnames(Set) == 'V6'] =n
    party=createDataPartition(Set[,6],p=0.8,list=F,times=1)
    Set_train=Set[party,]
    Set_test=Set[-party,]
    
    fitControl=trainControl(method="repeatedcv",number = 10,repeats = 1)
    model1=na %>% train(data=Set_train,method="treebag",trControl=fitControl,linout=TRUE)
    #write.table(varImp(model1$finalModel,scale=TRUE),file = paste0(n,".varImp.txt"),sep = "\t")
    pred1=predict(model1,Set_test)
    Set_all=as.data.frame(cbind(pred1,Set_test[,6]))
    colnames(Set_all)=c("pred1",n)
    rv=cor.test(Set_all[,1],Set_all[,2])
    corre5=c(corre5,rv$estimate)
    pre5=c(pre5,rv[["p.value"]])
  }
  
  
  qiepian_s=Set[,colnames(Set) %in% RVP10]
  corre10=c()
  pre10=c()
  for(zx in 1:25){
    i=2
    n=colnames(matr_s)[i]
    na=formula(paste0(n,"~."))
    Set=qiepian_s
    Set[,11]=matr_s[,i]
    colnames(Set)[colnames(Set) == 'V11'] =n
    party=createDataPartition(Set[,11],p=0.8,list=F,times=1)
    Set_train=Set[party,]
    Set_test=Set[-party,]
    
    fitControl=trainControl(method="repeatedcv",number = 10,repeats = 1)
    model1=na %>% train(data=Set_train,method="treebag",trControl=fitControl,linout=TRUE)
    #write.table(varImp(model1$finalModel,scale=TRUE),file = paste0(n,".varImp.txt"),sep = "\t")
    pred1=predict(model1,Set_test)
    Set_all=as.data.frame(cbind(pred1,Set_test[,11]))
    colnames(Set_all)=c("pred1",n)
    rv=cor.test(Set_all[,1],Set_all[,2])
    corre10=c(corre10,rv$estimate)
    pre10=c(pre10,rv[["p.value"]])
  }
  
  qiepian_s=Set[,colnames(Set) %in% RVP15]
  corre15=c()
  pre15=c()
  for(zx in 1:25){
    i=2
    n=colnames(matr_s)[i]
    na=formula(paste0(n,"~."))
    Set=qiepian_s
    Set[,16]=matr_s[,i]
    colnames(Set)[colnames(Set) == 'V16'] =n
    party=createDataPartition(Set[,16],p=0.8,list=F,times=1)
    Set_train=Set[party,]
    Set_test=Set[-party,]
    
    fitControl=trainControl(method="repeatedcv",number = 10,repeats = 1)
    model1=na %>% train(data=Set_train,method="treebag",trControl=fitControl,linout=TRUE)
    #write.table(varImp(model1$finalModel,scale=TRUE),file = paste0(n,".varImp.txt"),sep = "\t")
    pred1=predict(model1,Set_test)
    Set_all=as.data.frame(cbind(pred1,Set_test[,16]))
    colnames(Set_all)=c("pred1",n)
    rv=cor.test(Set_all[,1],Set_all[,2])
    corre15=c(corre15,rv$estimate)
    pre15=c(pre15,rv[["p.value"]])
  }
  
  qiepian_s=Set[,colnames(Set) %in% RVP20]
  corre20=c()
  pre20=c()
  for(zx in 1:25){
    i=2
    n=colnames(matr_s)[i]
    na=formula(paste0(n,"~."))
    Set=qiepian_s
    Set[,21]=matr_s[,i]
    colnames(Set)[colnames(Set) == 'V21'] =n
    party=createDataPartition(Set[,21],p=0.8,list=F,times=1)
    Set_train=Set[party,]
    Set_test=Set[-party,]
    
    fitControl=trainControl(method="repeatedcv",number = 10,repeats = 1)
    model1=na %>% train(data=Set_train,method="treebag",trControl=fitControl,linout=TRUE)
    #write.table(varImp(model1$finalModel,scale=TRUE),file = paste0(n,".varImp.txt"),sep = "\t")
    pred1=predict(model1,Set_test)
    Set_all=as.data.frame(cbind(pred1,Set_test[,21]))
    colnames(Set_all)=c("pred1",n)
    rv=cor.test(Set_all[,1],Set_all[,2])
    corre20=c(corre20,rv$estimate)
    pre20=c(pre20,rv[["p.value"]])
  }
  
  qiepian_s=Set[,colnames(Set) %in% RVP25]
  corre25=c()
  pre25=c()
  for(zx in 1:25){
    i=2
    n=colnames(matr_s)[i]
    na=formula(paste0(n,"~."))
    Set=qiepian_s
    Set[,26]=matr_s[,i]
    colnames(Set)[colnames(Set) == 'V26'] =n
    party=createDataPartition(Set[,26],p=0.8,list=F,times=1)
    Set_train=Set[party,]
    Set_test=Set[-party,]
    
    fitControl=trainControl(method="repeatedcv",number = 10,repeats = 1)
    model1=na %>% train(data=Set_train,method="treebag",trControl=fitControl,linout=TRUE)
    #write.table(varImp(model1$finalModel,scale=TRUE),file = paste0(n,".varImp.txt"),sep = "\t")
    pred1=predict(model1,Set_test)
    Set_all=as.data.frame(cbind(pred1,Set_test[,26]))
    colnames(Set_all)=c("pred1",n)
    rv=cor.test(Set_all[,1],Set_all[,2])
    corre25=c(corre25,rv$estimate)
    pre25=c(pre25,rv[["p.value"]])
  }
  
  qiepian_s=Set[,colnames(Set) %in% RVP30]
  corre30=c()
  pre30=c()
  for(zx in 1:25){
    i=2
    n=colnames(matr_s)[i]
    na=formula(paste0(n,"~."))
    Set=qiepian_s
    Set[,31]=matr_s[,i]
    colnames(Set)[colnames(Set) == 'V31'] =n
    party=createDataPartition(Set[,31],p=0.8,list=F,times=1)
    Set_train=Set[party,]
    Set_test=Set[-party,]
    
    fitControl=trainControl(method="repeatedcv",number = 10,repeats = 1)
    model1=na %>% train(data=Set_train,method="treebag",trControl=fitControl,linout=TRUE)
    #write.table(varImp(model1$finalModel,scale=TRUE),file = paste0(n,".varImp.txt"),sep = "\t")
    pred1=predict(model1,Set_test)
    Set_all=as.data.frame(cbind(pred1,Set_test[,31]))
    colnames(Set_all)=c("pred1",n)
    rv=cor.test(Set_all[,1],Set_all[,2])
    corre30=c(corre30,rv$estimate)
    pre30=c(pre30,rv[["p.value"]])
  }
  
  qiepian_s=Set[,colnames(Set) %in% RVP35]
  corre35=c()
  pre35=c()
  for(zx in 1:25){
    i=2
    n=colnames(matr_s)[i]
    na=formula(paste0(n,"~."))
    Set=qiepian_s
    Set[,36]=matr_s[,i]
    colnames(Set)[colnames(Set) == 'V36'] =n
    party=createDataPartition(Set[,36],p=0.8,list=F,times=1)
    Set_train=Set[party,]
    Set_test=Set[-party,]
    
    fitControl=trainControl(method="repeatedcv",number = 10,repeats = 1)
    model1=na %>% train(data=Set_train,method="treebag",trControl=fitControl,linout=TRUE)
    #write.table(varImp(model1$finalModel,scale=TRUE),file = paste0(n,".varImp.txt"),sep = "\t")
    pred1=predict(model1,Set_test)
    Set_all=as.data.frame(cbind(pred1,Set_test[,36]))
    colnames(Set_all)=c("pred1",n)
    rv=cor.test(Set_all[,1],Set_all[,2])
    corre35=c(corre35,rv$estimate)
    pre35=c(pre35,rv[["p.value"]])
  }
}

c5=corre5[!corre5 %in% (c(max(corre5),min(corre5)))]
c10=corre10[!corre10 %in% (c(max(corre10),min(corre10)))]
c15=corre15[!corre15 %in% (c(max(corre15),min(corre15)))]
c20=corre20[!corre20 %in% (c(max(corre20),min(corre20)))]
c25=corre25[!corre25 %in% (c(max(corre25),min(corre25)))]
c30=corre30[!corre30 %in% (c(max(corre30),min(corre30)))]
c35=corre35[!corre35 %in% (c(max(corre35),min(corre35)))]

cor_mtr=rbind(c5,c10,c15,c20,c25,c30,c35)
colnames(cor_mtr)=c(paste0(rep("rep",23),1:23))
rownames(cor_mtr)=c(5,10,15,20,25,30,35)
cor_mtr=as.data.frame(cor_mtr)
RAC=apply(cor_mtr[1:7,1:23],1,function(x){
  std.error(x)
})
cor_mtr$SE=RAC
cor_mtr$mean=rowMeans(cor_mtr[,1:23])
cor_mtr$head=rownames(cor_mtr)
cor_mtr$head=factor(cor_mtr$head,levels = c(5,10,15,20,25,30,35))
RVP_bar=ggplot(cor_mtr,aes(x=head,y=mean))+
  geom_bar(position=position_dodge(), stat="identity",fill="#8DD3C7")+
  geom_line(group=1,color="#FB8072",size=1)+
  geom_point(size=0.1)+
  geom_errorbar(aes(ymin=mean,ymax=mean+SE),
                width=.9, # 设置误差线的宽度 
                position=position_dodge(.9))
write.table(cor_mtr,file="RVP_rep25_filter.txt",quote = F,col.names = TRUE)
p=as.data.frame(rbind(pre5,pre10,pre15,pre20,pre25,pre30,pre35))
colnames(p)=c(paste0(rep("rep",25),1:25))
rownames(p)=c(5,10,15,20,25,30,35)
cor=as.data.frame(rbind(corre5,corre10,corre15,corre20,corre25,corre30,corre35))
colnames(cor)=c(paste0(rep("rep",25),1:25))
rownames(cor)=c(5,10,15,20,25,30,35)
write.table(p,file="RVP_rep25_p.txt",quote = F,col.names = TRUE)
write.table(cor,file="RVP_rep25_cor.txt",quote = F,col.names = TRUE)
ggsave(RVP_bar,file="RVP_bar_rep25.pdf",width=6.5,height=4)



####################################################画预测差异度
library(dplyr)
library(RColorBrewer)
library(ggplot2)
display.brewer.all()
mycol<- brewer.pal(n=4,name="Blues")
RVP=read.table("RootVolume_PR_predictData.txt")
RVP$chayi=((RVP$pred2-RVP$RootVolume)/(RVP$RootVolume+RVP$pred2))*100
tmp=rev(order(RVP$chayi))
RVP=RVP[tmp,]
RVcount=sum(between(abs(RVP$chayi),0,10))
RVcount[2]=sum(between(abs(RVP$chayi),10,25))
RVcount[3]=sum(between(abs(RVP$chayi),25,50))
RVindex=c(10,25,50)
RVsum=as.data.frame(cbind(RVcount,RVindex))
RVsum$RVindex=factor(RVsum$RVindex)
RVP_diff=ggplot(RVsum,aes(x=RVindex,y=RVcount))+geom_bar(position=position_dodge(), stat="identity",fill="#6BAED6",width=0.7)
ggsave(RVP_diff,file="RVP_diff.pdf",width=6.5,height=4)

PAP=read.table("ProjArea_predictData.txt")
PAP$chayi=((PAP$pred2-PAP$ProjArea)/(PAP$ProjArea+PAP$pred2))*100
tmp=rev(order(PAP$chayi))
PAP=PAP[tmp,]
PAPcount=sum(between(abs(PAP$chayi),0,10))
PAPcount[2]=sum(between(abs(PAP$chayi),10,25))
PAPcount[3]=sum(between(abs(PAP$chayi),25,50))
PAPindex=c(10,25,50)
PAPsum=as.data.frame(cbind(PAPcount,PAPindex))
PAPsum$PAPindex=factor(PAPsum$PAPindex)
PA_diff=ggplot(PAPsum,aes(x=PAPindex,y=PAPcount))+geom_bar(position=position_dodge(), stat="identity",fill="#6BAED6",width=0.7)
ggsave(PA_diff,file="PA_diff.pdf",width=6.5,height=4)


