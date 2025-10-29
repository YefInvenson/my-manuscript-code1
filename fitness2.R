rm(list = ls())
#====加载包====
library(readxl)
library(writexl)
library(ggplot2)
library(ggarrow)
library(ggrepel)#绘图包
library(ggpubr)#边际图
library(ggridges)
library(ggsignif)
library(ggtext)
library(ggforce)
library(grid)
library(gridExtra)#图像拼接包
library(ggbeeswarm)#蜂群图
library(RColorBrewer)#色彩配置包
library(fitnic)
library(RColorBrewer)
library(vctrs)
library(dplyr)
#You must library the "vctrs" namespace, otherwise you can't handle the data with "tbl_df" form.
#必须加载Vctrs命名空间，否则无法处理“tbl_df”类型的数据
library(vegan)#冗余分析数据分析包
library(linkET)#数据分析包：mantel分析
library(tidyverse)
library(rstatix)
library(fmsb)#雷达图包
#library(ggExtra)#边际图
library(cowplot)
library(randomForest)#
library(car)#检查方差齐性
library(broom)#简化包

fun0<-read_excel("C:/Users/Yef0/Desktop/study/data_unprocessed/Fertilizing_and_Mowing/fun_actually.xlsx")
plot0<-read_excel("C:/Users/Yef0/Desktop/study/data_unprocessed/Fertilizing_and_Mowing/plot.xlsx")
fun0<-data.frame(fun0)
plot0<-data.frame(plot0)


#====数据拼接:fun1:基础叶片性状与群落生态参数====
k=0
fun1<-data.frame()
plot1<-data.frame()
for(i in 1:dim(fun0)[1]){
  for(j in 1:dim(plot0)[1]){
    if(fun0[i,1]==plot0[j,1]){
      if(fun0[i,2]==plot0[j,2]){
        if(fun0[i,3]==plot0[j,3]){
            k=k+1
            d=dim(fun0)[2]
            dd=dim(plot0)[2]
            fun1[k,1:d]=fun0[i,1:d]
            fun1[k,d+1]=plot0[j,7]
            fun1[k,d+2]=plot0[j,8]
            fun1[k,d+3]=plot0[j,11]
            fun1[k,d+4]=plot0[j,12]
            plot1[k,1:dd]=plot0[j,1:dd]
        }
      }
    }
  }
}




#===fun2:基础个体叶性状与个体生态参数====
fun1<-data.frame(fun1)
leaf_n<-data.frame()
for(i in 1:dim(fun1)[1]){
  bb=numeric(fun1[i,17])
  
  leaf_n[i,1:3]=fun1[i,1:3]
  leaf_n[i,4]=fun1[i,17]/fun1[i,16]#个体平均生物量
  leaf_n[i,5]=0.35+0.19/(1+leaf_n[i,4]**2)#个体叶片数系数
  leaf_n[i,6]=ceiling(leaf_n[i,5]*leaf_n[i,4]/fun1[i,4])+1#个体叶片数
}

k=0
fun2<-data.frame()
for(i in 1:dim(fun0)[1]){
  for(j in 1:dim(plot0)[1]){
    if(fun0[i,1]==plot0[j,1]){
      if(fun0[i,2]==plot0[j,2]){
        if(fun0[i,3]==plot0[j,3]){
          k=k+1
          d=dim(fun0)[2]
          fun2[k,1:3]=fun0[i,1:3]
          fun2[k,4:5]=fun0[i,4:5]
          fun2[k,6:d]=fun0[i,6:d]
          fun2[k,d+1]=leaf_n[i,6]
          fun2[k,d+2]=plot0[j,7]/plot0[j,11]
          fun2[k,d+3]=plot0[j,8]*0.6+plot0[j,9]*0.3+plot0[j,8]*0.1
          fun2[k,d+4]=plot0[j,11]
          fun2[k,d+5]=plot0[j,12]/plot0[j,11]
          fun2[k,d+6]=(fun1[i,14]*fun1[i,15])/fun1[i,16]
        }
      }
    }
  }
}
fun_name<-colnames(fun1[,1:13])
colnames(fun2)<-c(fun_name,"LN","cov","H","abu","bio","ispa")
rm(fun_name)
#====
plot0<-read_excel("C:/Users/Yef0/Desktop/study/data_unprocessed/Fertilizing_and_Mowing/plot22.xlsx")
plot0<-data.frame(plot0)
pl1<-plot0[,2:4]
av_bio<-plot0[,8]/plot0[,7]
pl1<-cbind(pl1,av_bio)
mean_plot<- aggregate(pl1[,4], by=list(pl1$spname, pl1$mow,pl1$fer), FUN=mean)
for(i in 1:dim(mean_plot)[1]){
  if(mean_plot[i,3]==0){
    mean_plot[i,3]="00"
  }else if(mean_plot[i,3]==5){
    mean_plot[i,3]="05"
  }else if(mean_plot[i,3]==10){
    mean_plot[i,3]="10"
  }else if(mean_plot[i,3]==20){
    mean_plot[i,3]="20"
  }else if(mean_plot[i,3]==40){
    mean_plot[i,3]="40"
  }else if(mean_plot[i,3]==80){
    mean_plot[i,3]="80"
  }
}
for(i in 1:dim(fun2)[1]){
  for(j in 1:dim(mean_plot)[1]){
    if(fun2[i,1]==mean_plot[j,1]){
      if(fun2[i,2]==mean_plot[j,3]){
        if(fun2[i,3]==mean_plot[j,2]){
          fun2[i,14]=ceiling(leaf_n[i,5]*mean_plot[j,4]/fun2[i,4])+1
        }
      }
    }
  }
}


fun0<-read_excel("C:/Users/Yef0/Desktop/study/data_unprocessed/Fertilizing_and_Mowing/fun_actually_total.xlsx")
fun0<-data.frame(fun0)
df<-fun0[,7:12]
rm_fun1<-f_inter1(df)
rm_fun2<-f_inter2(df)
rm_fun1<-cbind(fun0[,1:3],rm_fun1)
rm_fun2<-cbind(fun0[,1:3],rm_fun2)
life<-read_excel("C:/Users/Yef0/Desktop/study/data_unprocessed/Fertilizing_and_Mowing/life.xlsx")
life<-data.frame(life)

#===fun3:基础叶片性状及其交互作用1====
fun3<-data.frame()
df=rm_fun1
fun3<- aggregate(df[,4:18], by=list(df$name, df$mow,df$fer), FUN=mean)
#===fun4:基础叶片性状及其交互作用2====
fun4<-data.frame()
df=rm_fun2
fun4<- aggregate(df[,4:18], by=list(df$name, df$mow,df$fer), FUN=mean)

LN<-leaf_n[1:dim(leaf_n)[1],6]

#回归系数替代导数值
#====随机森林====
rsq_rfm<-data.frame(mean_rsq=numeric(),max_rsq=numeric())
ave_rsq<-data.frame()
max_rsq<-data.frame()

d=12
en<-fun1[,14]

rm_data<-cbind(fun3[,4:18],en)
for(j in 1:d){
  for(i in 1:10){
    set.seed(i) # 设置随机种子以确保结果可重复
    rfModel <- randomForest(rm_data$en~ ., data = rm_data, ntree = 10000, mtry = j)
    predictions <- predict(rfModel, newdata = rm_data)
    accuracy <- sum(predictions == rm_data$en) / nrow(rm_data)
    
    confusionMatrix <- table(predictions, rm_data$en)
    ave_rsq[i,j]<-mean(rfModel$rsq)
    max_rsq[i,j]<-max(rfModel$rsq)
  }
}
for(i in 1:d){
  rsq_rfm[i,1]<-mean(ave_rsq[,i])
  rsq_rfm[i,2]<-max(max_rsq[,i])
}
rsq_rfm
#查看因子贡献 
importance(rfModel)
varImpPlot(rfModel)#因子贡献图
#====遴选====


WNP<-fun3$NN_RWC_1
LWD<-fun3$LA_RWC_1
LAC<-fun3$LA_CC_1
WRR<-fun4$RWC_CC_2
SLA<-fun4$LA_DW_2
RCN<-fun4$NN_CC_2
FT<-cbind(fun2[,1:9],WNP,LWD,LAC,WRR,SLA,RCN)
#====热图====

varespec=data.frame(fun2[,15:17])#因变量
varechem=cbind(FT[,4:15])
mantel <- mantel_test(varespec, varechem,
                      spec_select = list(Coverage=1,
                                         #Blade_number = 1,
                                         Height=2,
                                         #relative = 13:15,
                                         #ispace = 21
                                         Abundance=3)) %>%
  dplyr::mutate(rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),
                         labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),
                pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
                         labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")));
mantel

correlate(varechem) %>%
  qcorrplot(type = "lower", diag = T) +
  geom_square() +
  geom_couple(aes(colour = pd, size = rd), data = mantel, curvature = 0.1) +
  geom_mark(sep = '\n',size = 4, sig_level = c(0.05, 0.01, 0.001),
            sig_thres = 0.05, color = 'black',
  ) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "RdBu")) +
  scale_size_manual(values = c(0.5, 1, 2)) +
  scale_colour_manual(values = color_pal(3)) +
  labs(fill = "Pearson's correlation",
       size = "Mantel's r value",
       colour = "Mantel's p value")+
  theme(
    text = element_text(size = 14, family = "serif"),
    plot.title = element_text(size = 14, colour = "black", hjust = 0.5),
    legend.title = element_text(color = "black", size = 14),
    legend.text = element_text(color = "black", size = 14),
    axis.text.y = element_text(size = 14, color = "black", vjust = 0.5, hjust = 1, angle = 0),
    axis.text.x = element_text(size = 14, color = "black", vjust = 0.5, hjust = 0.5, angle = 0)
  )
#====冗余分析====
Fre<-plot1[,6]
imp<-plot1[,21]
HH=data.frame(fun2[,16])
colnames(HH)<-"HH"
LS<-life[,7]
SLA<-FT[,14]
NN<-FT[,8]
WNP<-FT[,10]
RWC<-FT[,6]
DW<-FT[,4]

fc1=cbind(HH,FT[,5:6],FT[,8:9],SLA,life[,4:7])###读取解释变量数据：对coverage
fc=cbind(HH,DW,RWC,NN,FT[,14:15],life[,4:5],LS)###读取解释变量数据：对Height

cc<-fun2[,15]*100
hh<-fun2[,16]

sp1=cbind(cc,fun2[,17:18])
sp=cbind(hh,fun2[,17:18])
colnames(sp1)<-c("Coverage","Abundance","Biomass")
colnames(sp)<-c("Height","Abundance","Biomass")
zzz=cbind(fc1,sp1)
zzz=cbind(fc,sp)

spp1=decostand(sp1,method = "hellinger")###对响应变量做hellinger转化
fcc1=log10(fc1)###对解释变量取对数
spp=decostand(sp,method = "hellinger")###对响应变量做hellinger转化
fcc=log10(fc)###对解释变量取对数

###做冗余分析
uu=rda(spp1~.,fcc1)
vv=rda(spp~.,fcc)
ii=summary(vv,scaling = 2)###查看分析结果
#vv$tot.chi 总变异性
#vv$colsum  解释变量的解释贡献
#Vv$Ybar    响应变量的标准化拟合值
#====RDA====
sp=as.data.frame(ii$species[,1:2],LN)###提取相应变量坐标，乘以5是使图美观，不影响分析
st=as.data.frame(ii$sites[,1:2],LN)###提取样方坐标，
st=cbind(st,fun2[,2:3])
for(i in 1:dim(st)[1]){
  if(st[i,4]==0){
    st[i,4]="No mowing"
  }else if(st[i,4]==1){
    st[i,4]="Mowing once"
  }else if(st[i,4]==2){
    st[i,4]="Mowing twice"
  }
}
yz=as.data.frame(ii$biplot[,1:2])###提取解释变量坐标
   
#
ggplot() +
  geom_point(data = st,aes(x=RDA1,y=RDA2,color=mow),size=1.2,alpha=0.8)+
  color_palette("jco")+
  geom_segment(data = sp,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
               arrow = arrow(angle=22.5,length = unit(0.25,"cm"),
                             type = "closed"),linetype=2, linewidth=1,alpha=0.6,colour = "#228B22")+
  geom_text_repel(data = sp,aes(RDA1,RDA2,label=row.names(sp)),size=3,colour="#228B22")+
  geom_segment(data = yz,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
               arrow = arrow(angle=22.5,length = unit(0.15,"cm"),
                             type = "closed"),linetype=1, linewidth=1,colour = "black")+
  geom_text_repel(data = yz,aes(RDA1,RDA2,label=row.names(yz)),size=3.5,colour="black")+
  labs(x="RDA1  54.12%",y="RDA2  8.83%")+
  #labs(x="RDA1  55.11%",y="RDA2  5.10%")+
  geom_hline(yintercept=0,linetype=3,linewidth=1) + 
  geom_vline(xintercept=0,linetype=3,linewidth=1)+
  theme_bw()+
  theme( panel.grid=element_blank(),
        plot.margin = unit(c(1, 0.5, 0.5, 0.5), "lines",),
        axis.title = element_text(family = "serif", size = 16),  # 设置轴标题字体
        axis.text = element_text(family = "serif", size = 14),  # 设置轴刻度文字字体
        legend.title = element_text(family = "serif", size = 14),  # 设置图例标题字体
        legend.text = element_text(family = "serif", size = 14))+
  coord_cartesian(xlim = c(-0.5, 0.65), ylim = c(-0.65, 0.45))
  #coord_cartesian(xlim = c(-0.7, 0.65), ylim = c(-0.72, 0.45))


#====CCA====
# fc = cbind(HH, DW, RWC, NN, FT[, 14:15], life[, 4:5], LS, LN)###读取解释变量数据：对Height
# sp = cbind(hh, fun2[, 17:19])
# colnames(sp) <- c("Height", "Abundance", "Biomass", "Space")
# zzz = cbind(fc, sp)
# spp = decostand(sp, method = "hellinger")###对响应变量做hellinger转化
# fcc = log10(fc)###对解释变量取对数
# uu = cca(spp ~ ., fcc)
# ii = summary(uu, scaling = 2)###查看分析结果
# #uu$tot.chi 总变异性
# #uu$colsum  解释变量的解释贡献
# #uu$Ybar    响应变量的标准化拟合值
# sp = as.data.frame(ii$species[, 1:2]) * 2###提取相应变量坐标，乘以5是使图美观，不影响分析
# yz = as.data.frame(ii$biplot[, 1:2]) * 2###提取解释变量坐标
# st = as.data.frame(ii$sites[, 1:2])###提取样方坐标，
# st = cbind(st, fun2[, 2:3])
# for (i in 1:dim(st)[1]) {
#   if (st[i, 4] == 0) {
#     st[i, 4] = "No mowing"
#   } else if (st[i, 4] == 1) {
#     st[i, 4] = "Mowing once"
#   } else if (st[i, 4] == 2) {
#     st[i, 4] = "Mowing twice"
#   }
# }
# 
# #
# ggplot() +
#   geom_point(
#     data = st,
#     aes(x = CCA1, y = CCA2, color = mow),
#     size = 2,
#     alpha = 0.8
#   ) +
#   color_palette("jco") +
#   geom_segment(
#     data = sp,
#     aes(
#       x = 0,
#       y = 0,
#       xend = CCA1,
#       yend = CCA2
#     ),
#     arrow = arrow(
#       angle = 22.5,
#       length = unit(0.25, "cm"),
#       type = "closed"
#     ),
#     linetype = 2,
#     linewidth = 1,
#     alpha = 0.6,
#     colour = "#228B22"
#   ) +
#   geom_text_repel(
#     data = sp,
#     aes(CCA1, CCA2, label = row.names(sp)),
#     size = 2.5,
#     colour = "#228B22"
#   ) +
#   geom_segment(
#     data = yz,
#     aes(
#       x = 0,
#       y = 0,
#       xend = CCA1,
#       yend = CCA2
#     ),
#     arrow = arrow(
#       angle = 22.5,
#       length = unit(0.15, "cm"),
#       type = "closed"
#     ),
#     linetype = 1,
#     linewidth = 1,
#     colour = "black"
#   ) +
#   geom_text_repel(
#     data = yz,
#     aes(CCA1, CCA2, label = row.names(yz)),
#     size = 3,
#     colour = "black"
#   ) +
#   labs(x = "CCA1  74.20%", y = "CCA2  0.03%") +
#   geom_hline(yintercept = 0,
#              linetype = 3,
#              linewidth = 1) +
#   geom_vline(xintercept = 0,
#              linetype = 3,
#              linewidth = 1) +
#   theme_bw() +
#   theme(
#     panel.grid = element_blank(),
#     plot.margin = unit(c(1, 0.5, 0.5, 0.5), "lines", ),
#     axis.title = element_text(family = "serif", size = 12),
#     # 设置轴标题字体
#     axis.text = element_text(family = "serif", size = 10),
#     # 设置轴刻度文字字体
#     legend.title = element_text(family = "serif", size = 12),
#     # 设置图例标题字体
#     legend.text = element_text(family = "serif", size = 10)
#   ) +
#   coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2))
#====VIF====
#Variance Inflation Factor，VIF方差膨胀因子
uc<-vif.cca(uu)
vifname<-names(uc)
ucc<-rbind(vifname,uc)
ucc<-data.frame(t(ucc[1:2,1:dim(ucc)[2]]))

ucc$uc<-as.numeric(as.character(ucc$uc))
ucc$vifname<- factor(ucc$vifname, levels = ucc$vifname[order(ucc$uc)])
ucc$uc<-round(ucc$uc, digits = 1)
ggplot(ucc, aes(x = vifname, y = uc)) +
  geom_point(size = 4, color = "#2E8A57") +  # 使用点图
  coord_flip() +  # 翻转坐标轴，使点图横向显示
  labs( x = "Traits", y = "VIF") +
  theme_minimal()+ # 使用简洁的主题
  theme( plot.margin = unit(c(1, 0.5, 0.5, 0.5), "lines",),
         axis.title = element_text(family = "serif", size = 12),  # 设置轴标题字体
         axis.text = element_text(family = "serif", size = 10),  # 设置轴刻度文字字体
         legend.title = element_text(family = "serif", size = 12),  # 设置图例标题字体
         legend.text = element_text(family = "serif", size = 10))

#====适合度计算====
#提取主轴====
score1 <- scores(uu)
score2 <- scores(vv)
RDA_1<-score1[["biplot"]][,1]
RDA_2<-score2[["biplot"]][,1]
rda1<-data.frame()
rda2<-data.frame()
rda1[1:dim(fun1)[1],1:3]=fun1[1:dim(fun1)[1],1:3]
rda2[1:dim(fun1)[1],1:3]=fun1[1:dim(fun1)[1],1:3]
rda1[,4]=0
rda2[,4]=0
for(i in 1:dim(fun1)[1]){
  for(j in 1:dim(fc)[2]){
    rda1[i,4]=RDA_1[j]*fc1[i,j]+rda1[i,4]
    rda2[i,4]=RDA_2[j]*fc[i,j]+rda2[i,4]
  }
}
#
dbdy=rda1
dbdx=rda2



#拟合曲线====
pp=cbind(fun2[,1:3],fun2[15:18],rda1[,4],rda2[,4])

colnames(pp)<-c("Species","fer","mow","Coverage","Height","Abundance","Biomass","dbdy","dbdx")
pp$mow<-as.character(pp$mow)
ff<-f_curve(5,8,pp)
ff
sig=round(ff$Rsquare,digits=4)
ggplot(pp,aes(y=dbdy,x=Height,fill=mow,color=mow,shape=mow))+
#ggplot(pp,aes(y=dbdx,x=log(Coverage),color=mow,shape=mow))+
  geom_point(aes(shape=mow,
                 fill=mow),
             stroke = 1,
             size=2,
             alpha=0.8) +
  fill_palette("jco")+
  geom_smooth(method=lm,se = FALSE, linetype = "solid", linewidth = 1.2) + 
  color_palette("jco")+# 添加线性拟合曲线+
  annotate(
    "text", 
    x = Inf,         # x坐标设为最大值（右侧边界）
    y = Inf,         # y坐标设为最大值（顶部边界）
    label = paste("R-squre: ",sig*100,"%"),  # 文本内容
    hjust = 1.1,     #                                                                                                                                                                                                                                     水平对齐：右对齐（向左侧偏移10%）
    vjust = 1.1,     # 垂直对齐：上对齐（向下方偏移10%）
    color = "#333333",  # 文本颜色
    size = 4,          # 字体大小
    family="serif"
  )+
  labs(x="Height (cm)",y="IFH")+
  #labs(y="IFC",x="log(Coverage)")+
  theme_minimal()+
  theme(plot.margin = unit(c(1, 1, 0, 1), "lines",),
        plot.title = element_text(family = "serif", size = 16, face = "bold"),  # 设置标题字体
        plot.subtitle = element_text(family = "serif", size = 12),  # 设置副标题字体
        axis.title = element_text(family = "serif", size = 12),  # 设置轴标题字体
        axis.text = element_text(family = "serif", size = 12),  # 设置轴刻度文字字体
        legend.title = element_text(family = "serif", size = 12),  # 设置图例标题字体
        legend.text = element_text(family = "serif", size = 12)   # 设置图例文字字体
        )

#适合度====
pp=cbind(fun1[,1:3],fun1[14:17],rda1[,4],rda2[,4])
ppp<-f_stand(pp[,4:dim(pp)[2]])
pp<-cbind(fun1[,1:3],ppp)
coe_dudx<-f_curve(4,9,pp)
coe_dNdx<-f_curve(4,6,pp)
coe_dvdy<-f_curve(5,8,pp)
coe_dNdy<-f_curve(5,6,pp)
k=0
dudx<-numeric()
dNdx<-numeric()
dvdy<-numeric()
dNdy<-numeric()
ff<-data.frame()

for(i in 1:dim(pp)[1]){
  if(pp[i,2]=="00"){
    pp[i,2]=0
  }else if(pp[i,2]=="05"){
    pp[i,2]=5
  }else if(pp[i,2]=="10"){
    pp[i,2]=10
  }else if(pp[i,2]=="20"){
    pp[i,2]=20
  }else if(pp[i,2]=="40"){
    pp[i,2]=40
  }else if(pp[i,2]=="80"){
    pp[i,2]=80
  }
}
plot0[,13:16]<-f_stand(plot0[,13:16])
for(i in 1:dim(plot0)[1]){
  for(j in 1:dim(pp)[1]){
    if(plot0[i,2]==pp[j,1]){
      if(plot0[i,3]==pp[j,2]){
        if(plot0[i,4]==pp[j,3]){
          k=k+1
          ff[k,1:4]=plot0[i,1:4]
          #====方向导数为负
          dudx[k]=f_dydx(coe_dudx,plot0[i,13])
          ff[k,5]=(plot0[i,15]*dudx[k])
          dNdx[k]=2*f_dydx(coe_dNdx,plot0[i,13])
          ff[k,6]=(pp[j,9]*dNdx[k])
          dvdy[k]=f_dydx(coe_dvdy,plot0[i,14])
          ff[k,7]=(plot0[i,15]*dvdy[k])
          dNdy[k]=2*f_dydx(coe_dNdy,plot0[i,14])
          ff[k,8]=(pp[j,8]*dNdy[k])
        }
      }
    }
  }
}
write_xlsx(ff,"C:/Users/Yef0/Desktop/study/data_unprocessed/Fertilizing_and_Mowing/spacial_change.xlsx")
#====多样性指数====
#===验证对比：主要物种的平均相对盖度和平均相对高度===
ff_y<-data.frame()
k=0
plot0[,13:16]<-f_stand(plot0[,13:16])
for(i in 1:dim(plot0)[1]){
  for(j in 1:dim(pp)[1]){
    if(plot0[i,2]==pp[j,1]){
      if(plot0[i,3]==pp[j,2]){
        if(plot0[i,4]==pp[j,3]){
          k=k+1
          ff_y[k,1:4]=plot0[i,1:4]
          ff_y[k,5]=plot0[i,13]
          ff_y[k,6]=plot0[i,14]
          ff_y[k,7]=plot0[i,15]
        }
      }
    }
  }
}
#===多样性计算
sfm<-data.frame()
i_simpson<-data.frame()
i_shannon<-data.frame()
i_pielou<-data.frame()
i_berger<-data.frame()
i_marglef<-data.frame()
l=0
fitness<-data.frame()
fd<-data.frame()

f3_y<-data.frame()
for(i in 1:6){
  d1=f_sel2(1,i,plot0)
  e1=f_sel2(1,i,ff)
  f1=f_sel2(1,i,ff_y)
  for(j in 1:3){
    d2=f_sel2(4,j-1,d1)
    e2=f_sel2(4,j-1,e1)
    f2=f_sel2(4,j-1,f1)
    for(k in 1:6){
      if(k==1){
        d3=f_sel2(3,0,d2)
        e3=f_sel2(3,0,e2)
        f3=f_sel2(3,0,f2)
        l=l+1
        #===地块
        sfm[l,1]=d3[1,1]
        sfm[l,2:3]=d3[1,3:4]
        #===多样性指数
        i_simpson[l,1:3]=sfm[l,1:3]
        i_simpson[l,4]=1-sum(d3[,19])
        
        i_shannon[l,1:3]=sfm[l,1:3]
        i_shannon[l,4]=-sum(d3[,20])
        
        i_pielou[l,1:3]=sfm[l,1:3]
        i_pielou[l,4]=i_shannon[l,4]/log(dim(d3)[1])
        
        i_berger[l,1:3]=sfm[l,1:3]
        i_berger[l,4]=max(d3[,7])/sum(d3[,7])
        
        i_marglef[l,1:3]=sfm[l,1:3]
        i_marglef[l,4]=(dim(d3)[1]-1)/log(d3[1,11])
        #===适合度
        fitness[l,1:3]=sfm[l,1:3]
        
        fitness[l,4]=mean(e3[,5])
        fitness[l,5]=mean(e3[,6])
        fitness[l,6]=mean(e3[,7])
        fitness[l,7]=mean(e3[,8])
        #===验证
        f3_y[l,1:3]=sfm[l,1:3]
        f3_y[l,4]=mean(f3[,5])
        f3_y[l,5]=mean(f3[,6])
        f3_y[l,6]=mean(f3[,7])
        
      }else{
        d3=f_sel2(3,5*2^(k-2),d2)
        e3=f_sel2(3,5*2^(k-2),e2)
        f3=f_sel2(3,5*2^(k-2),f2)
        l=l+1
        #===地块
        sfm[l,1]=d3[1,1]
        sfm[l,2:3]=d3[1,3:4]
        #===多样性指数
        i_simpson[l,1:3]=sfm[l,1:3]
        i_simpson[l,4]=1-sum(d3[,19])
        
        i_shannon[l,1:3]=sfm[l,1:3]
        i_shannon[l,4]=-sum(d3[,20])
        
        i_pielou[l,1:3]=sfm[l,1:3]
        i_pielou[l,4]=i_shannon[l,4]/log(dim(d3)[1])
        
        i_berger[l,1:3]=sfm[l,1:3]
        i_berger[l,4]=max(d3[,7])/sum(d3[,7])
        
        i_marglef[l,1:3]=sfm[l,1:3]
        i_marglef[l,4]=(dim(d3)[1]-1)/log(d3[1,11])
        #===适合度
        fitness[l,1:3]=sfm[l,1:3]
        
        fitness[l,4]=mean(e3[,5])
        fitness[l,5]=mean(e3[,6])
        fitness[l,6]=mean(e3[,7])
        fitness[l,7]=mean(e3[,8])
        #===验证
        f3_y[l,1:3]=sfm[l,1:3]
        f3_y[l,4]=mean(f3[,5])
        f3_y[l,5]=mean(f3[,6])
        f3_y[l,6]=mean(f3[,7])
      }
    }
  }
}
f3_y=cbind(f3_y[,1:6],i_marglef[,4])
fitness[,8]=fitness[,4]+fitness[,5]
fitness[,9]=fitness[,6]+fitness[,7]
fitness[,10]=fitness[,8]+fitness[,9]
l=0
l=0
#绝对差异====
for(i in 1:6){
  d1=f_sel2(1,i,plot0)
  e1=f_sel2(1,i,ff)
  for(j in 1:3){
    d2=f_sel2(4,j-1,d1)
    e2=f_sel2(4,j-1,e1)
    for(k in 1:6){
      if(k==1){
        d3=f_sel2(3,0,d2)
        e3=f_sel2(3,0,e2)
        
        l=l+1
        sfm[l,1]=d3[1,1]
        sfm[l,2:3]=d3[1,3:4]
        fd[l,1:3]=sfm[l,1:3]
        
        fd[l,4]=mean(e3[,5])+mean(e3[,6])
        fd[l,5]=mean(e3[,7])+mean(e3[,8])
        fd[l,6]=mean(abs(e3[,5]+e3[,6]-fd[l,4]))
        fd[l,7]=mean(abs(e3[,7]+e3[,8]-fd[l,5]))
        fd[l,8]=mean(abs(e3[,5]+e3[,6]+e3[,7]+e3[,8]-fd[l,4]-fd[l,5]))
        
      }else{
        d3=f_sel2(3,5*2^(k-2),d2)
        e3=f_sel2(3,5*2^(k-2),e2)
        l=l+1
        sfm[l,1]=d3[1,1]
        sfm[l,2:3]=d3[1,3:4]
        
        fd[l,1:3]=sfm[l,1:3]
        
        fd[l,4]=mean(e3[,5])+mean(e3[,6])
        fd[l,5]=mean(e3[,7])+mean(e3[,8])
        fd[l,6]=mean(abs(e3[,5]+e3[,6]-fd[l,4]))
        fd[l,7]=mean(abs(e3[,7]+e3[,8]-fd[l,5]))
        fd[l,8]=mean(abs(e3[,5]+e3[,6]+e3[,7]+e3[,8]-fd[l,4]-fd[l,5]))
      }
    }
  }
}

#====边际图====
#===分析====

fff<-cbind(fitness[,1:10],i_simpson[,4],i_shannon[,4],i_pielou[,4],i_berger[,4],i_marglef[,4])
fd<-cbind(fd[,1:3],fd[,6:8],fff[,11:15])
colnames(fd)<-c("repeat","fer","mow","fitness_x","fitness_y","fitness","simpson","shannon","pielou","berger","marglef")
colnames(fff)<-c("repeat","fer","mow","Ndudx","dNdxu","Ndvdy","dNdyv","fitness_x","fitness_y","fitness","simpson","shannon","pielou","berger","marglef")
colnames(f3_y)<-c("V1","fer","mow","fitness_x","fitness_y","fitness","marglef")
plot(x=fd$fitness_y,y=fd$berger)
#write_xlsx(fff,"C:/Users/Yef0/Desktop/study/data_unprocessed/Fertilizing_and_Mowing/fitness_biodiv.xlsx")
# Main plot
for(i in 1:dim(fff)[1]){
  if(fff[i,3]==0){
    fd[i,3]="No mowing"
    fff[i,3]="No mowing"
  }else if(fff[i,3]==1){
    fd[i,3]="Mowing once"
    fff[i,3]="Mowing once"
  }else if(fff[i,3]==2){
    fd[i,3]="Mowing twice"
    fff[i,3]="Mowing twice"
  }
}
for(i in 1:dim(fff)[1]){
  if(fff[i,2]==0){
    fd[i,2]="Low"
    fff[i,2]="Low"
  }else if(fff[i,2]==5){
    fd[i,2]="Low"
    fff[i,2]="Low"
  }else if(fff[i,2]==10){
    fd[i,2]="Moderate"
    fff[i,2]="Moderate"
  }else if(fff[i,2]==20){
    fd[i,2]="Moderate"
    fff[i,2]="Moderate"
  }else if(fff[i,2]==40){
    fd[i,2]="High"
    fff[i,2]="High"
  }else if(fff[i,2]==80){
    fd[i,2]="High"
    fff[i,2]="High"
  }
}
#===选择绘制fff====
fitness<-fff

fff=fitness
aov1<-aov(marglef~fitness_x,data=fff)
aa1<-summary(aov1)
pv1<-aa1[[1]]$`Pr(>F)`[1]
rr1<-cor.test(fff$shannon, fff$fitness_x, method = "pearson")
r1=round(rr1[[4]],2)

aov2<-aov(marglef~fitness_y,data=fff)
aa2<-summary(aov2)
pv2<-aa2[[1]]$`Pr(>F)`[1]
rr2<-cor.test(fff$shannon, fff$fitness_y, method = "pearson")
r2=round(rr2[[4]],2)

#===绘图====
pmain<-ggplot(fff, aes(x = fitness_x, y = fitness_y,fill=fer,color=fer)) +
  #geom_point()+
  geom_point(color="black",
             shape = 21, 
             stroke = 0.5,
             aes(size=shannon),alpha=0.8) +
  fill_palette("jco")+
  #fill_palette("jama")+
  #geom_mark_circle(aes(group = factor(mow)), size = 0.2, fill = NA,radius=0.05) +
  theme_classic()+
  #labs(size="Shannon-Wiener",fill="Cutting frequency",x="AFD x",y="AFD y")+
  labs(size="Shannon-Wiener",fill="Fertilization level",x="AFD x",y="AFD y")+
  #labs(size="Marglef richness",fill="Cutting frequency",x="Fitness x",y="Fitness y")+
  theme(legend.position = "left",
        # legend.background = element_rect(
        #   fill = "white",  # 背景颜色
        #   color = "black",  # 边框颜色
        #   size = 0.2,  # 边框宽度
        #   linetype = "solid"  # 边框类型
        # ),
        #plot.title = element_text(family = "serif", size = 16, face = "bold"),  # 设置标题字体
        #plot.subtitle = element_text(family = "serif", size = 12),  # 设置副标题字体
        axis.title = element_text(family = "serif", size = 12),  # 设置轴标题字体
        axis.text = element_text(family = "serif", size = 10),  # 设置轴刻度文字字体
        legend.title = element_text(family = "serif", size = 12),  # 设置图例标题字体
        legend.text = element_text(family = "serif", size = 10)  # 设置图例文字字体
        )+
  guides(
    fill = guide_legend(
      override.aes = list(
        size = 4      # 色块直径
      )
    )
  )
# Marginal densities along x axis
xdens <- axis_canvas(pmain, axis = "x") +
  geom_point(data=fff,aes(x = fitness_x,y=shannon,color=fer), alpha=0.8)+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"))+
  #color_palette("jama")+
  color_palette("jco")+
  geom_smooth(method="lm",data = fff, aes(x = fitness_x, y = shannon),color="green4",inherit.aes = FALSE,size=0.8) +
  annotation_custom(
    grob = textGrob(
      label = expression(atop(cor~-0.75,(p<0.001))),
      gp = gpar(col = "#333333", fontsize = 10, fontfamily = "serif"),
      vjust = 1,  # 垂直偏移
      hjust = 1,  # 水平偏移
    ),
    xmin = Inf, xmax = Inf, ymin = Inf, ymax = Inf
  )+
  theme_minimal() +
  #fill_palette("jama")
  fill_palette("jco")

# Marginal densities along y axis
# Need to set coord_flip = TRUE, if you plan to use coord_flip()
ydens <- axis_canvas(pmain, axis = "y", coord_flip = TRUE) +
  geom_point(data=fff,aes(x = fitness_y,y=shannon,color=fer), alpha=0.8)+
  theme(plot.margin = unit(c(10, 0.5, 0.5, 0.5), "lines"))+
  coord_flip()+
  #color_palette("jama")+
  color_palette("jco")+
  geom_smooth(method="lm",data = fff, aes(x = fitness_y, y = shannon),color="green4",inherit.aes = FALSE,size=0.8) +
  annotation_custom(
    grob = textGrob(
      label = expression(atop(cor~-0.46,(p<0.001))),
      gp = gpar(col = "#333333", fontsize = 10, fontfamily = "serif"),
      vjust = 1,  # 垂直偏移
      hjust = 1,   # 水平偏移
    ),
    xmin = Inf, xmax = Inf, ymin = Inf, ymax = Inf
  )+
  theme_minimal() +
  #fill_palette("jama")
  fill_palette("jco")
p1 <- insert_xaxis_grob(pmain, xdens, grid::unit(.2, "null"), position = "top")
p2 <- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(p2)




#====生态位计算====
#向量提取====
score1 <- scores(uu)
score2 <- scores(vv)

RDA_3<-score1[["biplot"]][,2]
RDA_4<-score2[["biplot"]][,2]

rda3<-data.frame()
rda4<-data.frame()
rda3[1:dim(fun1)[1],1:3]=fun1[1:dim(fun1)[1],1:3]
rda4[1:dim(fun1)[1],1:3]=fun1[1:dim(fun1)[1],1:3]
rda3[,4]=0
rda4[,4]=0
for(i in 1:dim(fun1)[1]){
  for(j in 1:dim(fc)[2]){
    rda3[i,4]=RDA_3[j]*fc1[i,j]+rda3[i,4]
    rda4[i,4]=RDA_4[j]*fc[i,j]+rda4[i,4]
  }
}
#
pp=cbind(fun1[,1:3],fun1[14:17],rda1[,4],rda2[,4],rda3[,4],rda4[,4])
ppp<-f_stand(pp[,4:dim(pp)[2]])
pp<-cbind(fun1[,1:3],ppp)
coe_dudx<-f_curve(4,9,pp)
coe_dudx1<-f_curve(4,11,pp)

coe_dNdx<-f_curve(4,6,pp)

coe_dvdy<-f_curve(5,8,pp)
coe_dvdy1<-f_curve(5,10,pp)

coe_dNdy<-f_curve(5,6,pp)

coe_dudy<-f_curve(5,9,pp)
coe_dvdx<-f_curve(4,8,pp)
k=0
dudx<-numeric()
dudx1<-numeric()
dNdx<-numeric()
dvdy<-numeric()
dvdy1<-numeric()
dNdy<-numeric()

dudy<-numeric()
dvdx<-numeric()
ff<-data.frame()

for(i in 1:dim(pp)[1]){
  if(pp[i,2]=="00"){
    pp[i,2]=0
  }else if(pp[i,2]=="05"){
    pp[i,2]=5
  }else if(pp[i,2]=="10"){
    pp[i,2]=10
  }else if(pp[i,2]=="20"){
    pp[i,2]=20
  }else if(pp[i,2]=="40"){
    pp[i,2]=40
  }else if(pp[i,2]=="80"){
    pp[i,2]=80
  }
}
plot0[,13:16]<-f_stand(plot0[,13:16])
plot00<-data.frame()
for(i in 1:dim(plot0)[1]){
  for(j in 1:dim(pp)[1]){
    if(plot0[i,2]==pp[j,1]){
      if(plot0[i,3]==pp[j,2]){
        if(plot0[i,4]==pp[j,3]){
          k=k+1
          ff[k,1:4]=plot0[i,1:4]
          #====方向导数为负
          dudx[k]=f_dydx(coe_dudx,plot0[i,13])
          ff[k,5]=(plot0[i,15]*dudx[k])
          dudx1[k]=f_dydx(coe_dudx1,plot0[i,13])
          ff[k,6]=(plot0[i,15]*dudx1[k])
          
          dNdx[k]=2*f_dydx(coe_dNdx,plot0[i,13])
          ff[k,7]=(pp[j,9]*dNdx[k])
          
          dvdy[k]=f_dydx(coe_dvdy,plot0[i,14])
          ff[k,8]=(plot0[i,15]*dvdy[k])
          dvdy1[k]=f_dydx(coe_dvdy1,plot0[i,14])
          ff[k,9]=(plot0[i,15]*dvdy1[k])
          
          dNdy[k]=2*f_dydx(coe_dNdy,plot0[i,14])
          ff[k,10]=(pp[j,8]*dNdy[k])
          
          dudy[k]=f_dydx(coe_dudy,plot0[i,14])
          dvdx[k]=f_dydx(coe_dvdx,plot0[i,13])
          
          
          plot00[k,1:20]=plot0[i,1:20]
          plot00[k,21]=log(plot0[i,9]*plot0[i,6]+1)
        }
      }
    }
  }
}
#源汇项量化====

trait<-data.frame()
l=0
for(i in 1:6){
  d1=f_sel2(1,i,ff)
  for(j in 1:3){
    d2=f_sel2(4,j-1,d1)
    for(k in 1:6){
      if(k==1){
        d3=f_sel2(3,0,d2)
        l=l+1
        sfm[l,1]=d3[1,1]
        sfm[l,2:3]=d3[1,3:4]
        
        trait[l,1:3]=sfm[l,1:3]
        for(zz in 4:9){
          trait[l,zz]=mean(d3[,zz+1])
        }
        
        
      }else{
        d3=f_sel2(3,5*2^(k-2),d2)
        l=l+1
        sfm[l,1]=d3[1,1]
        sfm[l,2:3]=d3[1,3:4]
        
        trait[l,1:3]=sfm[l,1:3]
        for(zz in 4:9){
          trait[l,zz]=mean(d3[,zz+1])
        }
        
      }
    }
  }
}

niche<-data.frame()
for(i in 1:dim(ff)[1]){
  #rda轴的适合度
  niche[i,1:4]=ff[i,1:4]
  niche[i,5]=(ff[i,5]+ff[i,7])*(ff[i,6]+ff[i,7])
  niche[i,6]=(ff[i,8]+ff[i,10])*(ff[i,9]+ff[i,10])
  #xy轴的方向
  niche[i,7]=(ff[i,5]+ff[i,7])*(ff[i,6]+ff[i,7])
  niche[i,8]=(ff[i,8]+ff[i,10])*(ff[i,9]+ff[i,10])
  #t轴的方向
  niche[i,9]=niche[i,5]+niche[i,6]
}
#平均生态位差异====
l=0
niche_d<-data.frame()
for(i in 1:6){
  d1=f_sel2(1,i,niche)
  for(j in 1:3){
    d2=f_sel2(4,j-1,d1)
    for(k in 1:6){
      if(k==1){
        d3=f_sel2(3,0,d2)
        l=l+1
        niche_d[l,1]=d3[1,1]
        niche_d[l,2:3]=d3[1,3:4]
        niche_d[l,4]=mean(d3[,5])
        niche_d[l,5]=mean(d3[,6])
        niche_d[l,6]=mean(d3[,7])
        niche_d[l,7]=mean(d3[,8])
        niche_d[l,8]=mean(d3[,9])
      }else{
        d3=f_sel2(3,5*2^(k-2),d2)
        l=l+1
        niche_d[l,1]=d3[1,1]
        niche_d[l,2:3]=d3[1,3:4]
        niche_d[l,4]=mean(d3[,5])
        niche_d[l,5]=mean(d3[,6])
        niche_d[l,6]=mean(d3[,7])
        niche_d[l,7]=mean(d3[,8])
        niche_d[l,8]=mean(d3[,9])
      }
    }
  }
}

ND_AFD_D<-cbind(niche_d[,1:8],fitness[,8:15])
#ND_AFD_D[,4:16]<-f_stand(ND_AFD_D[,4:16])
colnames(ND_AFD_D)<-c("repeat","fer","mow","ND1","ND2","ND3","ND4","ND5","fitness_x","fitness_y","AFD","simpson","shannon","pielou","berger","marglef")

#====绘图====

fff=ND_AFD_D
aov1<-aov(shannon~ND1,data=fff)
aa1<-summary(aov1)
pv1<-aa1[[1]]$`Pr(>F)`[1]
rr1<-cor.test(fff$shannon, fff$ND1, method = "pearson")
r1=round(rr1[[4]],2)

aov2<-aov(shannon~ND2,data=fff)
aa2<-summary(aov2)
pv2<-aa2[[1]]$`Pr(>F)`[1]
rr2<-cor.test(fff$shannon, fff$ND2, method = "pearson")
r2=round(rr2[[4]],2)

#===绘图====


for(i in 1:dim(fff)[1]){
  if(fff[i,3]==0){
   
    fff[i,3]="No mowing"
  }else if(fff[i,3]==1){
    
    fff[i,3]="Mowing once"
  }else if(fff[i,3]==2){
    
    fff[i,3]="Mowing twice"
  }
}
for(i in 1:dim(fff)[1]){
  if(fff[i,2]==0){
    
    fff[i,2]="Low"
  }else if(fff[i,2]==5){
    
    fff[i,2]="Low"
  }else if(fff[i,2]==10){
    
    fff[i,2]="Moderate"
  }else if(fff[i,2]==20){
    
    fff[i,2]="Moderate"
  }else if(fff[i,2]==40){
    
    fff[i,2]="High"
  }else if(fff[i,2]==80){
    
    fff[i,2]="High"
  }
}
pmain<-ggplot(fff, aes(x = ND1, y = ND2,fill=mow,color=mow)) +
  #geom_point()+
  geom_point(color="black",
             shape = 21, 
             stroke = 0.5,
             aes(size=shannon),alpha=0.8) +
  fill_palette("jama")+
  #fill_palette("jco")+
  #geom_mark_circle(aes(group = factor(mow)), size = 0.2, fill = NA,radius=0.05) +
  theme_classic()+
  #labs(size="Marglef richness",fill="Cutting frequency",x="AFD x",y="AFD y")+
  labs(size="Shannon-Wiener",fill="Cutting frequency",x="ND x",y="ND y")+
  #labs(size="Shannon-Wiener",fill="Fertilization level",x="ND x",y="ND y")+
  theme(legend.position = "left",
        # legend.background = element_rect(
        #   fill = "white",  # 背景颜色
        #   color = "black",  # 边框颜色
        #   size = 0.2,  # 边框宽度
        #   linetype = "solid"  # 边框类型
        # ),
        #plot.title = element_text(family = "serif", size = 16, face = "bold"),  # 设置标题字体
        #plot.subtitle = element_text(family = "serif", size = 12),  # 设置副标题字体
        axis.title = element_text(family = "serif", size = 12),  # 设置轴标题字体
        axis.text = element_text(family = "serif", size = 10),  # 设置轴刻度文字字体
        legend.title = element_text(family = "serif", size = 12),  # 设置图例标题字体
        legend.text = element_text(family = "serif", size = 10)  # 设置图例文字字体
  )+
  guides(
    fill = guide_legend(
      override.aes = list(
        size = 4      # 色块直径
      )
    )
  )
# Marginal densities along x axis
xdens <- axis_canvas(pmain, axis = "x") +
  geom_point(data=fff,aes(x = ND1,y=shannon,color=mow), alpha=0.8)+
  color_palette("jama")+
  #color_palette("jco")+
  geom_smooth(method="lm",data = fff, aes(x = ND1, y = shannon),color="green4",inherit.aes = FALSE,size=0.8) +
  annotation_custom(
    grob = textGrob(
      label = expression(atop(cor~0.72,(p<0.001))),
      gp = gpar(col = "#333333", fontsize = 10, fontfamily = "serif"),
      vjust = 1,  # 垂直偏移
      hjust = 1,  # 水平偏移
    ),
    xmin = Inf, xmax = Inf, ymin = Inf, ymax = Inf
  )+
  theme_minimal() +
  fill_palette("jama")
  #fill_palette("jco")

# Marginal densities along y axis
# Need to set coord_flip = TRUE, if you plan to use coord_flip()
ydens <- axis_canvas(pmain, axis = "y", coord_flip = TRUE) +
  geom_point(data=fff,aes(x = ND2,y=shannon,color=mow), alpha=0.8)+
  theme(plot.margin = unit(c(10, 0.5, 0.5, 0.5), "lines"))+
  coord_flip()+
  #color_palette("jco")+
  color_palette("jama")+
  geom_smooth(method="lm",data = fff, aes(x = ND2, y = shannon),color="green4",inherit.aes = FALSE,size=0.8) +
  annotation_custom(
    grob = textGrob(
      label = expression(atop(cor~0.34,(p<0.001))),
      gp = gpar(col = "#333333", fontsize = 10, fontfamily = "serif"),
      vjust = 1,  # 垂直偏移
      hjust = 1,   # 水平偏移
    ),
    xmin = Inf, xmax = Inf, ymin = Inf, ymax = Inf
  )+
  theme_minimal() +
  fill_palette("jama")
  #fill_palette("jco")

p3 <- insert_xaxis_grob(pmain, xdens, grid::unit(.2, "null"), position = "top")
p4 <- insert_yaxis_grob(p3, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(p4)

#====
write_xlsx(ND_AFD_D,"C:/Users/Yef0/Desktop/study/data_unprocessed/Fertilizing_and_Mowing/ND_AFD_D.xlsx")

#====综合统计与预测====
plot_2023<-read_excel("C:/Users/Yef0/Desktop/study/data_unprocessed/Fertilizing_and_Mowing/plot23.xlsx")

pre0<-ff
for (i in 1:dim(pre0)[1]){
  for (j in 1:dim(plot_2023)[1]){
    if (pre0[i,1]==plot_2023[j,1]){
      if (pre0[i,2]==plot_2023[j,2]){
        if (pre0[i,3]==plot_2023[j,3]){
          if (pre0[i,4]==plot_2023[j,4]){
            pre0[i,11:14]=plot_2023[j,13:16]
          }
        }
      }
    }
  }
}
pre0$rcov[is.na(pre0$rcov)] <- 0
pre0$rh[is.na(pre0$rh)] <- 0
pre0$rabu[is.na(pre0$rabu)] <- 0
pre0$rbio[is.na(pre0$rbio)] <- 0

for (i in 1:dim(pre0)[1]){
  pre0[i,15]=pre0[i,11]-plot00[i,13]
  pre0[i,16]=pre0[i,12]-plot00[i,14]
  pre0[i,17]=pre0[i,13]-plot00[i,15]
  pre0[i,18]=pre0[i,14]-plot00[i,16]
}
#===散点拟合====

nnn=10
#nnn=14,则绘制两年间生态参数改变量，nnn=10，则绘制第二年生态参数

for( i in 1:4){
  if(i==1){
    xx="dudx"
    ss="#A52A2A"
    cc="#CAFF70"
  }else if(i==2){
    xx="dNdx"
    ss="#8B6508"
    cc="#008B8B"
  }else if(i==3){
    xx="dvdy"
    ss="#006400"
    cc="#EE3B3B"
  }else if(i==4){
    xx="dNdy"
    ss="#0000CD"
    cc="#EEAD0E"
  }
  
  for(j in 1:4){
    if(j==1){
      yy="rcov"
    }else if(j==2){
      yy="rh"
    }else if(j==3){
      yy="rabu"
    }else if(j==4){
      yy="rbio"
    }
    
    # if(j==1){
    #   yy="tcov"
    # }else if(j==2){
    #   yy="th"
    # }else if(j==3){
    #   yy="tabu"
    # }else if(j==4){
    #   yy="tbio"
    # }
    
    #model approximate
    #模型拟合
    model <- lm(pre0[,nnn+j] ~ pre0[,4+i], data =pre0)
    rr1<-cor.test(pre0[,nnn+j], pre0[,4+i], method = "pearson")
    r1=round(rr1[[4]],2)
    p_value <- rr1$p.value
    
    if(p_value<0.05){
      pp=0.05
      d_map<-data.frame(cbind(x=pre0[,4+i],pre0[,nnn+j]))
      colnames(d_map)<-c("x","y")
      print(r1)
      plot<-ggplot(d_map,aes(x=x,y=y))+
        geom_point(color=ss,size=2,alpha=0.15)+
        geom_smooth(method = "lm", se = TRUE, color = cc, linetype = "solid") +
        labs(x = xx, y = yy)+
        annotate("text", x = min(d_map$x), y = max(d_map$y), 
                 label = paste0("cor =", r1, " (p <", pp,")"), 
                 hjust = 0, vjust = 0.5*(max(d_map$y)-min(d_map$y)), size = 4, color = "black") +
        theme_minimal()+
        theme(plot.margin = unit(c(1, 1, 0, 1), "lines",),
              plot.title = element_text(family = "serif", size = 16, face = "bold"),  # 设置标题字体
              plot.subtitle = element_text(family = "serif", size = 12),  # 设置副标题字体
              axis.title = element_text(family = "serif", size = 14),  # 设置轴标题字体
              axis.text = element_text(family = "serif", size = 14),  # 设置轴刻度文字字体
              legend.title = element_text(family = "serif", size = 14),  # 设置图例标题字体
              legend.text = element_text(family = "serif", size = 14),   # 设置图例文字字体
              plot.background = element_rect(fill = "white", color = "white"),  # 设置整个图形的背景颜色
              panel.background = element_rect(fill = "white", color = "white"),  # 设置绘图区域的背景颜色
        )
      if(p_value<0.01){
        pp=0.01
        d_map<-data.frame(cbind(x=pre0[,4+i],pre0[,nnn+j]))
        colnames(d_map)<-c("x","y")
        print(r1)
        plot<-ggplot(d_map,aes(x=x,y=y))+
          geom_point(color=ss,size=2,alpha=0.15)+
          geom_smooth(method = "lm", se = TRUE, color = cc, linetype = "solid") +
          labs(x = xx, y = yy)+
          annotate("text", x = min(d_map$x), y = max(d_map$y), 
                   label = paste0("cor =", r1, " (p <", pp,")"), 
                   hjust = 0, vjust = 0.5*(max(d_map$y)-min(d_map$y)), size = 4, color = "black") +
          theme_minimal()+
          theme(plot.margin = unit(c(1, 1, 0, 1), "lines",),
                plot.title = element_text(family = "serif", size = 16, face = "bold"),  # 设置标题字体
                plot.subtitle = element_text(family = "serif", size = 12),  # 设置副标题字体
                axis.title = element_text(family = "serif", size = 14),  # 设置轴标题字体
                axis.text = element_text(family = "serif", size = 14),  # 设置轴刻度文字字体
                legend.title = element_text(family = "serif", size = 14),  # 设置图例标题字体
                legend.text = element_text(family = "serif", size = 14),   # 设置图例文字字体
                plot.background = element_rect(fill = "white", color = "white"),  # 设置整个图形的背景颜色
                panel.background = element_rect(fill = "white", color = "white"),  # 设置绘图区域的背景颜色
          )
        if(p_value<0.001){
          pp=0.001
          d_map<-data.frame(cbind(x=pre0[,4+i],pre0[,nnn+j]))
          colnames(d_map)<-c("x","y")
          print(r1)
          plot<-ggplot(d_map,aes(x=x,y=y))+
            geom_point(color=ss,size=2,alpha=0.15)+
            geom_smooth(method = "lm", se = TRUE, color = cc, linetype = "solid") +
            labs(x = xx, y = yy)+
            annotate("text", x = min(d_map$x), y = max(d_map$y), 
                     label = paste0("cor =", r1, " (p <", pp,")"), 
                     hjust = 0, vjust = 0.5*(max(d_map$y)-min(d_map$y)), size = 4, color = "black") +
            theme_minimal()+
            theme(plot.margin = unit(c(1, 1, 0, 1), "lines",),
                  plot.title = element_text(family = "serif", size = 16, face = "bold"),  # 设置标题字体
                  plot.subtitle = element_text(family = "serif", size = 12),  # 设置副标题字体
                  axis.title = element_text(family = "serif", size = 14),  # 设置轴标题字体
                  axis.text = element_text(family = "serif", size = 14),  # 设置轴刻度文字字体
                  legend.title = element_text(family = "serif", size = 14),  # 设置图例标题字体
                  legend.text = element_text(family = "serif", size = 14),   # 设置图例文字字体
                  plot.background = element_rect(fill = "white", color = "white"),  # 设置整个图形的背景颜色
                  panel.background = element_rect(fill = "white", color = "white"),  # 设置绘图区域的背景颜色
            )
        }
      }
    }else{
      pp=round(p_value,2)
      d_map<-data.frame(cbind(x=pre0[,4+i],pre0[,nnn+j]))
      colnames(d_map)<-c("x","y")
      print(r1)
      plot<-ggplot(d_map,aes(x=x,y=y))+
        geom_point(color=ss,size=2,alpha=0.15)+
        geom_smooth(method = "lm", se = TRUE, color = cc, linetype = "solid") +
        labs(x = xx, y = yy)+
        annotate("text", x = min(d_map$x), y = max(d_map$y), 
                 label = paste0("cor =", r1, " (p =", pp,")"), 
                 hjust = 0, vjust = 0.5*(max(d_map$y)-min(d_map$y)), size = 4, color = "black") +
        theme_minimal()+
        theme(plot.margin = unit(c(1, 1, 0, 1), "lines",),
              plot.title = element_text(family = "serif", size = 16, face = "bold"),  # 设置标题字体
              plot.subtitle = element_text(family = "serif", size = 12),  # 设置副标题字体
              axis.title = element_text(family = "serif", size = 14),  # 设置轴标题字体
              axis.text = element_text(family = "serif", size = 14),  # 设置轴刻度文字字体
              legend.title = element_text(family = "serif", size = 14),  # 设置图例标题字体
              legend.text = element_text(family = "serif", size = 14),   # 设置图例文字字体
              plot.background = element_rect(fill = "white", color = "white"),  # 设置整个图形的背景颜色
              panel.background = element_rect(fill = "white", color = "white"),  # 设置绘图区域的背景颜色
        )
      
    }
    #list mapping
    #批量绘图
    a<-paste0("plot",i,j)
    assign(a,plot)
    if(nnn==14){
      ggsave(filename = paste0("C:/Users/Yef0/Desktop/study/Article1/picture/pre/", 4*(i-1)+j, ".png"), plot = plot, width = 4, height = 3.2)
    }else{
      ggsave(filename = paste0("C:/Users/Yef0/Desktop/study/", 4*(i-1)+j, ".png"), plot = plot, width = 4, height = 3.2)
    }
    
  }
}
# plot1<-ggplot(pre0,aes(x=V8,y=V18))+
#   geom_point(color="blue",size=2,alpha=0.4)+
#   geom_smooth(method = "lm", se = TRUE, color = "purple", linetype = "solid") +
#   labs(title = "散点图与拟合曲线", x = "AFDX", y = "coverage") +
#   theme_minimal()+
#   theme(plot.margin = unit(c(1, 1, 0, 1), "lines",),
#         plot.title = element_text(family = "serif", size = 16, face = "bold"),  # 设置标题字体
#         plot.subtitle = element_text(family = "serif", size = 12),  # 设置副标题字体
#         axis.title = element_text(family = "serif", size = 12),  # 设置轴标题字体
#         axis.text = element_text(family = "serif", size = 12),  # 设置轴刻度文字字体
#         legend.title = element_text(family = "serif", size = 12),  # 设置图例标题字体
#         legend.text = element_text(family = "serif", size = 12)   # 设置图例文字字体
#   )
# colnames(preo[,15:18])<-c("tcov","th","tabu","tbio")
#===绘图====
for( i in 1:4){
  if(i==1){
    xx="dudx"
    ss="#A52A2A"
    cc="#CAFF70"
  }else if(i==2){
    xx="dNdx"
    ss="#8B6508"
    cc="#008B8B"
  }else if(i==3){
    xx="dvdy"
    ss="#006400"
    cc="#EE3B3B"
  }else if(i==4){
    xx="dNdy"
    ss="#0000CD"
    cc="#EEAD0E"
  }
  
  for(j in 1:4){
    if(j==1){
      yy="tcov"
    }else if(j==2){
      yy="th"
    }else if(j==3){
      yy="tabu"
    }else if(j==4){
      yy="tbio"
    }
    
    #model approximate
    #模型拟合
    model <- lm(pre0[,nnn+j] ~ pre0[,4+i], data =pre0)
    rr1<-cor.test(pre0[,nnn+j], pre0[,4+i], method = "pearson")
    r1=round(rr1[[4]],2)
    p_value <- rr1$p.value
    if(p_value<0.05){
      pp=0.05
      if(p_value<0.01){
        pp=0.01
        if(p_value<0.001){
          pp=0.001
        }
      }
    }
    #list mapping
    #批量绘图
    d_map<-data.frame(cbind(x=pre0[,4+i],pre0[,nnn+j]))
    colnames(d_map)<-c("x","y")
    print(r1)
    plot<-ggplot(d_map,aes(x=x,y=y))+
      geom_point(color=ss,size=2,alpha=0.15)+
      geom_smooth(method = "lm", se = TRUE, color = cc, linetype = "solid") +
      xlab(NULL)+
      ylab(NULL)+
      annotate("text", x = min(d_map$x), y = max(d_map$y), 
               label = paste0("cor =", r1, " (p <", pp,")"), 
               hjust = 0, vjust = 0.5*(max(d_map$y)-min(d_map$y)), size = 4, color = "black") +
      theme_minimal()+
      theme(plot.margin = unit(c(1, 1, 0, 1), "lines",),
            plot.title = element_text(family = "serif", size = 16, face = "bold"),  # 设置标题字体
            plot.subtitle = element_text(family = "serif", size = 12),  # 设置副标题字体
            axis.title = element_text(family = "serif", size = 12),  # 设置轴标题字体
            axis.text = element_text(family = "serif", size = 12),  # 设置轴刻度文字字体
            legend.title = element_text(family = "serif", size = 12),  # 设置图例标题字体
            legend.text = element_text(family = "serif", size = 12)   # 设置图例文字字体
      )
    if(i==1){
      d_map<-data.frame(cbind(x=pre0[,4+i],pre0[,nnn+j]))
      colnames(d_map)<-c("x","y")
      print(r1)
      plot<-ggplot(d_map,aes(x=x,y=y))+
        geom_point(color=ss,size=2,alpha=0.15)+
        geom_smooth(method = "lm", se = TRUE, color = cc, linetype = "solid") +
        xlab(NULL)+
        ylab(yy)+
        annotate("text", x = min(d_map$x), y = max(d_map$y), 
                 label = paste0("cor =", r1, " (p <", pp,")"), 
                 hjust = 0, vjust = 0.5*(max(d_map$y)-min(d_map$y)), size = 4, color = "black") +
        theme_minimal()+
        theme(plot.margin = unit(c(1, 1, 0, 1), "lines",),
              plot.title = element_text(family = "serif", size = 16, face = "bold"),  # 设置标题字体
              plot.subtitle = element_text(family = "serif", size = 12),  # 设置副标题字体
              axis.title = element_text(family = "serif", size = 12),  # 设置轴标题字体
              axis.text = element_text(family = "serif", size = 12),  # 设置轴刻度文字字体
              legend.title = element_text(family = "serif", size = 12),  # 设置图例标题字体
              legend.text = element_text(family = "serif", size = 12)   # 设置图例文字字体
        )
    }
    if(j==4){
      d_map<-data.frame(cbind(x=pre0[,4+i],pre0[,nnn+j]))
      colnames(d_map)<-c("x","y")
      print(r1)
      plot<-ggplot(d_map,aes(x=x,y=y))+
        geom_point(color=ss,size=2,alpha=0.15)+
        geom_smooth(method = "lm", se = TRUE, color = cc, linetype = "solid") +
        ylab(NULL)+
        xlab(xx)+
        annotate("text", x = min(d_map$x), y = max(d_map$y), 
                 label = paste0("cor =", r1, " (p <", pp,")"), 
                 hjust = 0, vjust = 0.5*(max(d_map$y)-min(d_map$y)), size = 4, color = "black") +
        theme_minimal()+
        theme(plot.margin = unit(c(1, 1, 0, 1), "lines",),
              plot.title = element_text(family = "serif", size = 16, face = "bold"),  # 设置标题字体
              plot.subtitle = element_text(family = "serif", size = 12),  # 设置副标题字体
              axis.title = element_text(family = "serif", size = 12),  # 设置轴标题字体
              axis.text = element_text(family = "serif", size = 12),  # 设置轴刻度文字字体
              legend.title = element_text(family = "serif", size = 12),  # 设置图例标题字体
              legend.text = element_text(family = "serif", size = 12)   # 设置图例文字字体
        )
    }
    if(j==4&&i==1){
      d_map<-data.frame(cbind(x=pre0[,4+i],pre0[,nnn+j]))
      colnames(d_map)<-c("x","y")
      print(r1)
      plot<-ggplot(d_map,aes(x=x,y=y))+
        geom_point(color=ss,size=2,alpha=0.15)+
        geom_smooth(method = "lm", se = TRUE, color = cc, linetype = "solid") +
        labs(x = xx, y = yy)+
        annotate("text", x = min(d_map$x), y = max(d_map$y), 
                 label = paste0("cor =", r1, " (p <", pp,")"), 
                 hjust = 0, vjust = 0.5*(max(d_map$y)-min(d_map$y)), size = 4, color = "black") +
        theme_minimal()+
        theme(plot.margin = unit(c(1, 1, 0, 1), "lines",),
              plot.title = element_text(family = "serif", size = 16, face = "bold"),  # 设置标题字体
              plot.subtitle = element_text(family = "serif", size = 12),  # 设置副标题字体
              axis.title = element_text(family = "serif", size = 12),  # 设置轴标题字体
              axis.text = element_text(family = "serif", size = 12),  # 设置轴刻度文字字体
              legend.title = element_text(family = "serif", size = 12),  # 设置图例标题字体
              legend.text = element_text(family = "serif", size = 12)   # 设置图例文字字体
        )
    }
    
    a<-paste0("plot",i,j)
    assign(a,plot)
  }
}
#===图像拼接====
plots<-list(plot11,plot21,plot31,plot41,plot12,plot22,plot32,plot42,plot13,plot23,plot33,plot43,plot14,plot24,plot34,plot44)

grid.arrange(grobs = plots, nrow = 4, ncol = 4,
             widths=c(1,1,1,1),
             heights = c(1, 1,1,1),
             clip="on")
#===多样性预测====

p_simpson<-data.frame()
p_shannon<-data.frame()
p_pielou<-data.frame()
p_berger<-data.frame()
p_marglef<-data.frame()
l=0
d1<-data.frame()
d2<-data.frame()
d3<-data.frame()

for(i in 1:6){
  d1=f_sel2(1,i,plot_2023)
  for(j in 1:3){
    d2=f_sel2(4,j-1,d1)

    for(k in 1:6){
      if(k==1){
        d3=f_sel2(3,0,d2)

        l=l+1
        #===地块
        sfm[l,1]=d3[1,1]
        sfm[l,2:3]=d3[1,3:4]
        #===多样性指数
        p_simpson[l,1:3]=d3[1,1:3]
        p_simpson[l,4]=1-sum(d3[,19])
        
        p_shannon[l,1:3]=sfm[l,1:3]
        p_shannon[l,4]=-sum(d3[,20])
        
        p_pielou[l,1:3]=sfm[l,1:3]
        p_pielou[l,4]=p_shannon[l,4]/log(dim(d3)[1])
        
        p_berger[l,1:3]=sfm[l,1:3]
        p_berger[l,4]=max(d3[,7])/sum(d3[,7])
        
        p_marglef[l,1:3]=sfm[l,1:3]
        p_marglef[l,4]=(dim(d3)[1]-1)/log(d3[1,11])
        
      }else{
        d3=f_sel2(3,5*2^(k-2),d2)
        
        l=l+1
        #===地块
        sfm[l,1]=d3[1,1]
        sfm[l,2:3]=d3[1,3:4]
        #===多样性指数
        p_simpson[l,1:3]=sfm[l,1:3]
        p_simpson[l,4]=1-sum(d3[,19])
        
        p_shannon[l,1:3]=sfm[l,1:3]
        p_shannon[l,4]=-sum(d3[,20])
        
        p_pielou[l,1:3]=sfm[l,1:3]
        p_pielou[l,4]=p_shannon[l,4]/log(dim(d3)[1])
        
        p_berger[l,1:3]=sfm[l,1:3]
        p_berger[l,4]=max(d3[,7])/sum(d3[,7])
        
        p_marglef[l,1:3]=sfm[l,1:3]
        p_marglef[l,4]=(dim(d3)[1]-1)/log(d3[1,11])
        
      }
    }
  }
}

fff<-cbind(ND_AFD_D[,1:11],p_simpson[,4],p_shannon[,4],p_pielou[,4],p_berger[,4],p_marglef[,4])
colnames(fff)<-c("repeat","fer","mow","ND1","ND2","ND3","ND4","ND5","fitness_x","fitness_y","AFD","simpson","shannon","pielou","berger","marglef")
write_xlsx(fff,"C:/Users/Yef0/Desktop/study/data_unprocessed/Fertilizing_and_Mowing/ND_AFD_D_23.xlsx")
#====空间变化率的统计补充====

ff<-read_excel("C:/Users/Yef0/Desktop/study/data_unprocessed/Fertilizing_and_Mowing/spacial_change.xlsx")
colnames(ff)<-c("Repeat","Species","fer","mow","dudx","dNdx","dvdy","dNdy")
ff$ch<-(ff$dvdy+ff$dNdy)/(ff$dudx+ff$dNdx)
for(i in 1:dim(ff)[1]){
  ff$Species[i]<-f_replace(ff$Species[i])
  
  if(ff$ch[i]>2.5){
    ff$ch[i]=2.5
  }else if(ff$ch[i]<(-2.5)){
    ff$ch[i]=-2.5
  }
  
  if(ff$ch[i]>1){
    ff$ch[i]=1+ff$ch[i]/10
  }else if(ff$ch[i]<(-1)){
    ff$ch[i]=-1+ff$ch[i]/10
  }
}
ff$mow <- as.character(ff$mow)
ff$fer <- as.character(ff$fer)
# ff$fer <- factor(ff$fer, levels = unique(ff$fer))
# ff$mow <- factor(ff$mow, levels = unique(ff$mow))
for(i in 1:dim(ff)[1]){
  
  if(ff[i,4]=="0"){
    
    ff[i,4]="No mowing"
  }else if(ff[i,4]==1){
    
    ff[i,4]="Mowing once"
  }else if(ff[i,4]==2){
    
    ff[i,4]="Mowing twice"
  }
}
for(i in 1:dim(ff)[1]){
  
  if(ff[i,3]=="CK"){
    
    ff[i,3]="Low"
  }else if(ff[i,3]==5){
    
    ff[i,3]="Low"
  }else if(ff[i,3]==10){
    
    ff[i,3]="Moderate"
  }else if(ff[i,3]==20){
    
    ff[i,3]="Moderate"
  }else if(ff[i,3]==40){
    
    ff[i,3]="High"
  }else if(ff[i,3]==80){
    
    ff[i,3]="High"
  }
}
ggplot(ff,aes(x=Species,y=ch,color=Species))+
  geom_beeswarm(method = "compactswarm",size = 1.5,dodge.width=4,corral.width=2, alpha = 0.7) +
  scale_color_manual(values=c("SSC" = "#A52A2A", "SPR" = "#8B6508", "SJA" = "#006400","SCO"="#0000CD","PBI"="#CAFF70","CLA"="#008B8B","CARI"="#EE3B3B","AGM"="#EEAD0E"))+
  labs(title = "横向蜂群图", x = "处理方式", y = "数值")+
  coord_flip() +  # 将图形转为横向
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 0, 1), "lines",),
        axis.title = element_text(family = "serif", size = 12),  # 设置轴标题字体
        axis.text = element_text(family = "serif", size = 12),  # 设置轴刻度文字字体
        legend.title = element_text(family = "serif", size = 12),  # 设置图例标题字体
        legend.text = element_text(family = "serif", size = 12)   # 设置图例文字字体
  )
#====天气附图====
ww<-read_excel("C:/Users/Yef0/Desktop/study/data_unprocessed/weather/2022_2023guyuan.xlsx")
ww$mouth <- factor(ww$mouth, levels = unique(ww$mouth))
ww$group <- factor(ww$group, levels = unique(ww$group))

colors <- c("2022" = "#0073C2FF", "2023" = "#EFC000FF")
colors1<- c("2022" = "#00A1D5FF", "2023" = "#374E55FF")
colors2<- c("2022" = "#DF8F44FF", "2023" = "#B24745FF")
plot1<-ggplot(data=ww, aes(x=mouth, y=rain,group=group,color=group)) + 
  geom_point(size=2) + 
  geom_line() + 
  labs(title="", x="Month", y="Precipitation (mm)",color="years")+ 
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 0, 1), "lines",),
        axis.title = element_text(family = "serif", size = 12),  # 设置轴标题字体
        axis.text = element_text(family = "serif", size = 12),  # 设置轴刻度文字字体
        legend.title = element_text(family = "serif", size = 12),  # 设置图例标题字体
        legend.text = element_text(family = "serif", size = 12)   # 设置图例文字字体
  )+
  scale_color_manual(values = colors)
plot2<-ggplot(data=ww, aes(x=mouth, y=wind,group=group,color=group)) + 
  geom_point(size=2) + 
  geom_line() + 
  labs(title="", x="Month", y="Wind speed (km/h)",color="years")+ 
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 0, 1), "lines",),
        axis.title = element_text(family = "serif", size = 12),  # 设置轴标题字体
        axis.text = element_text(family = "serif", size = 12),  # 设置轴刻度文字字体
        legend.title = element_text(family = "serif", size = 12),  # 设置图例标题字体
        legend.text = element_text(family = "serif", size = 12)   # 设置图例文字字体
  )+
  scale_color_manual(values = colors)
plot3<-ggplot(data=ww, aes(x=mouth, y=min_t,group=group,color=group)) + 
  geom_point(size=2) + 
  geom_line() + 
  labs(title="", x="Month", y="Mean minimum temperature (\u00B0C)",color="years")+ 
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 0, 1), "lines",),
        axis.title = element_text(family = "serif", size = 12),  # 设置轴标题字体
        axis.text = element_text(family = "serif", size = 12),  # 设置轴刻度文字字体
        legend.title = element_text(family = "serif", size = 12),  # 设置图例标题字体
        legend.text = element_text(family = "serif", size = 12)   # 设置图例文字字体
  )+
  scale_color_manual(values = colors1)
plot4<-ggplot(data=ww, aes(x=mouth, y=max_t,group=group,color=group)) + 
  geom_point(size=2) + 
  geom_line() + 
  labs(title="", x="Month", y="Mean maximum temperature (\u00B0C)",color="years")+ 
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 0, 1), "lines",),
        axis.title = element_text(family = "serif", size = 12),  # 设置轴标题字体
        axis.text = element_text(family = "serif", size = 12),  # 设置轴刻度文字字体
        legend.title = element_text(family = "serif", size = 12),  # 设置图例标题字体
        legend.text = element_text(family = "serif", size = 12)   # 设置图例文字字体
  )+
  scale_color_manual(values = colors2)
plots<-list(plot1,plot2,plot4,plot3)

grid.arrange(grobs = plots, nrow = 4, ncol = 1,
             widths=1,
             heights =c(1,1,1,1),
             clip="on")
#====物种统计====
colors=c("#374E55","#DF8F44","#00A1D5","#B24745","#79AF97","#6A6599","#80796B","#9900CC","#FFFF00","#00FF0099","#A52A2A","#8B6508","#006400","#0000CD","#CAFF70","#008B8B","#EE3B3B","#EEAD0E")
ss<-read_excel("C:/Users/Yef0/Desktop/study/data_unprocessed/Fertilizing_and_Mowing/Species_charac.xlsx")
data<-data.frame(cbind(ss$Species,ss$Family,ss[,8]*2))
colnames(data)<-c("individual","group","value")

empty_bar <- 4
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

label_data <- data
number_of_bar <- nrow(label_data)

angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity",alpha=0.9) +
  ylim(-100,120) +
  labs(fill="Family")+
  theme_minimal() +
  theme(
    #legend.position = "none",
    legend.title = element_text(family = "serif", size = 12),  # 设置图例标题字体
    legend.text = element_text(family = "serif", size = 10),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, 
            aes(x=id, y=value+10, label=individual, hjust=hjust), 
            color="black", 
            fontface="bold",
            alpha=0.6,
            size=2.5, 
            angle= label_data$angle, 
            inherit.aes = FALSE)+
  scale_fill_manual(values=colors)


#====措施与多样性绘图====
tt<-read_excel("C:/Users/Yef0/Desktop/study/data_unprocessed/Fertilizing_and_Mowing/fitness_biodiv.xlsx")

dfff<-function(n,tt){
  mean0<- aggregate(tt[,n], by=list(tt$fer, tt$mow), FUN=mean)
  sd <- aggregate(tt[,n], by=list(tt$fer, tt$mow), FUN=sd)
  len <- aggregate(tt[,n], by=list(tt$fer, tt$mow), FUN=length)
  dff <- data.frame(mean0, sd=sd[,3], len=len[,3])
  str(dff)
  
  return(dff)
}
n=12
ttt<-dfff(n,tt)
colnames(ttt)<-c("fer","mow","mean0","sd","len")

ttt <- ttt %>% 
  mutate(
    mow = factor(
      mow,
      levels = unique(mow),
      labels = case_when(
        unique(mow) == 0 ~ "No mowing",
        unique(mow) == 1 ~ "Mowing once",
        unique(mow) == 2 ~ "Mowing twice",
        TRUE             ~ as.character(unique(mow))
      )
    ),
    fer = factor(
      fer,
      levels = unique(fer),
      labels = case_when(
        unique(fer) == 0 ~ "Low",
        unique(fer) == 1 ~ "Moderate",
        unique(fer) == 2 ~ "High",
        TRUE             ~ as.character(unique(fer))
      )
    )
  )
  
ggplot(data = ttt,
       aes(x = mow, y = fer, fill = mean0)) +
  geom_tile() +
  geom_text(aes(label = round(mean0, 2)), color = "black", size = 4) +
  scale_fill_gradient(low = "#FBCEB1", high = "#A52A2A",name="Shannon-Wiener") +
  labs(x="Cutting frequency",y="Fertilization level")+
  theme_bw() +
  coord_fixed(ratio = 1)+
  theme(
    axis.ticks      = element_blank(),
    panel.grid      = element_blank(),
    axis.text       = element_text(family = "serif",color='black',size = 12),
    axis.title.x    = element_text(family = "serif",color='black',size = 14),   # 横轴
    axis.title.y    = element_text(family = "serif",color='black',size = 14),   # 纵轴
    legend.title    = element_text(family = "serif",color='black',size = 14),   # 图例
    legend.text     = element_text(family = "serif",color='black',size = 12),   # 图例
  )
#===方差分析====
  ###
  tt <- tt %>% 
    mutate(
      mow = factor(
        mow,
        levels = unique(mow),
        labels = case_when(
          unique(mow) == 0 ~ "No mowing",
          unique(mow) == 1 ~ "Mowing once",
          unique(mow) == 2 ~ "Mowing twice",
          TRUE             ~ as.character(unique(mow))
        )
      ),
      fer = factor(
        fer,
        levels = unique(fer),
        labels = case_when(
          unique(fer) == 0 ~ "Low",
          unique(fer) == 1 ~ "Moderate",
          unique(fer) == 2 ~ "High",
          TRUE             ~ as.character(unique(fer))
        )
      )
    )
  
  #正态分布检验
  shapiro.test(tt$shannon)
  
  #方差齐性检验
  tt$fer <- as.factor(tt$fer)
  tt$mow <- as.factor(tt$mow)
  leveneTest(shannon ~ fer, data = tt)  # 仅测试 fer 的分组
  leveneTest(shannon ~ mow, data = tt)  # 仅测试 mow 的分组
  
  #双因素方差分析
  model <- aov(shannon ~ fer * mow, data = tt)
  summary(model)
  
  #Tukey多重比较检验====
  T_result<-TukeyHSD(model)
  tukey_df <-  tidy(T_result)
  
  # 绘制箱线图并添加显著性标记
  #shannon-Wiener====
  tt$fer <- factor(tt$fer)
  tt$mow <- factor(tt$mow)
  tt <- tt %>% 
    mutate(
      mow = factor(
        mow,
        levels = unique(mow),
        labels = case_when(
          unique(mow) == 0 ~ "No mowing",
          unique(mow) == 1 ~ "Mowing once",
          unique(mow) == 2 ~ "Mowing twice",
          TRUE             ~ as.character(unique(mow))
        )
      ),
      fer = factor(
        fer,
        levels = unique(fer),
        labels = case_when(
          unique(fer) == 0 ~ "Low",
          unique(fer) == 1 ~ "Moderate",
          unique(fer) == 2 ~ "High",
          TRUE             ~ as.character(unique(fer))
        )
      )
    )
  #组内比较
  df_p_val1 <- tt %>% 
    group_by(fer) %>% 
    wilcox_test(formula = shannon ~ mow) %>% 
    add_significance(p.col = 'p',cutpoints = c(0,0.001,0.01,0.05,1),symbols = c('***','**','*','ns')) %>% 
    add_xy_position(x='fer')
  #组间比较
  df_p_val2 <- tt %>% 
    wilcox_test(formula = shannon ~ fer) %>% 
    add_significance(p.col = 'p',cutpoints = c(0,0.001,0.01,0.05,1),symbols = c('***','**','*','ns')) %>% 
    add_xy_position()
  
  ggplot()+
    geom_boxplot(data = tt,mapping = aes(x=fer,y=shannon,fill=mow))+
    scale_fill_manual(values = c('#00A1D5FF','#374E55FF','#DF8F44FF'),name="Cuting frequency")+
    stat_pvalue_manual(data = df_p_val1,label = '{p.signif}',tip.length = 0,step.increase  = 0,y.position = df_p_val1$y.position*0.8+0.35)+
    stat_pvalue_manual(data = df_p_val2,label = '{p.signif}',tip.length = 0.01,step.increase  = 0.04,y.position = 2.85)+
    labs(x='Fertilization level',y='Shannon-Wiener Diversity Index')+
    theme_test()+
    theme(plot.caption = element_markdown(face = 'bold'),
          axis.title = element_text(family = "serif",color='black', size = 14),
          axis.text = element_text(family = "serif",color='black', size = 12),
          legend.title = element_text( family = "serif",color='black',size = 14),
          legend.text = element_text( family = "serif",color='black',size = 12),
          legend.key.width  = unit(1.2, "cm"),   # 每个色块宽
          legend.key.height = unit(0.8, "cm"))+   # 每个色块高+
    scale_y_continuous(limits = c(0.9,3.1),expand = c(0,0))
  #Pielous ====
  df_p_val1 <- tt %>% 
    group_by(fer) %>% 
    wilcox_test(formula = pielou ~ mow) %>% 
    add_significance(p.col = 'p',cutpoints = c(0,0.001,0.01,0.05,1),symbols = c('***','**','*','ns')) %>% 
    add_xy_position(x='fer')
  #组间比较
  df_p_val2 <- tt %>% 
    wilcox_test(formula = pielou ~ fer) %>% 
    add_significance(p.col = 'p',cutpoints = c(0,0.001,0.01,0.05,1),symbols = c('***','**','*','ns')) %>% 
    add_xy_position()
  
  ggplot()+
    geom_boxplot(data = tt,mapping = aes(x=fer,y=pielou,fill=mow))+
    scale_fill_manual(values = c('#00A1D5FF','#374E55FF','#DF8F44FF'),name="Cuting frequency")+
    stat_pvalue_manual(data = df_p_val1,label = '{p.signif}',tip.length = 0,step.increase  = 0,y.position = df_p_val1$y.position*1.35-0.35)+
    stat_pvalue_manual(data = df_p_val2,label = '{p.signif}',tip.length = 0.01,step.increase  = 0.04,y.position = 1.046)+
    labs(x='Fertilization level',y="Pielou's Evenness Index")+
    theme_test()+
    theme(plot.caption = element_markdown(face = 'bold'),
          axis.title = element_text(family = "serif",color='black', size = 14),
          axis.text = element_text(family = "serif",color='black', size = 12),
          legend.title = element_text( family = "serif",color='black',size = 14),
          legend.text = element_text( family = "serif",color='black',size = 12),
          legend.key.width  = unit(1.2, "cm"),   # 每个色块宽
          legend.key.height = unit(0.8, "cm"))+   # 每个色块高+
    scale_y_continuous(limits = c(0.63,1.1),expand = c(0,0))
  
  #Berger ====
  df_p_val1 <- tt %>% 
    group_by(fer) %>% 
    wilcox_test(formula = berger ~ mow) %>% 
    add_significance(p.col = 'p',cutpoints = c(0,0.001,0.01,0.05,1),symbols = c('***','**','*','ns')) %>% 
    add_xy_position(x='fer')
  #组间比较
  df_p_val2 <- tt %>% 
    wilcox_test(formula = berger ~ fer) %>% 
    add_significance(p.col = 'p',cutpoints = c(0,0.001,0.01,0.05,1),symbols = c('***','**','*','ns')) %>% 
    add_xy_position()
  
  ggplot()+
    geom_boxplot(data = tt,mapping = aes(x=fer,y=berger,fill=mow))+
    scale_fill_manual(values = c('#00A1D5FF','#374E55FF','#DF8F44FF'),name="Cuting frequency")+
    stat_pvalue_manual(data = df_p_val1,label = '{p.signif}',tip.length = 0,step.increase  = 0,y.position = df_p_val1$y.position*0.9+0.05)+
    stat_pvalue_manual(data = df_p_val2,label = '{p.signif}',tip.length = 0.01,step.increase  = 0.04,y.position = 0.9863)+
    labs(x='Fertilization level',y="Berger-Parker Dominance Index")+
    theme_test()+
    theme(plot.caption = element_markdown(face = 'bold'),
          axis.title = element_text(family = "serif",color='black', size = 14),
          axis.text = element_text(family = "serif",color='black', size = 12),
          legend.title = element_text( family = "serif",color='black',size = 14),
          legend.text = element_text( family = "serif",color='black',size = 12),
          legend.key.width  = unit(1.2, "cm"),   # 每个色块宽
          legend.key.height = unit(0.8, "cm"))+   # 每个色块高+
    scale_y_continuous(limits = c(0.1,1.1),expand = c(0,0))
  
  #交互效应图
  interaction.plot(tt$fer, tt$mow, tt$shannon, 
                   xlab = "Fertilization level", 
                   ylab = "Shannon Diversity Index", 
                   trace.label = "Mowing Frequency (mow)",
                   type = "b", pch = 1:3, col = 1:3)
#====性状雷达图====
  fun0<-read_excel("C:/Users/Yef0/Desktop/study/data_unprocessed/Fertilizing_and_Mowing/fun_main.xlsx")
  fun_t1=fun0[,2:9]
  fun_t1<-  fun_t1 %>% 
    mutate(
      mow = factor(
        mow,
        levels = unique(mow),
        labels = case_when(
          unique(mow) == 0 ~ "No mowing",
          unique(mow) == 1 ~ "Mowing once",
          unique(mow) == 2 ~ "Mowing twice",
          TRUE             ~ as.character(unique(mow))
        )
      ),
      fer = factor(
        fer,
        levels = unique(fer),
        labels = case_when(
          unique(fer) == "00" ~ "Low",
          unique(fer) == "05" ~ "Low",
          unique(fer) == "10" ~ "Moderate",
          unique(fer) == "20" ~ "Moderate",
          unique(fer) == "40" ~ "High",
          unique(fer) == "80" ~ "High",
          TRUE             ~ as.character(unique(fer))
        )
      )
    )
  
  
  fun_t2=fun_t1
  fun_t2[,3]=fun_t1[,4]/fun_t1[,3]
  fun_t2[,4]=fun_t1[,5]/fun_t1[,6]
  fun_t2[,5]=fun_t1[,5]*fun_t1[,7]
  fun_t2[,6]=fun_t1[,4]*fun_t1[,7]
  fun_t2[,7]=fun_t1[,3]*fun_t1[,6]
  fun_t2[,8]=fun_t1[,6]/fun_t1[,7]
  colnames(fun_t2)<-c("fer","mow","SLA","WRR","WNP","LWD","LAC","RCN")
  fun_m1<-fun_t1[1,]
  fun_m2<-fun_t2[1,]
  fun_m1[,3:8]<-colMeans(fun_t1[,3:8])
  fun_m2[,3:8]<-colMeans(fun_t2[,3:8])
  fun_pic1<-data.frame()
  fun_pic2<-data.frame()
  for(i in 1:dim(fun_t1)[1]){
    fun_pic1[i,1:2]=fun_t1[i,1:2]
    fun_pic2[i,1:2]=fun_t1[i,1:2]
    fun_pic1[i,3:8]=(fun_t1[i,3:8]-fun_m1[1,3:8])/fun_m1[1,3:8]*5
    fun_pic2[i,3:8]=(fun_t2[i,3:8]-fun_m2[1,3:8])/fun_m2[1,3:8]*5
  }
  pic_11<-aggregate(fun_pic1[,3:8], by=list(fun_pic1$fer), FUN=mean)
  pic_12<-aggregate(fun_pic1[,3:8], by=list(fun_pic1$mow), FUN=mean)
  pic_21<-aggregate(fun_pic2[,3:8], by=list(fun_pic2$fer), FUN=mean)
  pic_22<-aggregate(fun_pic2[,3:8], by=list(fun_pic2$mow), FUN=mean)
  pic_1<-cbind(pic_11,pic_21[,2:7])
  pic_2<-cbind(pic_12,pic_22[,2:7])
  
  #fer
  #施肥
  fun_pic=pic_1
  pic<-data.frame(rbind(1,-1,fun_pic[,2:13]))
  par(family = "serif")
  p<-radarchart(pic,
                axistype = 1,
                # pcol = c("blue","orange","green","red","purple","black"),plwd = c(3,2,2,2,2,2),
                pfcol = scales::alpha(0.3),
                pcol = c('#00A1D5FF','#374E55FF','#DF8F44FF'),plwd = c(2,2,2),
                
                plty = 1,
                cglcol = "black", cglty = 2, cglwd = 1,
                axislabcol = "black", 
                vlcex = 1.2, vlabels = colnames(pic),
                caxislabels = c(-1.0, " ",0.0," ", 1.0)
  )
  #mow
  #刈割
  fun_pic=pic_2
  pic<-data.frame(rbind(1,-1,fun_pic[,2:13]))
  par(family = "serif")
  p<-radarchart(pic,
                axistype = 1,
                # pcol = c("blue","orange","green","red","purple","black"),plwd = c(3,2,2,2,2,2),
                pfcol = scales::alpha(0.3),
                pcol = c('#B24745FF','#EFC000FF','#0073C2FF'),plwd = c(2,2,2),
                
                plty = 1,
                cglcol = "black", cglty = 2, cglwd = 1,
                axislabcol = "black", 
                vlcex = 1.2, vlabels = colnames(pic),
                caxislabels = c(-1.0, " ",0.0," ", 1.0)
  )

