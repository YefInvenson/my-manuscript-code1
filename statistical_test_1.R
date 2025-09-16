#====程序包====
library(readxl)#读取excel功能包
#====
fit_analy<-read_excel("C:/Users/Yef0/Desktop/study/data_unprocessed/Fertilizing_and_Mowing/fitness_biodiv.xlsx")
df=ND_AFD_D
df$fer <- as.character(df$fer)
for(i in 1:dim(df)[1]){
  if(df[i,2]==0){
    df[i,2]="Low"
  }else if(df[i,2]==5){
    df[i,2]="Low"
  }else if(df[i,2]==10){
    df[i,2]="Moderate"
  }else if(df[i,2]==20){
    df[i,2]="Moderate"
  }else if(df[i,2]==40){
    df[i,2]="High"
  }else if(df[i,2]==80){
    df[i,2]="High"
  }
}


#正态分布检验
shapiro.test(df$ND1)
shapiro.test(df$fitness_x)
shapiro.test(df$ND2)
shapiro.test(df$fitness_y)
#方差齐性检验
df$fer <- as.factor(df$fer)
df$mow <- as.factor(df$mow)

leveneTest(ND1 ~ fer, data = df)  # 仅测试 fer 的分组
leveneTest(ND1 ~ mow, data = df)  # 仅测试 mow 的分组
leveneTest(fitness_x ~ fer, data = df)  # 仅测试 fer 的分组
leveneTest(fitness_y ~ mow, data = df)  # 仅测试 mow 的分组


k=4   # 4:ND1,NDx
      # 5:ND2,NDy
      # 8:ND5,ND
      # 9:fitness_x,
      # 10:fitness_y,
      # 11,AFD
  mean0<- aggregate(df[,k], by=list(df$fer), FUN=mean)
  sd <- aggregate(df[,k], by=list(df$fer), FUN=sd)
  len <- aggregate(df[,k], by=list(df$fer), FUN=length)
  
  # aov1<-aov(ND1~fer,df)
  # aov2<-aov(ND2~fer,df)
  # aov3<-aov(ND1~mow,df)
  # aov4<-aov(ND2~mow,df)
  # 
  # aov5<-aov(fitness_x~fer,df)
  # aov6<-aov(fitness_y~fer,df)
  # aov7<-aov(fitness_x~mow,df)
  # aov8<-aov(fitness_y~mow,df)
  # 
  # a1_results<-summary(aov1)
  # a2_results<-summary(aov2)
  # a3_results<-summary(aov3)
  # a4_results<-summary(aov4)
  
  #不满足正态分布的数据，不应使用基于正态分布假设的ANOVA方差分析，可以采用Kruskal-Wallis检验
  kruskal.test(ND1 ~ fer, data = df)  
  #不满足方差齐性的数据，不应使用基于方差齐性假设的ANOVA方差分析，可以采用Welch's 稳健检验
  oneway.test(ND1 ~ fer, data = df, var.equal = FALSE)
  
  kru1<- kruskal.test(ND1 ~ fer, data = df) 
  kru2<- kruskal.test(ND2 ~ fer, data = df)
  kru3<- kruskal.test(ND1 ~ mow, data = df)
  kru4<- kruskal.test(ND2 ~ mow, data = df)
  kru5<- kruskal.test(fitness_x ~ fer, data = df) 
  kru6<- kruskal.test(fitness_y ~ fer, data = df)
  kru7<- kruskal.test(fitness_x ~ mow, data = df)
  kru8<- kruskal.test(fitness_y ~ mow, data = df)

#====
  pre0<-read_excel("C:/Users/Yef0/Desktop/study/data_unprocessed/Fertilizing_and_Mowing/pre00.xlsx")
  pre0<-as.data.frame(pre0)
  #5:dudx
  #6:dNdx
  #7:dvdy
  #8:dNdy
  
  #11:rcov
  #11:rh
  #13:rabu
  #14:rbio
  #15:tcov
  #16:th
  #17:tabu
  #18:tbio
  nnn=10  
  for( i in 1:4){
    for(j in 1:8){
      if(j==1){
        yy="tcov"
      }else if(j==2){
        yy="th"
      }else if(j==3){
        yy="tabu"
      }else if(j==4){
        yy="tbio"
      }
      ijk=8*(i-1)+j
      model[[ijk]] <- lm(pre0[,nnn+j] ~ pre0[,4+i], data =pre0)
    }
  }
  summary(model[[]])