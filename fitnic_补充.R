f_curve<-function(x,y,data){
  a=cbind(data[1:dim(data)[1],x],data[1:dim(data)[1],y])
  a=data.frame(a)
  colnames(a)<-c("x","y")
  
  d=list()
  #y~x
  a1=a
  b1<-lm(y~x,a1)
  c1<-summary(b1)
  R1=round(c1$r.squared,digits=5)
  #y~sqrt(x)
  a2=a
  a2[,1]=sqrt(a2[,1])
  b2<-lm(y~x,a2)
  c2<-summary(b2)
  R2=round(c2$r.squared,digits=5)
  #y~ln(x+1)
  a3=a
  a3[,1]=log(a3[,1]+1)
  b3<-lm(y~x,a3)
  c3<-summary(b3)
  R3=round(c3$r.squared,digits=5)
  #y~x^2
  a4=a
  a4[,1]=a4[,1]^2
  b4<-lm(y~poly(x,2),a4)
  c4<-summary(b4)
  R4=round(c4$r.squared,digits=5)
  #y~1/(x+1)
  a5=a
  a5[,1]=1/(a5[,1]+1)
  b5<-lm(y~x,a5)
  c5<-summary(b5)
  R5=round(c5$r.squared,digits=5)
  
  d[[1]]=c1
  d[[2]]=c2
  d[[3]]=c3
  d[[4]]=c4
  d[[5]]=c5
  RR<-c(R1,R2,R3,R4,R5)
  for(i in 1:5){
    if(RR[i]==max(RR)){
      dd=d[[i]]
      if(i==1){
        bb="y~x"
        cc=c1$coefficients
        j=i
      }else if(i==2){
        bb="y~sqrt(x)"
        cc=c2$coefficients
        j=i
      }else if(i==3){
        bb="y~log(x+1)"
        cc=c3$coefficients
        j=i
      }else if(i==4){
        bb="y~x^2+x"
        cc=c4$coefficients
        j=i
      }else{
        bb="y~1/(x+1)"
        cc=c5$coefficients
        j=i
      }
    }
  }
  ddd=list()
  #拟合函数
  ddd$formula=bb
  #拟合函数编号
  ddd$f_num=j
  #拟合系数
  ddd$coefficient=cc
  #R-square
  ddd$Rsquare=max(RR)
  return(ddd)
}

f_sel2<-function(n,name,data){
  d1=dim(data)[1]
  b=data.frame()
  k=0
  for(i in 1:d1){
    if(data[i,n]==name){
      k=k+1
      b[k,1:dim(data)[2]]=data[i,1:dim(data)[2]]
    }
  }
  return(b)
}

f_deri<-function(coe_curve){
  x=coe_curve
  n=x[[2]]#determine the type of function
  a=numeric()
  bb=0
  if(n==1){
    b=x[[3]][2,1]
  }else if(n==2){
    b=0.5*x[[3]][2,1]
  }else if(n==3){
    b=x[[3]][2,1]
  }else if(n==4){
    b=x[[3]][2,1]
    bb=x[[3]][3,1]
  }else if(n==5){
    b=-x[[3]][2,1]
  }
  a=c(b,bb)
  return(a)
}

f_stand<-function(data0){
  dim_f=dim(data0)[2]
  fmean<-colMeans(data0[1:dim(data0)[1],1:dim(data0)[2]])
  fsd<-apply(data0[1:dim(data0)[1],1:dim(data0)[2]],2,sd)
  data1=data0
  for(i in 1:dim(data0)[2]){
    data1[,i]=(data0[,i]-fmean[i])/fsd[i]
  }
  min_f<-apply(data1[,1:dim(data0)[2]],2,min)
  max_f<-apply(data1[,1:dim(data0)[2]],2,max)
  for(i in 1:dim(data0)[2]){
    data1[,i]=(data1[,i]-min_f[i])/(max_f[i]-min_f[i])
  }
  return(data1)
}

f_inter1<-function(data){
  d1=dim(data)[1]
  d2=dim(data)[2]
  inter<-data.frame()
  k=0
  name<-colnames(data)
  cc<-character()
  for(i in 1:(d2-1)){
    for(j in (i+1):d2){
      k=k+1
      inter[1:d1,k]=data[1:d1,i]*data[1:d1,j]
      cc[k]<-paste(name[i],name[j],"1",sep="_")
    }
  }
  colnames(inter)<-cc
  return(inter)
}

f_inter2<-function(data){
  d1=dim(data)[1]
  d2=dim(data)[2]
  inter<-data.frame()
  k=0
  name<-colnames(data)
  cc<-character()
  for(i in 1:(d2-1)){
    for(j in (i+1):d2){
      k=k+1
      inter[1:d1,k]=data[1:d1,j]/data[1:d1,i]
      cc[k]<-paste(name[j],name[i],"2",sep="_")
    }
  }
  colnames(inter)<-cc
  return(inter)
}

f_dydx<-function(coe_curve,data){
  d=coe_curve[[2]]
  dd=coe_curve[[3]]
  if(d==1){
    dy=dd[2,1]
  }else if(d==2){
    dy=dd[2,1]/sqrt(data+1)
  }else if(d==3){
    dy=dd[2,1]/(data+1)
  }else if(d==4){
    dy=dd[2,1]*data+dd[3,1]
  }else if(d==5){
    dy=-dd[2,1]/(data^2)
  }
}

f_cos<-function(a1,a2){
  b=(a1-a2)^2
  a=mean(b[1,])
  print(a)
  return(a)
}

f_replace<-function(x){
  if(x=="白莲蒿"){
    x="AGM"
  }else if(x=="多毛"){
    x="SSC"
  }else if(x=="甘菊"){
    x="CLA"
  }else if(x=="甘青"){
    x="SPR"
  }else if(x=="苔草"){
    x="CARI"
  }else if(x=="猪毛菜"){
    x="SCO"
  }else if(x=="风毛菊"){
    x="SJA"
  }else if(x=="二裂"){
    x="PBI"
  }else{
    x="sp"
  }
  return(x)
}

