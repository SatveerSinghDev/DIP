library("imager")
image1<-grayscale(load.image("C:\Users\1999s\Desktop\aa.png"))
plot(image1)
  m1<-as.matrix(image1)
m1
m2<-matrix(0,nrow=nrow(m1)+4,ncol=ncol(m1)+4)
for(i in 1:nrow(m1)){
  for(j in 1:ncol(m1)){
    m2[i+2,j+2]=m1[i,j]
  }
}

a<-matrix(c(1,1,1,1,1,1,1,1,1),nrow=3,ncol=3)
a
resul<-matrix(0,nrow=nrow(m2),ncol=ncol(m2))
a1<-matrix(0,nrow=nrow(m2),ncol=ncol(m2))
resul2<-matrix(0,nrow=nrow(m2),ncol=ncol(m2))
for(i in 2:(nrow(m2)-1))
{
  for(j in 2:(ncol(m2)-1))
  {
    a2=a[1,1]*m2[i-1,j-1]  
    b=a[1,2]*m2[i-1,j]  
    c=a[1,3]*m2[i-1,j+1]  
    d=a[2,1]*m2[i,j-1]  
    e=a[2,2]*m2[i,j]  
    f=a[2,3]*m2[i,j+1] 
    g=a[3,1]*m2[i+1,j-1]  
    h=a[3,2]*m2[i+1,j]  
    k=a[3,3]*m2[i+1,j+1]
    a1=matrix(c(a2,b,c,d,e,f,g,h,k))
    resul[i,j]=min(a1);
  }
}
plot(as.cimg(resul))
for(i in 2:(nrow(resul)-1))
{
  for(j in 2:(ncol(resul)-1))
  {
    a2=a[1,1]*resul[i-1,j-1]  
    b=a[1,2]*resul[i-1,j]  
    c=a[1,3]*resul[i-1,j+1]  
    d=a[2,1]*resul[i,j-1]  
    e=a[2,2]*resul[i,j]  
    f=a[2,3]*resul[i,j+1] 
    g=a[3,1]*resul[i+1,j-1]  
    h=a[3,2]*resul[i+1,j]  
    k=a[3,3]*resul[i+1,j+1]
    a1=matrix(c(a2,b,c,d,e,f,g,h,k))
    resul2[i,j]=max(a1);
  }
}
plot(as.cimg(resul2))