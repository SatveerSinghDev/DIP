library("imager")
image1<-round(grayscale(load.image("C:\Users\1999s\Desktop.png")))
plot(image1)
m1<-as.matrix(image1)
m1
m2<-matrix(0,nrow=nrow(m1)+4,ncol=ncol(m1)+4)
for(i in 1:nrow(m1)){
  for(j in 1:ncol(m1)){
    m2[i+2,j+2]=m1[i,j]
  }
}
plot(as.cimg(m2))
a<-matrix(c(1,1,1,0,1,1,0,0,1),nrow=3,ncol=3)
a
resul<-matrix(0,nrow=nrow(m2),ncol=ncol(m2))
a1<-matrix(0,nrow=nrow(m2),ncol=ncol(m2))

for (i in 2:(nrow(m2)-1)) 
{
  for (j in 2:(ncol(m2)-1)) 
  {
    f = 1
    for (k in 1:nrow(a)) {
      temp1=(k-1)-(floor(nrow(a)/2))
      for (l in 1:ncol(a)) {
        temp2=(l-1)-(floor(ncol(a)/2))
        temp3=m2[i-temp1,j-temp2]
        temp4=a[k,l]
        if( temp4!=0 && bitwAnd(temp3,temp4) == 0){
          f=0
          break
        }
      }
    }
    if(f == 0)
      resul[i,j] = 0
    else
      resul[i,j] = 1
  }
}
plot(as.cimg(resul))
bound<-matrix(0,nrow=nrow(m2),ncol=ncol(m2))
for(i in 2:(nrow(m2)-1))
{
  for(j in 2:(ncol(m2)-1))
  {
    bound[i,j]=m2[i,j]-resul[i,j]
  }
}
plot(as.cimg(bound))
