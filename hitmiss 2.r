library("imager")
m1= matrix(c(0,0,0,0,0,0,0,0,0,0,0,
              0,0,1,0,0,0,0,0,0,0,0,
              0,0,1,0,0,1,1,1,1,0,0,
              0,1,1,1,0,0,0,0,0,0,0,
              0,0,1,0,0,0,0,1,1,0,0,
              0,0,0,0,1,0,0,1,1,1,0,
              0,0,0,1,1,1,0,0,1,0,0,
              0,0,0,0,1,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0), byrow = TRUE, nrow=9, ncol=11)
m1
plot(as.cimg(m1))
m3<-matrix(0,nrow=nrow(m1),ncol=ncol(m1))
for(i in 1:nrow(m1))
{
  for(j in 1:ncol(m1))
  {
    m3[i,j]<-1-m1[i,j]
  }
}
m3
plot(as.cimg(m3))
m4<-matrix(0,nrow=nrow(m3)+4,ncol=ncol(m3)+4)
for(i in 1:nrow(m3)){
  for(j in 1:ncol(m3)){
    m4[i+2,j+2]=m3[i,j]
  }
}
plot(as.cimg(m4))
m2<-matrix(0,nrow=nrow(m1)+4,ncol=ncol(m1)+4)
for(i in 1:nrow(m1)){
  for(j in 1:ncol(m1)){
    m2[i+2,j+2]=m1[i,j]
  }
}
plot(as.cimg(m2))
a<-matrix(c(0,1,0,
            1,1,1,
            0,1,0),nrow=3,ncol=3)
a
resul<-matrix(0,nrow=nrow(m2),ncol=ncol(m2))

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

b<-matrix(c(1,0,1,
            0,0,0,
            1,0,1),nrow=3,ncol=3)
resul2<-matrix(0,nrow=nrow(m4),ncol=ncol(m4))
for (i in 2:(nrow(m4)-1)) 
{
  for (j in 2:(ncol(m4)-1)) 
  {
    f1 = 1
    for (k in 1:nrow(b)) {
      temp5=(k-1)-(floor(nrow(b)/2))
      for (l in 1:ncol(b)) {
        temp6=(l-1)-(floor(ncol(b)/2))
        temp7=m4[i-temp5,j-temp6]
        temp8=b[k,l]
        if( temp8!=0 && bitwAnd(temp7,temp8) == 0){
          f1=0
          break
        }
      }
    }
    if(f1 == 0)
      resul2[i,j] = 0
    else
      resul2[i,j] = 1
  }
}
plot(as.cimg(resul2))
r1<-matrix(0,nrow=nrow(m2),ncol=ncol(m2))
for(i in 2:nrow(m2)-1)
{
  for( j in 2:ncol(m2)-1)
  {
    r1[i,j]=resul[i,j] & resul2[i,j]
  }
}
r1
plot(as.cimg(r1))
