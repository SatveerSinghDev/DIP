#Histogram Equalisation
Mode<-function(a)
{
  l=as.vector(a)
  l=sort.int(l)
  l1=unique(l)
  v=c()
  i=1
  j=1
  
  while(i<=length(l1))
  {
    count=0
    while(l1[i]==l[j] && j<=length(l))
    {
      count=count+1
      #print(paste(l1[i]," ",l[j]," ",count))
      j=j+1
    }
    v=append(v, count, after = length(v))
    #print(v)
    i=i+1
  }
  return(l1[match(max(v),v)])
}

library(imager)
library(magick)
img1 <- load.image("/home/sankalp/Desktop/DIP/src/low_contrast_gray.jpg")
img2 <- image_read("/home/sankalp/Desktop/DIP/src/low_contrast_gray.jpg")
img1
plot(img1,rescale=FALSE)
hist(img1)

m1<-as.matrix(img1)
m2=matrix(0,256,1) #matrix to hold frequency of all levels 0-255
for(i in 1:230)
{
  for(j in 1:232)
  {
    m1[i,j]=m1[i,j]*255
    m2[m1[i,j]+1]=m2[m1[i,j]+1]+1 #counting frequency
  }
}
m4=matrix(0,256,1)
for(i in 1:256)
{
  m4[i]=m2[i]/length(m1)          #computing probability
}

g1=plot(0:255,m4,type="h",xlab="Intensities", ylab="Frequency/Total")


#computing s(k) values
m3=matrix(-1,256,1)
m3[1]=255*m4[1]
for(i in 2:256)
{
  m3[i]=m3[i-1]+255*m4[i]
  m3[i-1]=round(m3[i-1])
}
m3[i]=round(m3[i])

m5=matrix(0,256,1)


for(i in 0:255)
{
  cnt=0
  
  for(j in 1:256)
  {
    if(m3[j]==i)
      cnt=cnt+m2[j]
  }
  
  for(k in 1:256)
  {
    if(m3[k]==i)
      m5[k]=cnt
  }
}
  
for(i in 1:256)
  m5[i]=m5[i]/length(m1)


g2=plot(0:255,m5,type="h",xlab="Intensities",ylab="Frequency/Total")
