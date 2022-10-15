library(imager)
img1 <- load.image("/home/sankalp/Desktop/DIP/src/test_gray.png")
img1
plot(img1)
m1<-as.matrix(img1)
plot(m1,xlim=c(0,1),type="h")

for (i in 1:2340) 
{
  for(j in 1:2340)
  {
    if(m1[i,j]>0.3 && m1[i,j]<0.6)
      m1[i,j]=1
  }
}
img2=as.cimg(m1)
plot(img2)
plot(m1,xlim=c(0,1),type="h")
