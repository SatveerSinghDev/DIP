img1 <- load.image("/home/sankalp/Desktop/DIP/src/test_gray.png")
img1
plot(img1)
m1<-as.matrix(img1)
for (i in 1:2340) 
{
  for(j in 1:2340)
  {
    if(m1[i,j]>0.3 && m1[i,j]<0.6)
      m1[i,j]=1
    else
      m1[i,j]=0
  }
}
img2=as.cimg(m1)
plot(img2)
