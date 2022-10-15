#Contrast Stretching using 1/(1+(m/r)^e)

library(imager)
library(magick)
img1 <- load.image("/home/sankalp/Desktop/DIP/src/low_contrast_gray.jpg")
img1
plot(img1,rescale=FALSE)

m1<-as.matrix(img1,231,233)
m=mean(m1)

for (i in 1:230) 
{
  for(j in 1:232)
  {
    m1[i,j]=1/(1+(m/m1[i,j])^2.71)
  }
}

img2=as.cimg(m1)
plot(img2)

hist(img1)
hist(img2)
