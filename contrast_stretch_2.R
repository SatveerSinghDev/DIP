#Contrast Stretching using (r-min)/(max-min)

library(imager)
library(magick)
img1 <- load.image("/home/sankalp/Desktop/DIP/src/low_contrast_gray.jpg")
img1
plot(img1,rescale=FALSE)

m1<-as.matrix(img1,231,233)

Max=max(m1)
Min=min(m1)
d=Max-Min

for (i in 1:230) 
{
  for(j in 1:232)
  {
    m1[i,j]=(m1[i,j]-Min)/d
  }
}
img2=as.cimg(m1)
plot(img2)

hist(img1)
hist(img2)
