library(imager)

img1=load.image("/home/sankalp/Desktop/DIP/src/dollar.jpg")
img2=grayscale(img1)
save.image(img2,"/home/sankalp/Desktop/DIP/src/dollar_gray.jpg")

m1=as.matrix(img2)
m2=matrix(0,nrow(m1),ncol(m1))

for(i in 1:nrow(m1))
{
  for(j in 1:ncol(m1))
  {
    m1[i,j]=m1[i,j]*255
    m1[i,j]=round(m1[i,j])
    m2[i,j]=bitwAnd(m1[i,j], 128)
  }
}

img2=as.cimg(m2)
plot(img2)
