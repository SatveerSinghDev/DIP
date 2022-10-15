library(imager)
lena.img<-load.image("/home/nitish/Desktop/R_stuff/woman_blonde.tif")
dim(lena.img)
lena.img<-as.matrix(lena.img)
#kernel.mat<-matrix(c(1,4,7,4,1,4,16,26,16,4,7,26,41,26,7,4,16,26,16,4,1,4,7,4,1),nrow = 5,ncol = 5)/273
kernel.mat<-matrix(c(1,4,6,4,1,4,16,24,16,4,6,24,-476,24,6,4,16,24,16,4,1,4,6,4,1),nrow = 5,ncol = 5)/-256

lena.row<-nrow(lena.img)
lena.col<-ncol(lena.img)
kernel.row<-nrow(kernel.mat)
kernel.col<-ncol(kernel.mat)

#equalising dimensions of image and kernel for convolution multiplication
for (i in 1:(lena.row-1)) {
  kernel.mat<-rbind(kernel.mat,rep(0,times = ncol(kernel.mat)))
}
for (i in 1:(lena.col-1)) {
  kernel.mat<-cbind(kernel.mat,rep(0,times = nrow(kernel.mat)))
}

for (i in 1:(kernel.row-1)) {
  lena.img<-rbind(lena.img,rep(0,times = ncol(lena.img)))
}
for (i in 1:(kernel.col-1)) {
  lena.img<-cbind(lena.img,rep(0,times = nrow(lena.img)))
}

#FFT
ff.lena<-FFT(as.cimg(lena.img))
ff.kernel<-FFT(as.cimg(kernel.mat))

#element by element multiplication of both real and imaginary part
result.final<-Map('*',ff.lena,ff.kernel)

#Inverse fourier transform to get back the resulting image
result.final.inv<-FFT(result.final$real,result.final$imag,inverse = TRUE)

#Saving the real and imaginary(part) images
save.image(result.final.inv$real,"/home/nitish/Desktop/R_stuff/FrequencyDomRealPart.png")
save.image(result.final.inv$imag,"/home/nitish/Desktop/R_stuff/FrequencyDomImagPart.png")

