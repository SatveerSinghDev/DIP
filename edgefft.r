library(imager)
img=load.image("/home/satveer/Documents/sem6/DIP/IMAGES/home.tif")
img
plot(img,main="Original image")
img_mat=as.matrix(img)
rows=nrow(img_mat)
cols=ncol(img_mat)
nr=log2(rows)
nc=log2(cols)
p=2^ceiling(nr)
q=2^ceiling(nc)
pad_img=matrix(0,p,q)
for(i in 1:nrow(img_mat))
{
  for(j in 1:ncol(img_mat))
  {
    pad_img[i,j]=img_mat[i,j]
  }
}
plot(as.cimg(pad_img),main="Padded image")
#mutiply (-1)
for(i in 1:nrow(pad_img))
{
  for(j in 1:ncol(pad_img))
  {
    pad_img[i,j]=pad_img[i,j]*((-1)^(i+j))
  }
}
plot(as.cimg(pad_img),main="After center")
pad.fft=FFT(as.cimg(pad_img),inverse = FALSE)
plot(pad.fft$real)
plot(pad.fft$imag)
#padreal=as.matrix(pad.fft$real)
#padimg=as.matrix(pad.fft$imag)
#pad.fft.mat=as.matrix(pad.fft)
#plot(as.cimg(pad.fft.mat)%>%abs,main="After center")
filter=matrix(c(-1,0,1,-2,0,2,-1,0,1),3,3)
#plot(as.cimg(filter))
pad.filter=matrix(0,p,q)
for(i in 1:nrow(filter))
{
  for(j in 1:ncol(filter))
  {
    pad.filter[i,j]=filter[i,j]
  }
}
#filter mul -1
for(i in 1:nrow(pad.filter))
{
  for(j in 1:ncol(pad.filter))
  {
    pad.filter[i,j]=pad.filter[i,j]*((-1)^(i+j))
  }
}

plot(as.cimg(pad.filter))
filter.fft=FFT(as.cimg(pad.filter),inverse = FALSE)
plot(filter.fft$real)
plot(filter.fft$imag)
#filter.fft$real=filter.fft$real*0
#imaggr=as.matrix(filter.fft$real)
#imagg=as.matrix(filter.fft$imag)
#for(i in 1:nrow(imagg))
#{
 # for(j in 1:ncol(imagg))
  #{
   # imagg[i,j]=imagg[i,j]*((-1)^(i+j))
    #imaggr[i,j]=imaggr[i,j]*((-1)^(i+j))
  #}
#}
#filter.fft$imag=as.cimg(imagg)
#filter.fft$real=as.cimg(imaggr)
#fftreal=as.matrix(filter.fft$real)
#fftimg=as.matrix(filter.fft$imag)
final=Map("*",filter.fft,pad.fft)
#matr=as.matrix(final$real)
#matimg=as.matrix(final$imag)
plot(final$real)
plot(final$imag)
#plot(as.cimg(abs(filter.fft)*abs(pad.fft)))
final.fft=FFT(final$real,final$imag,inverse = TRUE)

plot(final.fft$real)
final.fft.mat=as.matrix(final.fft$real)

#final.fft.mat.im=as.matrix(final.fft$imag)
#real mul -1
for(i in 1:nrow(final.fft.mat))
{
 for(j in 1:ncol(final.fft.mat))
  {
    final.fft.mat[i,j]=final.fft.mat[i,j]*((-1)^(i+j))
    #final.fft.mat.im[i,j]=final.fft.mat.im[i,j]*((-1)^(i+j))
  }
}
plot(as.cimg(final.fft.mat))
#plot(as.cimg(final.fft.mat.im))
new.final.fft.mat=matrix(0,rows,cols)
for(i in 1:nrow(new.final.fft.mat))
{
  for(j in 1:ncol(new.final.fft.mat))
  {
    new.final.fft.mat[i,j]=final.fft.mat[i,j]
    
  }
}
plot(img)
plot(as.cimg(new.final.fft.mat))
save.image(as.cimg(new.final.fft.mat),"/home/satveer/Documents/sem6/DIP/IMAGES/filtered.png")
finalthres=threshold(as.cimg(new.final.fft.mat),"20%")%>%plot
save.image(as.cimg(finalthres),"/home/satveer/Documents/sem6/DIP/IMAGES/thresh.png")
