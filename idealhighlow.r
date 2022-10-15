library(imager)
img=load.image("/home/satveer/Documents/sem6/DIP/IMAGES/aa.tif")
#image=load.image("/home/sakeena_shahid/Downloads/")
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
pad.fft=FFT(as.cimg(pad_img))
plot(pad.fft$real)
plot(pad.fft$imag)
pad.filter=matrix(0,p,q)
for(i in 1:nrow(pad.filter))
{
  for(j in 1:ncol(pad.filter))
  {
    dist=sqrt((i-(p/2))^2+(j-(q/2))^2)
    if(dist<=20) #dnote=20
    {
      pad.filter[i,j]=0#gaussian high pass
      #pad.filter[i,j]=1#gaussian low pass
    }
    else
      {
        pad.filter[i,j]=1#high pass
        #pad.filter[i,j]=0#low pass
      }
  }
}
plot(as.cimg(pad.filter))
#filter mul -1
#for(i in 1:nrow(pad.filter))
#{
# for(j in 1:ncol(pad.filter))
#{
# pad.filter[i,j]=pad.filter[i,j]*((-1)^(i+j))
#}
#}
plot(as.cimg(pad.filter))
#filter.fft=FFT(as.cimg(pad.filter))
#plot(filter.fft$real)
#plot(filter.fft$imag)
filter.fft=list(as.cimg(pad.filter))
final=Map("*",filter.fft,pad.fft)
names(final)=c("real","imag")
plot(final$real)
plot(final$imag)
final.fft=FFT(final$real,final$imag,inverse = TRUE)
plot(final.fft$real)
final.fft.mat=as.matrix(final.fft$real)
#real mul -1
for(i in 1:nrow(final.fft.mat))
{
  for(j in 1:ncol(final.fft.mat))
  {
    final.fft.mat[i,j]=final.fft.mat[i,j]*((-1)^(i+j))
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
#plot(img)
plot(as.cimg(new.final.fft.mat))
# threshold(as.cimg(new.final.fft.mat),"auto")%>%plot
save.image(as.cimg(new.final.fft.mat),"/home/satveer/Documents/sem6/DIP/IMAGES/idealhighfiltered.png")
