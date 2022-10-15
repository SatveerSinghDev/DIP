library(imager)
img=load.image("/home/satveer/Documents/sem6/DIP/IMAGES/aa.tif")
imgg=grayscale(img)
func=as.matrix(img)
plot(as.cimg(func))
mask=matrix(c(0,-2,0,-2,0,-2,0,-2,0),nrow =15,ncol=15,byrow=TRUE)

summask=0
for(i in 1:nrow(mask))
{
  for(j in 1:ncol(mask))
  {
    summask=summask+mask[i,j]
  }
}
msize=nrow(mask)-1
prsize=nrow(func)+msize*2
pcsize=ncol(func)+msize*2
padfun=matrix(0,prsize,pcsize)
for(i in 1:nrow(func))
{
  for(j in 1:ncol(func))
  {
    padfun[msize+i,msize+j]=func[i,j]
  }
}
plot(as.cimg(padfun))
newpadfun=matrix(0,prsize,pcsize)
for(i in 2:(nrow(padfun)-msize-1))
{
  for( j in 2:(ncol(padfun)-msize-1))
  {
    sum=0
    sumav=0
    for(k in 1:nrow(mask))
    {
      for(l in 1:ncol(mask))
      {
        sum=sum+(padfun[i+k-1,l+j-1]*mask[k,l])
        
      }
    }
    sumav=sum/summask
    newpadfun[i+1,j+1]=sumav
  }
}
plot(imgg)
plot(as.cimg(newpadfun))
cropped=matrix(0,nrow(func),ncol(func))
for(i in 1:nrow(cropped))
{
  for(j in 1:ncol(cropped))
  {
    cropped[i,j]=newpadfun[msize+i,msize+j]
  }
}
plot(as.cimg(cropped))
newimg=matrix(0,nrow(cropped),ncol(cropped))
for(i in 1:nrow(cropped))
{
  for(j in 1:ncol(cropped))
  {
    newimg[i,j]=func[i,j]+cropped[i,j]*(-1)
  }
}
plot(as.cimg(newimg))
imgafter=as.cimg(newimg)
save.image(imgafter,".png")
