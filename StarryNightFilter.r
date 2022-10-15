library(imager)
filter.func<-function(fofx,img.filter)
{
  fofx<-as.matrix(fofx)
  #padding image with a boundary of 0's
  padded.fofx<-matrix(0,nrow = nrow(fofx)+2,ncol = ncol(fofx)+2)
  for (i in 1:nrow(fofx)) {
    for (j in 1:ncol(fofx)) {
      padded.fofx[i+1,j+1] = fofx[i,j]
    }
  }
  
  #filling the 0's padding with neighbouring pixels
  for (i in 1:ncol(padded.fofx)) {
    padded.fofx[1,i] = padded.fofx[2,i]
    padded.fofx[nrow(padded.fofx),i] = padded.fofx[nrow(padded.fofx)-1,i]
  }
  
  for (i in 1:nrow(padded.fofx)) {
    padded.fofx[i,1] = padded.fofx[i,2]
    padded.fofx[i,ncol(padded.fofx)] = padded.fofx[i,ncol(padded.fofx)-1]
  }
  
  #function to calculate sum of products
  sop<- function(x,y) {
    val<-0
    for (i in -1:1) {
      for (j in -1:1) {
        val = val + padded.fofx[x+i,y+j] * img.filter[2+i,2+j]
      }
    }
    return(val)
  }
  
  final.fofx<-matrix(0,nrow = nrow(fofx)+2,ncol = ncol(fofx)+2)
  #applying filter on padded image
  for (i in 2:(nrow(padded.fofx)-1)) {
    for (j in 2:(ncol(padded.fofx)-1)) {
      final.fofx[i,j] = sop(i,j)
    }
  }
  
  plot(as.cimg(final.fofx))
}
avg<-matrix(c(0,2,0, 2,4,2, 0,2,0),3,3)/16
laplacian<-matrix(c(-1,2,-1, -1,2,-1, -1,2,-1),3,3)

lena.img<-load.image("/home/nitish/Desktop/R_stuff/woman_blonde.tif")
filtered.img<-filter.func(lena.img,laplacian)
threshold(filtered.img,"90%")%>%plot
