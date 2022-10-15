library("imager")
image=round(grayscale(load.image("C:\Users\1999s\Desktop.png")))

plot(image)
image.mat<-as.matrix(image)

pad.image<-matrix(0,nrow=nrow(image.mat)+4,ncol=ncol(image.mat)+4)
for(i in 1:nrow(image.mat)){
  for(j in 1:ncol(image.mat)){
    pad.image[i+2,j+2]=image.mat[i,j]
  }
}


plot(as.cimg(pad.image))
stElement=matrix(c(1,1,1,0,0,0,0,1,0),nrow=3,ncol=3)
erodedImg=pad.image

for (i in 2:(nrow(pad.image)-1)) 
  {
  for (j in 2:(ncol(pad.image)-1)) 
       {
    f = TRUE
    for (k in 1:nrow(stElement)) {
      temp1=(k-1)-(floor(nrow(stElement)/2))
      for (l in 1:ncol(stElement)) {
        temp2=(l-1)-(floor(ncol(stElement)/2))
        
        temp3=pad.image[i-temp1,j-temp2]
        temp4=stElement[k,l]
        
        if( temp4!=0 && bitwAnd(temp3,temp4) == 0){
          f = FALSE
          break
        }
      }
    }
    if(f == FALSE)
      erodedImg[i,j] = 0
    else
      erodedImg[i,j] = 1
  }
}

plot(as.cimg(erodedImg))
