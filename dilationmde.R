library("imager")
#img=load.image("/home/sakeena_shahid/Downloads/ero.tif")
img=load.image("/home/satveer/Documents/sem6/DIP/IMAGES/text.tif")
mask=matrix(c(0,1,0,1,1,1,0,1,0),3,3,byrow = TRUE) #mask1
#mask=matrix(1,3,3,byrow = TRUE)  #mask2
input=as.matrix(img)
pad.input=matrix(0,(nrow(input)+nrow(mask)-1),ncol(input)+(ncol(mask)-1))
out.pad.input=matrix(0,nrow(pad.input),ncol(pad.input))
for(i in 1:nrow(input))
{
  for(j in 1:ncol(input))
  {
    pad.input[i+floor((nrow(mask)/2)),j+floor((ncol(mask)/2))]=input[i,j]
  }
}
plot(as.cimg(pad.input))
counter=0
count=0
for(i in 1:(nrow(pad.input)-(nrow(mask)-1)))
{
  for( j in 1:(ncol(pad.input)-(ncol(mask)-1)))
  {
    for (k in 1:nrow(mask))
    {
      for(l in 1:ncol(mask))
      { 
        if(mask[k,l]==0)
        {
          counter=counter+1
        }
        else
        {
          if(mask[k,l]==pad.input[(i+k)-1,(j+l)-1])
          {
            count=count+1
          }
        }
      }
    }
    if(count>0)
    {
      out.pad.input[i+ceiling((nrow(mask)/2))-1,j+ceiling((ncol(mask)/2))-1]=1 
    }
    else
    {
      out.pad.input[i+ceiling((nrow(mask)/2))-1,j+ceiling((ncol(mask)/2))-1]=0
    }
    
    count=0
    counter=0
    
  }
}
plot(as.cimg(out.pad.input))
crop.out.pad.input=matrix(0,nrow(input),ncol(input))
for(i in 1:nrow(crop.out.pad.input))
{
  for(j in 1:ncol(crop.out.pad.input))
  {
    
    crop.out.pad.input[i,j]=out.pad.input[i+floor((nrow(mask)/2)),j+floor((ncol(mask)/2))]
  }
}
plot(as.cimg(crop.out.pad.input),main = "After dilation")
#save.image(as.cimg(crop.out.pad.input),"/home/satveer/Documents/sem6/DIP/IMAGES/afterdil.png")

