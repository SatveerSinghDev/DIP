library("imager")
erosion=function(input,mask){
  pad.input=matrix(0,(nrow(input)+nrow(mask)-1),ncol(input)+(ncol(mask)-1))
  out.pad.input=matrix(0,nrow(pad.input),ncol(pad.input))
  for(i in 1:nrow(input))
  {
    for(j in 1:ncol(input))
    {
      pad.input[i+floor((nrow(mask)/2)),j+floor((ncol(mask)/2))]=input[i,j]
    }
  }
  count=0
  for(i in 1:(nrow(pad.input)-(nrow(mask)-1)))
  {
    for( j in 1:(ncol(pad.input)-(ncol(mask)-1)))
    {
      for (k in 1:nrow(mask))
      {
        for(l in 1:ncol(mask))
        { 
          if(mask[k,l]==pad.input[(i+k)-1,(j+l)-1])
          {
            count=count+1
            
          }
          else
          {
            count=0
          }
        }
        
        
      }
      if(count==length(mask))
      {
        out.pad.input[i+ceiling((nrow(mask)/2))-1,j+ceiling((ncol(mask)/2))-1]=1
      }
      else
      {
        out.pad.input[i+ceiling((nrow(mask)/2))-1,j+ceiling((ncol(mask)/2))-1]=0
      }
      
      count=0
    }
  }
  crop.out.pad.input=matrix(0,nrow(input),ncol(input))
  for(i in 1:nrow(crop.out.pad.input))
  {
    for(j in 1:ncol(crop.out.pad.input))
    {
      
      crop.out.pad.input[i,j]=out.pad.input[i+floor((nrow(mask)/2)),j+floor((ncol(mask)/2))]
    }
  }
  return(crop.out.pad.input)
  
}
#img=load.image("/home/sakeena_shahid/Downloads/ero.tif")
img=load.image("/home/satveer/Documents/sem6/DIP/IMAGES/bound.tif")
mask=matrix(1,3,3,byrow = TRUE)
input=as.matrix(img)
plot(as.cimg(input),main="INPUT IMAGE")

#boundary extraction
eroded.image=erosion(input,mask)
boundary.image=matrix(0,nrow(input),ncol(input))
for(i in 1:nrow(boundary.image))
{
  for(j in 1:ncol(boundary.image))
  {
    boundary.image[i,j]=input[i,j]-eroded.image[i,j]
  }
}
plot(as.cimg(boundary.image),main = "BOUNDARY IMAGE")
#save.image(as.cimg(boundary.image),"/home/satveer/Document/sem6/DIP/IMAGES/extractboundary.png")
