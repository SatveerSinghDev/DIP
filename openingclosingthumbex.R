library("imager")
dilation=function(input,mask){
  pad.input=matrix(0,(nrow(input)+nrow(mask)-1),ncol(input)+(ncol(mask)-1))
  out.pad.input=matrix(0,nrow(pad.input),ncol(pad.input))
  for(i in 1:nrow(input))
  {
    for(j in 1:ncol(input))
    {
      pad.input[i+floor((nrow(mask)/2)),j+floor((ncol(mask)/2))]=input[i,j]
    }
  }
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
img=load.image("/home/satveer/Documents/sem6/DIP/IMAGES/thumb.tif")
mask=matrix(1,3,3,byrow = TRUE)  #mask2
input=as.matrix(img)
plot(as.cimg(input),main="INPUT IMAGE")
eroded.image=erosion(input,mask)
plot(as.cimg(eroded.image),main="ERODED IMAGE")
#save.image(as.cimg(eroded.image),"/home/satveer/Documents/sem6/DIP/IMAGES/eroded.png")
opening.image=dilation(eroded.image,mask)
plot(as.cimg(opening.image),main="OPENING IMAGE")
#save.image(as.cimg(opening.image),"/home/satveer/Documents/sem6/DIP/IMAGES/opening.png")
opening.dila=dilation(opening.image,mask)
plot(as.cimg(opening.dila),main="DILATION OPENING")
#save.image(as.cimg(opening.dila),"/home/satveer/Documents/sem6/DIP/IMAGES/dilationopen.png")
closing.dil=erosion(opening.dila,mask)
plot(as.cimg(closing.dil),main="CLOSING OF OPENING")
#save.image(as.cimg(closing.dil),"/home/satveer/Documents/sem6/DIP/IMAGES/closofopn.png")  
