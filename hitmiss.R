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
img=load.image("/home/satveer/Documents/sem6/DIP/IMAGES/thumb.tif")
input=as.matrix(img)
input.comp=matrix(0,nrow(input),ncol(input))
hitmiss=matrix(0,nrow(input),ncol(input))
#b1=matrix(1,3,3,byrow = TRUE)
b1=matrix(c(0,1,0,1,1,1,0,1,0),3,3,byrow = TRUE)
b2=matrix(c(1,0,1,0,0,0,1,0,1),3,3,byrow = TRUE)
plot(as.cimg(input),main = "Input Image")
erosion.image.b1=erosion(input,b1)
plot(as.cimg(erosion.image.b1),main = "Eroded with B1")
dilation.image.b2=dilation(input,b2)
plot(as.cimg(dilation.image.b2),main = "Dilation with B2")
for(i in 1:nrow(hitmiss))
{
  for(j in 1:ncol(hitmiss))
  {
    
    hitmiss[i,j]=erosion.image.b1[i,j]-dilation.image.b2[i,j]
    #if(hitmiss[i,j]<0) #without checking negative value it also give some output
    #{
        #use one of them
    #hitmiss[i,j]=0 #in this my image is pure black
     #hitmiss[i,j]=0-hitmiss[i,j] #i am making postive value and it is giving some output 
    
    #}
    
  }
}
plot(as.cimg(hitmiss),main ="Hit and MISS Transformation")
# save.image(as.cimg(hitmiss),"/home/satveer/Documents/sem6/DIP/IMAGES/negvalpos.png")
