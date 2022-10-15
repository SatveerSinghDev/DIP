library(imager)
image=load.image("/home/sakeena_shahid/Downloads/home.tif")
plot(image)
image
image_mat=as.matrix(image)
rows=nrow(image_mat)
cols=ncol(image_mat)
nr=log2(rows)
nc=log2(cols)
pad_rows=2^ceiling(nr) 
pad_cols=2^ceiling(nc)
difr=pad_rows-rows
difc=pad_cols-cols
pr=difr/2
pc=difc/2
pad_image_mat=matrix(0,pad_rows,pad_cols)
for(i in 1:nrow(image_mat))
{
  for(j in 1:ncol(image_mat))
  {
    pad_image_mat[pr+i,pc+j]=image_mat[i,j]
  }
}
plot(as.cimg(pad_image_mat))
fouriers=FFT(as.cimg(pad_image_mat),inverse=FALSE)
#is.list(fouriers)
#save.image(fouriers$real,"real.png")
#save.image(fouriers$imag,"img.png")
#is.cimg(fouriers$real)

##THIS IS NOT CORRECT. YOU HAVE TO PLOT THE INVERSE OF FFT. 
## BY TAKIN G INVERSE YOU ARE PLOTTING THE IMAGE BACK IN SPATIAL DOMAIN

## COMMENTING SOME LINES OF YOUR CODE NOW......
#plot(fouriers$real)
#plot(fouriers$imag)

## FOR inverse FFT, use the following:
finalim <- FFT(fouriers$real, fouriers$imag, inverse =TRUE)
plot(finalim$real)
plot(finalim$imag)

## after obtaining the image, you should crop out the extra information that you added.
