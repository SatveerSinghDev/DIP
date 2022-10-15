library(imager)
img <- load.image("/home/nitish/Desktop/R_stuff/aa.tif")
plot(img)
#-------------------VIEWING THE SPECTRUM ONLY-------------------#
#------------------ FOR HIGH/LOW PASS FILTERING, SEE AFTER THIS SECTION-------------------#

#~~~~~~~~~method 1 for viewing~~~~~~~~~~~~~~~~~~#
# Application of abs() and log is just to view the power spectrum of image in appropriate form.
#FFT of image using fft()
img.fft <- fft(img)%>%as.matrix
img.fft.abs <- lapply(img.fft,abs)
img.fft.abs.log <- lapply(img.fft.abs,log)
plot(as.cimg(as.matrix(img.fft.abs.log)))
#since real and imaginary part are in the form real+(i*img), processing with real part is tough. Hence we use FFT()
#~~~~~~~~~~~~method 1 unsuccesful for me! ;(~~~~~~~~~~~~~~~#

#~~~~~~~~~~method 2 for viewing~~~~~~~~~~~~~~~~~#
##FFT of image using FFT(), produces a list containing real and imaginary part
img.FFT <- FFT(img)
class(img.FFT$real)
log((sqrt(img.FFT$real^2 + img.FFT$imag^2))) %>% plot(main = "Power spectrum in FFT")

#finding absolute values for plotting 
img.FFT.abs <- lapply(img.FFT, abs)

#to see significant plot, we log the values.
img.FFT.abs.log <- lapply(img.FFT.abs, log2)
plot(img.FFT.abs.log$real,main = "real log part of image FFT")
plot(img.FFT.abs.log$imag,main = "imaginary log part of image FFT")

#--Shift the real part image for centering main frequency. Use boundary_conditions = 2 for pattern repeat in imshift.
abc<-imshift(img.FFT.abs.log$real,256,256,boundary_conditions = 2)%>%plot(main= "shifted FFT real")
#~~~~~~~~~~~~method 2 succesful for me! ;)~~~~~~~~~~~~~~~#

#-----------------------VIEWING SECTION ENDS HERE---------------------------#

#------------unsuccessful attempts for High/Low pass filtering---------#
#try 1
lp2<-function(x) {if(abs(x)<200) print(x) }
img.FFT <- FFT(img)
img.FFT <- lapply(img.FFT,lp2)
final<- FFT(img.FFT$real,img.FFT$imag,inverse = TRUE)
plot(img.FFT$real)

##try 2
img.FFT <- FFT(img)
for (i in 1:length(img.FFT$real)) {
  if(img.FFT$real[i] > 0)
    img.FFT$real[i] = 0
}
for (i in 1:length(img.FFT$imag)) {
  if(img.FFT$imag[i] > 0)
    img.FFT$imag[i] = 0
}
##somewhat worked but data type error occured
#----------------;(---------------------------#


#--------Finally something worked, didn't wanted to use loops but had to at the end;P-----#
# IDEAL LOW PASS
img.fft <- fft(img)
class(img.fft)
img.fft.matrix <- as.matrix(img.fft)
class(img.fft.matrix)
for (i in 1:nrow(img.fft.matrix)) {
  for (j in 1:ncol(img.fft.matrix)) {
    if(abs(img.fft.matrix[i,j]) < 200)
      img.fft.matrix[i,j] = 0
  }
}
final.img <- fft(img.fft.matrix, inverse = TRUE)
final.img <- as.cimg(final.img)%>%abs
class(final.img)
plot(final.img,main = "Ideal Low pass filtered image")
save.image(final.img,"/home/nitish/Desktop/R_stuff/aaLPfiltered.png")
###

# IDEAL HIGH PASS (only the sign inside for-loop comparison is changed)

img.fft <- fft(img)
class(img.fft)
img.fft.matrix <- as.matrix(img.fft)
class(img.fft.matrix)
for (i in 1:nrow(img.fft.matrix)) {
  for (j in 1:ncol(img.fft.matrix)) {
    if(abs(img.fft.matrix[i,j]) > 200)
      img.fft.matrix[i,j] = 0
  }
}
final.img <- fft(img.fft.matrix, inverse = TRUE)
final.img <- as.cimg(final.img)%>%abs
class(final.img)
plot(final.img,main = "Ideal High pass filtered image")
save.image(final.img,"/home/nitish/Desktop/R_stuff/aaHPfiltered.png")
###

# Since the first value of the matrix(i.e at 1,1) is the DC component of the image, it is the sum of all pixel values.
# To find the average we just divide fft[1,1] by total no. of pixels.
img.fft<-fft(img)
avg.img <- abs(img.fft[1,1]) / (nrow(img) * ncol(img))
avg.img
