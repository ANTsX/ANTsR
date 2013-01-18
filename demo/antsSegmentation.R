

library(ANTsR)
tfn<-getANTsRData('r16')
img<-antsImageRead(tfn,2,"float")
imgn3<-antsImageClone(img)
N3BiasFieldCorrection(list(img@dimension,img,imgn3,'4'))
N3BiasFieldCorrection(list(img@dimension,imgn3,imgn3,'2'))
mask<-antsImageClone(img,"unsigned int")
mask[ img > 10 ]<-1
mask[ img <= 10  ]<-0
seg_img_uint<-antsImageClone(img,"unsigned int")
Atropos( list( d = 2 , a = imgn3 , m = "[0.2,1x1]" , o = seg_img_uint , c = "[5,0]" , i = "kmeans[3]" , x = mask) )
par(mfrow=c(1,2))
plotANTsImage( imgn3 )
plotANTsImage( seg_img_uint )
countgm<-sum( seg_img_uint == 2 )/sum( mask == 1 )



isucceed<-FALSE
if ( round( countgm*100 ) == 40 ) isucceed<-TRUE
if (  isucceed ) print("SUCCESS")
if ( !isucceed ) print("FAILURE")


