## ----eval=FALSE----------------------------------------------------------
#  library( ANTsR )
#  tem<-antsImageRead( getANTsRData("ch2") )
#  temlab2<-antsImageRead( getANTsRData("ch2a")  )
#  temporalLobeRegions = thresholdImage( temlab2, 80, 90  ) %>%
#    smoothImage( 3 )
#  otherRegions = thresholdImage( temlab2, 20, 25  ) %>%
#    smoothImage( 1.5 )

## ----message=FALSE,warning=FALSE,eval=FALSE------------------------------
#  mytem<-list( tem, temporalLobeRegions, otherRegions )
#  plot( tem, temporalLobeRegions,
#        window.overlay=c(0.1, max( temporalLobeRegions ) ) )
#  plot( tem, otherRegions, axis=3,
#        window.overlay=c(0.1, max( otherRegions ) ) )
#  plot( tem, otherRegions, axis=1,
#        window.overlay=c(0.1, max( otherRegions ) ) )

## ----eval=FALSE----------------------------------------------------------
#  mymni<-list( antsImageRead(getANTsRData("mni") ),
#              antsImageRead(getANTsRData("mnib") ),
#              antsImageRead(getANTsRData("mnia") ) )

## ----eval=FALSE----------------------------------------------------------
#  mynetworkdescriptor<-getMultivariateTemplateCoordinates(
#   mytem, mymni , convertToTal = TRUE , pvals=c(0.01,0.05) )

## ----results='asis',eval=FALSE-------------------------------------------
#  knitr::kable( mynetworkdescriptor$networks )

