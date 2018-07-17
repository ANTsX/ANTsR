## ----applymodel----------------------------------------------------------
mmseg<-mrvnrfs.predict( rfm$rflist, list(list(ll2$limg)),
  timask, rad=rad, multiResSchedule=mr, voxchunk=500  )

## ----vizmodel,echo=FALSE-------------------------------------------------
invisible( plot(mmseg$seg[[1]], window.img=c(0,max(mmseg$seg[[1]]) ) ) )

## ----gtseg,echo=FALSE----------------------------------------------------
invisible( plot( seg2, window.img=c(0,max(seg2) ) ) )

## ----vizprob,echo=FALSE--------------------------------------------------
invisible( plot( mmseg$probs[[1]][[ max(mmseg$seg[[1]])  ]] ) )

## ----eval----------------------------------------------------------------
dicenumer<-sum(  mmseg$seg[[1]] == max(mmseg$seg[[1]]) & seg2 == max(seg2) )
dicedenom<-sum( mmseg$seg[[1]] == max(mmseg$seg[[1]]) ) + sum( seg2 == max(seg2)  )
dice <- 2.0 * dicenumer / dicedenom
if ( dice < 0.87 ) stop("suggests performance regression in mrvnrfs")

