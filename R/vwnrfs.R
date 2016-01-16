#' voxelwise neighborhood random forest segmentation and prediction
#'
#' Represents feature images as a neighborhood and uses the features
#' to build a random forest prediction from an image population
#'
#' @param y list of training label images, can be a factor or numeric vector
#' this can also be a regular old vector
#' @param x a list of lists where each list contains feature images
#' @param labelmasks a list of masks where each mask defines the image space
#' for the given list. that is, the nth mask indexes the nth feature set.
#' multi-label masks will try to balance sampling for each label.
#' @param rad vector of dimensionality d define nhood radius
#' @param nsamples (per subject to enter training)
#' @param ntrees (for the random forest model)
#' @param asFactors boolean - treat the y entries as factors
#' @return list a 4-list with the rf model, training vector, feature matrix
#' and the random mask
#' @author Avants BB, Tustison NJ, Pustina D
#'
#' @examples
#'
#' mask<-makeImage( c(10,10), 0 )
#' mask[ 3:6, 3:6 ]<-1
#' mask[ 5, 5:6]<-2
#' ilist<-list()
#' lablist<-list()
#' masklist<-list()
#' inds<-1:50
#' scl<-0.33 # a noise parameter
#' for ( predtype in c("label","scalar") )
#' {
#' for ( i in inds ) {
#'   img<-antsImageClone(mask)
#'   imgb<-antsImageClone(mask)
#'   limg<-antsImageClone(mask)
#'   if ( predtype == "label") {  # 4 class prediction
#'     img[ 3:6, 3:6 ]<-rnorm(16)*scl+(i %% 4)+scl*mean(rnorm(1))
#'     imgb[ 3:6, 3:6 ]<-rnorm(16)*scl+(i %% 4)+scl*mean(rnorm(1))
#'     limg[ 3:6, 3:6 ]<-(i %% 4)+1  # the label image is constant
#'     }
#'     if ( predtype == "scalar") {
#'       img[ 3:6, 3:6 ]<-rnorm(16,1)*scl*(i)+scl*mean(rnorm(1))
#'       imgb[ 3:6, 3:6 ]<-rnorm(16,1)*scl*(i)+scl*mean(rnorm(1))
#'       limg<-i^2.0  # a real outcome
#'       }
#'     ilist[[i]]<-list(img,imgb)  # two features
#'     lablist[[i]]<-limg
#'     masklist[[i]] = mask
#'   }
#'   rfm<-vwnrfs( lablist , ilist, masklist[[1]], rad=c(2,2) ) # use single mask
#'   rfm<-vwnrfs( lablist , ilist, masklist, rad=c(2,2) )
#'   if ( predtype == "label" )
#'     print(  sum( rfm$tv != predict(rfm$rfm) ) )
#'   if ( predtype == "scalar" )
#'     print( cor(as.numeric(rfm$tv) , as.numeric(predict(rfm$rfm) ) ) )
#' } # end predtype loop
#'
#' @export vwnrfs
vwnrfs <- function( y, x, labelmasks, rad=NA, nsamples=8,
                    ntrees=500, asFactors=TRUE, reduceFactor=1) {
  
  if ( ! usePkg("randomForest") )
    stop("Please install the randomForest package, example: install.packages('randomForest')")
 
  # one labelmask or many
  useFirstMask=FALSE
  if ( typeof(labelmasks) != "list" ) {
    inmask = antsImageClone( labelmasks )
    labelmasks=list()
    for ( i in 1:length(x) ) labelmasks[[i]] = inmask
    useFirstMask = TRUE
  }
  
  # set rad=0 if not defined
  if ( all( is.na( rad )  ) ) rad<-rep(0, x[[1]][[1]]@dimension )
  
  # check y type
  yisimg<-TRUE
  if (  typeof(y[[1]]) == "integer" | typeof(y[[1]]) == "double" ) yisimg<-FALSE
  
  idim<-length(rad)
  if ( idim != x[[1]][[1]]@dimension )
    stop("vwnrfs: dimensionality does not match")
  
  invisible(gc())

  # initialize fm and tv to maximum potential size
  ulabs<-sort( unique( c( as.numeric( labelmasks[[1]] ) ) ) )
  ulabs<-ulabs[ ulabs > 0 ]
  neigh = prod(rad*2+1) # neighborhood size we'll get from getNeighborhoodInMask
  nsubj = length(x) # number of subjects
  nfeats = length(x[[1]]) # number of features
  tv<-rep( NA, nsamples*length(ulabs)*nsubj )  # Y for random forest
  fm = matrix(nrow=length(tv) ,  ncol=neigh*nfeats )  # X for random forest
  
  # fill tv and fm
  fromrow = torow =  0
  for ( i in 1:nsubj) {
    
    xfactor = x[[i]]
    if (yisimg) { yfactor = y[[i]] 
    } else { yfactor = y[i] }
    labmaskfactor = antsImageClone(labelmasks[[i]])
    
    # resample subject images with provided factor
    if (reduceFactor != 1) {
      subdim<-round( dim(labelmasks[[i]]) / reduceFactor )
      subdim[ subdim < 2*rad+1 ] <- ( 2*rad+1 )[  subdim < 2*rad+1 ]
      if (yisimg) yfactor<-resampleImage( y[[i]], subdim, useVoxels=1, interpType=as.numeric(asFactors) )
      for ( k in 1:nfeats ) xfactor[[k]]<-resampleImage( xfactor[[k]], subdim, useVoxels=1, 0 )
      if (i==1 | useFirstMask==F) 
        labmaskfactor = resampleImage(labmaskfactor, subdim, useVoxels=1,
                                     interpType=as.numeric(asFactors) ) 
    }      
      
      
    # get randmask, only once unless necessary
    if (i==1 | useFirstMask==F) {
      randmask = randomMask(labmaskfactor,nsamples=nsamples,perLabel=T)
      randvox = sum(randmask==1)
      if ( randvox == 0 ) stop("error in input data - randmask ", i," is empty")
    }
    
    # which rows shall we fill
    fromrow = torow+1
    torow = fromrow + randvox - 1
    
    # fill tv
    if ( yisimg ) { 
      tv[ fromrow:torow ] = t(getNeighborhoodInMask( yfactor, randmask, rad*0, spatial.info=F, boundary.condition='image' ))
    } else { 
      tv[ fromrow:torow ] = rep( yfactor, randvox ) 
    }
    
    fromcol = tocol = 0 # columns need reset for nfeats loop
    for ( k in 1:nfeats ) {
      # which columns shall we fill
      fromcol = tocol+1
      tocol = fromcol + neigh - 1
      
      # get neighborhood
      m1<-t(getNeighborhoodInMask( xfactor[[k]], randmask, rad, spatial.info=F, boundary.condition='image' ))
      
      # make sure neiborhood is not out of image
      if (any(is.na(m1)))
        stop(paste('Neighborhood falling out of image for subject',i,'feature',k,'\n',
                   'Consider zero-padding images to increase neighborhood availability.'))
      
      # put in fm
      fm[fromrow:torow, fromcol:tocol] = m1
      
      invisible(gc())
    }
  }
  
  invisible(gc())

  # prune tv and fm to non-NA rows
  fm = fm[!is.na(tv),]
  tv = tv[!is.na(tv)]
  if ( asFactors ) tv<-factor( tv )
  
  invisible(gc())

  rfm <- randomForest::randomForest(y=tv,x=fm, ntree = ntrees,
                                    importance = FALSE, proximity = FALSE, keep.inbag = FALSE,
                                    keep.forest = TRUE , na.action = na.omit, norm.votes=FALSE )

  invisible(gc())
  return( list(rfm=rfm, tv=tv, fm=fm, randmask=randmask ) )
}






#' voxelwise neighborhood random forest prediction
#'
#' Takes a model created with vwnrfs and builds a prediction
#' based on similar features used to train vwnrfs
#'
#' @param rfm random forest model trained with vwnrfs with certain 
#' number of features.
#' @param x a list of lists. Each list contains the list of feature
#' images required to predict a response or an image. The features
#' must be the same used during training. I.e., if you train on
#' T1 and T2 images, those should be the same features used for
#' prediction, in the same exact order for each subject.
#' @param labelmasks a list of masks where each mask defines the space 
#' to predict from. These can be individual masks for each subject 
#' (i.e., custom brain masks) or a single antsImage that will be used
#' for all subjects.
#' @param rad vector of dimensionality d define the neighborhood radius.
#' Must be the same radius with which the model was trained, i.e.,
#' c(1,1,1)
#' @param asFactors boolean - treat the y entries as factors. If this is
#' true, the prediction will be a classification, and the output will
#' produce images. If this is false, the prediction will be a regression, 
#' and the output will produce a single response value.
#' @return list a 2-list with the rf model, training vector, feature matrix
#' and the random mask
#' @author Pustina D
#'
#' @examples
#' ## Do not run
#' ## vwnrfs.predict(rfm, x=x, labelmasks=labelmasks,
#' ## rad=rad, asFactors=TRUE, voxchunk=voxchunk,
#' ## reduceFactor = mr)mask<-makeImage( c(10,10), 0 )
#' ## End do not run
#'
#' @export vwnrfs.predict
vwnrfs.predict = function(rfm, x, labelmasks, rad=NA, 
                          asFactors=TRUE, voxchunk=30000, 
                          reduceFactor = 1) {
  
  if ( ! usePkg("randomForest") )
    stop("Please install the randomForest package, example: install.packages('randomForest')")
  
  # one labelmask or many
  if ( typeof(labelmasks) != "list" ) {
    inmask = antsImageClone( labelmasks )
    labelmasks=list()
    for ( i in 1:length(x) ) labelmasks[[i]] = inmask
  } 
  
  neigh = prod(rad*2+1) # neighborhood size we'd get from getNeighborhoodInMask
  nsubj = length(x) # number of subjects
  nfeats = length(x[[1]]) # number of features
  masterprobs = list()  # this will have posterior probabilities
  if (asFactors) seg = list()  # this will have segmentations
  if (!asFactors) response = rep(NA, nsubj)  # or responses
  
  # predict each subject individually
  for(i in 1:nsubj) {

    xfactor = x[[i]]
    labmaskfactor = antsImageClone(labelmasks[[i]])
    
    # resample subject images with provided factor
    if (reduceFactor != 1) {
      subdim = round( dim(labmaskfactor) / reduceFactor )
      subdim[ subdim < 2*rad+1 ] <- ( 2*rad+1 )[  subdim < 2*rad+1 ]
      for ( k in 1:nfeats ) xfactor[[k]]<-resampleImage( xfactor[[k]], subdim, useVoxels=1, 0 )
      labmaskfactor = resampleImage(labmaskfactor, subdim, useVoxels=1, interpType=as.numeric(asFactors) ) 
    }

    # initialize output images for this subject
    if (asFactors) { nprob = length(levels(rfm$y))
    } else { nprob=1 }
    masterprobs[[i]] = list()
    for (t in 1:nprob) masterprobs[[i]][[t]] = labmaskfactor*0
    ##
    
    
    nchunks = round( sum(labmaskfactor!=0) / voxchunk )
    if ( nchunks <= 2 ) nchunks=1
    chunkmask = splitMask(labmaskfactor,nchunks)
    for (ch in 1:nchunks) {
      fm = matrix(nrow=sum(chunkmask==ch), ncol=neigh*nfeats) # initialize matrix to predict from
      fromcol = tocol = 0 # reset this for the nfeats loop
      binchunk = thresholdImage(chunkmask,ch,ch) # binary mask for this chunk only
      
      for ( k in 1:nfeats ) {
        # which columns shall we fill
        fromcol = tocol+1
        tocol = fromcol + neigh - 1
        
        # get neighborhood
        m1<-t(getNeighborhoodInMask( xfactor[[k]], binchunk, rad, spatial.info=F, boundary.condition='image' ))
        
        # make sure neiborhood is not out of image
        if (any(is.na(m1)))
          stop(paste('Neighborhood falling out of image for subject',i,'feature',k,'\n',
                     'Consider padding the images with zero values to increase neighborhood availability.'))
        
        # put in fm
        fm[, fromcol:tocol] = m1
        invisible(gc())
      }
      
      # predict this chunk
      predtype<-'response'
      if ( asFactors ) predtype<-'prob'
      probs = t( predict( rfm ,newdata=fm, type=predtype) )
      
      # fill masterprobs of this subject
      for (m in 1:nprob) masterprobs[[i]][[m]][binchunk==1] = probs[m,]
      
      rm(probs)
      invisible(gc()) # clean up some memory
    }
    
    # create segmentation for this sub
    if (asFactors) { 
      temp = imageListToMatrix( unlist(masterprobs[[i]]) , labmaskfactor )
      temp = apply( temp, FUN=which.max, MARGIN=2)
      seg[[i]] = makeImage( labmaskfactor , temp )
      rm(temp); invisible(gc())
    } else {
      response[i] = apply( imageListToMatrix( unlist(masterprobs) , labmaskfactor ), FUN=median, MARGIN=1 )
    }
  }
  
  # return either image segmentation or response
  if ( asFactors ) {
    return( list( seg=seg, probs=masterprobs ) )
  } else {
    return( list( seg=response, probs=masterprobs ) )
  }
}




#' split a mask into n labeled sub-masks
#'
#' @param mask antsImage mask
#' @param n number of mask chunks
#' @return relabeledMask
#' @author Avants BB, Tustison NJ
#'
#' @examples
#' mask = getMask( antsImageRead( getANTsRData("r16" ) ) )
#' smask = splitMask( mask, 10 )
#'
#' @export splitMask
splitMask <- function( mask, n )
{
  # first compute chunk size
  hasvalues = mask >= 0.5
  nnz = sum( hasvalues )
  voxchunk = round( nnz / n ) - 1
  chunk.seq = seq(1, nnz, by=voxchunk )
  chunk.seq[ length(chunk.seq) ] = nnz
  smask = mask * 0
  for ( ch in 1:( length(chunk.seq)-1 ) )
  {
    # set end of this chunk
    chnxt = chunk.seq[ ch + 1 ] - 1
    if ( ch ==  ( length(chunk.seq)-1 ) ) chnxt = nnz
    # create mask for this chunk
    temp = which( hasvalues, arr.ind=T )[ chunk.seq[ch]:chnxt ]
    tnnz = hasvalues
    tnnz[ -temp ] = FALSE
    smask[ tnnz ] = ch
  }
  if ( sum( mask >= 0.5 ) != sum(smask >= 0.5 ) )
  {
    stop("submask non-zero entries should be the same as input mask" )
  }
  return( smask )
}
