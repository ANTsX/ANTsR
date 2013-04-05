eigSeg <- function( mask = NA, imgList = NA )
  {
  if ( typeof(mask) != "S4" | typeof(imgList) != "list" )
    {
    print( args( eigSeg ) )
    return( 1 )
    }
  maskvox<-( mask > 0 )
  maskseg<-antsImageClone( mask )
  maskseg[ maskvox ]<-0
  if ( length( imgList ) > 0 )
    {
    mydata <- imageListToMatrix( imgList, mask )
    segids<-apply( abs( mydata ) , 2 , which.max)
    segmax<-apply( abs( mydata ) , 2 , max)
    maskseg[ maskvox ]<-( segids * ( segmax > 0 ) )
    print( max( segmax ) )
    print( max( segids ) )
    return( maskseg )
    }
  else print("No images in list")
  }
