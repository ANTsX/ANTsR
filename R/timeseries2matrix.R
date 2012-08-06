timeseries2matrix <- function( img , mask )
{
logmask <- ( mask == 1 )
mat <- img[ logmask ]
dim( mat ) <- c( sum( logmask == 1 ) , dim( img )[ length( dim(img) ) ] )

return( t( mat ) )
}
