blockStimulus <- function(scans=1 ,onsets=c(1) ,durations=c(1), rt=3)
{
  stimulus<-rep( 0 , scans )
  if ( length( durations ) > 1 &  length( durations ) !=  length( onsets ) )
    {
      print("length of durations should be 1 or length of onsets")
      q()
    }
  if ( length( durations ) == 1 )
    durations<-rep( durations[1] , length(onsets) )
  for ( stim in 1:length(onsets) )
    {
    stimulus[ onsets[stim]:(onsets[stim]+durations[stim]) ] <- 1
    }
  return( stimulus )
}



