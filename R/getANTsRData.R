getANTsRData<- function( fileid )
{
  library(tools)
  myusage<-"usage: getANTsRData(fileid = whatever )"
  if ( missing( fileid )  )
    {
    print(myusage)
    return(NULL)
    }
  myurl<-switch( fileid ,
                  r16="http://placid.nlm.nih.gov/download?items=10764",
                  r64="http://placid.nlm.nih.gov/download?items=10765",
                  KK="http://placid.nlm.nih.gov/download?items=10766",
                  ADNI="http://placid.nlm.nih.gov/download?folders=238"
            )
  myext<-".nii.gz"
  if ( fileid == "ADNI" ) myext<-".zip"
  tfn<-tempfile( pattern = "antsr", tmpdir = tempdir(), fileext = myext ) 
  download.file(myurl, tfn )
  if ( fileid == "ADNI" ) return( tfn )
  # could use md5sum 
  mymd5<-switch( fileid ,
                  r16="37aaa33029410941bf4affff0479fa18",
                  r64="8a629ee7ea32013c76af5b05f880b5c6",
                  KK="397a773658558812e91c03bbb29334bb"
            )
  if ( md5sum( tfn ) != mymd5 ) { print("checksum failure"); return(NULL) }
  return( tfn )
}
