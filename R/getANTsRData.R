getANTsRData<- function( fileid , usefixedlocation = FALSE)
{
  library(tools)
  myusage<-"usage: getANTsRData(fileid = whatever , usefixedlocation = TRUE )"
  if ( missing( fileid )  )
    {
    print(myusage)
    return(NULL)
    }
  myurl<-switch( fileid ,
                  r16="http://placid.nlm.nih.gov/download?items=10764",
                  r64="http://placid.nlm.nih.gov/download?items=10765",
                  KK="http://placid.nlm.nih.gov/download?items=10766",
                  ADNI="http://placid.nlm.nih.gov/download?folders=238",
                  K1="http://www.nitrc.org/frs/downloadlink.php/2201",
                  BT="http://placid.nlm.nih.gov/download?items=10767",
                  AB="http://placid.nlm.nih.gov/download?items=10753",
                  ch2="http://placid.nlm.nih.gov/download?items=10778",
                  ch2b="http://placid.nlm.nih.gov/download?items=10780",
                  mni="http://mindboggle.info/data/templates/MNI152_T1_1mm_brain.nii.gz",
                  mnib="http://mindboggle.info/data/templates/MNI152_T1_1mm_brain.nii.gz"
            )
  myext<-".nii.gz"
  if ( fileid == "ADNI" | fileid == "K1"  ) myext<-".zip"
  tdir<-tempdir() # for temporary storage
  tfn<-tempfile( pattern = "antsr", tmpdir = tdir, fileext = myext ) # for temporary storage
  if ( usefixedlocation == TRUE ) {
    tdir<-system.file(package='ANTsR') # for a fixed location 
    tfn<-paste(tdir,"/html/",fileid,myext,sep='') # for a fixed location
  }
  if ( !file.exists(tfn) ) download.file(myurl, tfn )
  if ( fileid == "ADNI" | fileid == "K1"  ) {
    unzip( tfn )
    return( tfn ) 
  }
  if ( fileid == "mni" |  fileid == "mnib"  ) {
    return( tfn ) 
  }
  # could use md5sum 
  mymd5<-switch( fileid ,
                  r16="37aaa33029410941bf4affff0479fa18",
                  r64="8a629ee7ea32013c76af5b05f880b5c6",
                  KK="397a773658558812e91c03bbb29334bb",
                  BT="eb1f8ee2bba81fb80fed77fb459600f0",
                  AB="d38b04c445772db6e4ef3d2f34787d67",
                  ch2="501c45361cf92dadd007bee55f02e053",
                  ch2b="5db6c10eb8aeabc663d10e010860465f"
            )
  if ( md5sum( tfn ) != mymd5 ) { print("checksum failure"); return(NULL) }
  return( tfn )
}
