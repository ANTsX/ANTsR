getANTsRData <- function(fileid, usefixedlocation = FALSE) {
  library(tools)
  myusage <- "usage: getANTsRData(fileid = whatever , usefixedlocation = TRUE )"
  if (missing(fileid)) {
    print(myusage)
    return(NULL)
  }
  
  # ch2b = brodmann ch2a = aal mnib = brodmann mnia = all mnit = tracts
  
  myurl <- switch(fileid, r16 = "http://placid.nlm.nih.gov/download?items=10764", r64 = "http://placid.nlm.nih.gov/download?items=10765", 
    KK = "http://placid.nlm.nih.gov/download?items=10766", ADNI = "http://placid.nlm.nih.gov/download?folders=238", 
    K1 = "http://www.nitrc.org/frs/downloadlink.php/2201", BT = "http://placid.nlm.nih.gov/download?items=10767", 
    AB = "http://placid.nlm.nih.gov/download?items=10753", ch2 = "http://placid.nlm.nih.gov/download?items=10778", 
    ch2b = "http://placid.nlm.nih.gov/download?items=10780", ch2a = "http://placid.nlm.nih.gov/download?items=10784", 
    mni = "http://placid.nlm.nih.gov/download?items=10785", mnib = "http://placid.nlm.nih.gov/download?items=10787", 
    mnia = "http://placid.nlm.nih.gov/download?items=10786", mnit = "http://placid.nlm.nih.gov/download?items=11660")
  
  myext <- ".nii.gz"
  if (fileid == "ADNI" | fileid == "K1") 
    myext <- ".zip"
  tdir <- tempdir()  # for temporary storage
  tfn <- tempfile(pattern = "antsr", tmpdir = tdir, fileext = myext)  # for temporary storage
  if (usefixedlocation == TRUE) {
    tdir <- system.file(package = "ANTsR")  # for a fixed location
    tfn <- paste(tdir, "/html/", fileid, myext, sep = "")  # for a fixed location
  }
  if (!file.exists(tfn)) 
    download.file(myurl, tfn)
  if (fileid == "ADNI" | fileid == "K1") {
    unzip(tfn)
    return(tfn)
  }
  # could use md5sum
  mymd5 <- switch(fileid, r16 = "37aaa33029410941bf4affff0479fa18", r64 = "8a629ee7ea32013c76af5b05f880b5c6", 
    KK = "397a773658558812e91c03bbb29334bb", BT = "eb1f8ee2bba81fb80fed77fb459600f0", AB = "d38b04c445772db6e4ef3d2f34787d67", 
    ch2 = "501c45361cf92dadd007bee55f02e053", ch2b = "5db6c10eb8aeabc663d10e010860465f", ch2a = "caf2d979a7d9c86f515a5bc447856e7c", 
    mnit = "dab456335a4bfa2b3bc31e9882699ee9")
  if (!is.null(mymd5)) 
    if (md5sum(tfn) != mymd5) {
      print("checksum failure")
      return(NULL)
    }
  return(tfn)
} 
