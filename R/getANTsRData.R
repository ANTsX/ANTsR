#' getANTsRData
#'
#' Downloads antsr test data
#'
#' @param fileid one of the permitted file ids
#' @param usefixedlocation directory to which you download files
#' @return filename string
#' @author Avants BB
#' @examples
#'
#' fi <- getANTsRData( "r16" )
#'
#' @export getANTsRData
getANTsRData <- function(fileid, usefixedlocation = FALSE) {
  myusage <- "usage: getANTsRData(fileid = whatever , usefixedlocation = TRUE )"
  if (missing(fileid)) {
    print(myusage)
    return(NULL)
  }

  if ( fileid == "r16" )
    return( paste(path.package("ANTsR"),"/extdata/r16slice.jpg",sep="") )
  if ( fileid == "r64" )
    return( paste(path.package("ANTsR"),"/extdata/r64slice.jpg",sep="") )

  # ch2b = brodmann ch2a = aal mnib = brodmann mnia = all mnit = tracts

  myurl <- switch(fileid, r16 = "http://placid.nlm.nih.gov/download?items=10764",
    r27 = "http://placid.nlm.nih.gov/download?items=11916", r30 = "http://placid.nlm.nih.gov/download?items=11917",
    r62 = "http://placid.nlm.nih.gov/download?items=11918", r85 = "http://placid.nlm.nih.gov/download?items=11920",
    r64 = "http://placid.nlm.nih.gov/download?items=10765", KK = "http://placid.nlm.nih.gov/download?items=10766",
    ADNI = "http://placid.nlm.nih.gov/download?folders=238", K1 = "http://www.nitrc.org/frs/downloadlink.php/2201",
    BT = "http://placid.nlm.nih.gov/download?items=10767", AB = "http://placid.nlm.nih.gov/download?items=10753",
    ch2 = "http://placid.nlm.nih.gov/download?items=10778", ch2b = "http://placid.nlm.nih.gov/download?items=10780",
    ch2a = "http://placid.nlm.nih.gov/download?items=10784", mni = "http://placid.nlm.nih.gov/download?items=10785",
    mnib = "http://placid.nlm.nih.gov/download?items=10787", mnia = "http://placid.nlm.nih.gov/download?items=10786",
    mnit = "http://placid.nlm.nih.gov/download?items=11660", nki = "http://files.figshare.com/1363201/NKI.zip",
    pcasl = "http://files.figshare.com/1862041/101_pcasl.nii.gz")

  myext <- ".nii.gz"
  if (fileid == "ADNI" | fileid == "K1" | fileid == "nki")
    myext <- ".zip"
  tdir <- tempdir()  # for temporary storage
  tfn <- tempfile(pattern = "antsr", tmpdir = tdir, fileext = myext)  # for temporary storage
  if (usefixedlocation == TRUE) {
    tdir <- system.file(package = "ANTsR")  # for a fixed location
    tfn <- paste(tdir, "/html/", fileid, myext, sep = "")  # for a fixed location
  }
  if (!file.exists(tfn))
    download.file(myurl, tfn)
  if (fileid == "ADNI" | fileid == "K1" | fileid == "nki") {
    return(tfn)
  }
  # could use md5sum
  mymd5 <- switch(fileid, r16 = "37aaa33029410941bf4affff0479fa18", r27 = "a118cf2d46fbf025fc0d20e2c6a19016",
    r30 = "870341a792caa45cffdbacfcaead9c28", r62 = "1aa85420f54c9e4040b81b6dc91d7c5a",
    r85 = "21e42b0039faa1a61f3ffd713ae477a1", r64 = "8a629ee7ea32013c76af5b05f880b5c6",
    KK = "397a773658558812e91c03bbb29334bb", BT = "eb1f8ee2bba81fb80fed77fb459600f0",
    AB = "d38b04c445772db6e4ef3d2f34787d67", ch2 = "501c45361cf92dadd007bee55f02e053",
    ch2b = "5db6c10eb8aeabc663d10e010860465f", ch2a = "caf2d979a7d9c86f515a5bc447856e7c",
    mnit = "dab456335a4bfa2b3bc31e9882699ee9", pcasl = "e59716ae76a853465efacf3cfb53bc58")
  if (!is.null(mymd5))
    if (md5sum(tfn) != mymd5) {
      print("checksum failure")
      return(NULL)
    }
  return(tfn)
}
