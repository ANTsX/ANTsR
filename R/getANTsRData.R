#' getANTsRData
#'
#' Downloads antsr test data
#'
#' @param fileid one of the permitted file ids or pass "show" to list all
#'   valid possibilities. Note that most require internet access to download.
#' @param usefixedlocation directory to which you download files
#' @param verbose optional boolean
#' @param method Method to be used for downloading files,
#' passed to \code{\link{download.file}}
#' @param quiet If \code{TRUE}, suppress status messages
#' (if any), and the progress bar.
#' @return filename string
#' @author Avants BB
#' @examples
#'
#' fi <- getANTsRData("r16")
#'
#' @export getANTsRData
getANTsRData <- function(fileid,
                         usefixedlocation = FALSE,
                         verbose = FALSE,
                         method = ifelse(
                           Sys.info()["sysname"] == "Linux",
                           "wget", "auto"
                         ),
                         quiet = FALSE) {
  myusage <- "usage: getANTsRData(fileid = whatever , usefixedlocation = TRUE )"
  if (missing(fileid)) {
    print(myusage)
    return(NULL)
  }
  validlist <- c(
    "r16", "r27", "r30", "r62", "r64", "r85", "r64", "nki", "pcasl",
    "ch2", "ch2a", "ch2b", "finn", "finncsv",
    "mni", "mnia", "mnib", "mninat", "mnijhu1", "mnijhu2", "mnidfn", "mniyeo",
    "atroposseg", "simple", "fmrinetworks",
    "rsbold", "rsboldmask", "rsboldseg", "rsboldpts", "decslice", "dtislice",
    "adfmri", "population", "surf", "blob", "rand", "show",
    "multi_component_image"
  )
  if (sum(validlist == fileid) == 0) {
    stop("no data with that id - try show to get list of valid ids")
  }
  if (fileid == "show") {
    return(validlist)
  }
  if (fileid == "population") {
    ilist <- list(
      antsImageRead(getANTsRData("r16")),
      antsImageRead(getANTsRData("r27")),
      antsImageRead(getANTsRData("r64"))
    )
    return(ilist)
  }

  maketmpimg <- function() {
    img <- makeImage(c(256, 256),
      voxval = rnorm(256^2, 100, 3),
      pixeltype = "unsigned char"
    )
    tfn <- tempfile(fileext = ".jpg")
    antsImageWrite(img, tfn)
    return(tfn)
  }

  if (fileid == "simple") {
    x <- system.file("extdata", "simple.jpg", package = "ANTsRCore")
    if (nchar(x) == 0) {
      x <- paste(path.package("ANTsRCore"),
        "/extdata/simple.jpg",
        sep = ""
      )
    }
    if (nchar(x) == 0) x <- maketmpimg()
    return(x)
  }
  if (fileid == "r16") {
    x <- system.file("extdata", "r16slice.jpg", package = "ANTsRCore")
    if (nchar(x) == 0) {
      x <- paste(path.package("ANTsRCore"),
        "/extdata/r16slice.jpg",
        sep = ""
      )
    }
    if (nchar(x) == 0) x <- maketmpimg()
    return(x)
  }
  if (fileid == "r27") {
    x <- system.file("extdata", "r27slice.jpg", package = "ANTsRCore")
    if (nchar(x) == 0) {
      x <- paste(path.package("ANTsRCore"),
        "/extdata/r27slice.jpg",
        sep = ""
      )
    }
    if (nchar(x) == 0) x <- maketmpimg()
    return(x)
  }
  if (fileid == "r30") {
    x <- system.file("extdata", "r30slice.jpg", package = "ANTsRCore")
    if (nchar(x) == 0) {
      x <- paste(path.package("ANTsRCore"),
        "/extdata/r30slice.jpg",
        sep = ""
      )
    }
    if (nchar(x) == 0) x <- maketmpimg()
    return(x)
  }
  if (fileid == "r62") {
    x <- system.file("extdata", "r62slice.jpg", package = "ANTsRCore")
    if (nchar(x) == 0) {
      x <- paste(path.package("ANTsRCore"),
        "/extdata/r62slice.jpg",
        sep = ""
      )
    }
    if (nchar(x) == 0) x <- maketmpimg()
    return(x)
  }
  if (fileid == "r64") {
    x <- system.file("extdata", "r64slice.jpg", package = "ANTsRCore")
    if (nchar(x) == 0) {
      x <- paste(path.package("ANTsRCore"),
        "/extdata/r64slice.jpg",
        sep = ""
      )
    }
    if (nchar(x) == 0) x <- maketmpimg()
    return(x)
  }
  if (fileid == "r85") {
    x <- system.file("extdata", "r85slice.jpg", package = "ANTsRCore")
    if (nchar(x) == 0) {
      x <- paste(path.package("ANTsRCore"),
        "/extdata/r85slice.jpg",
        sep = ""
      )
    }
    if (nchar(x) == 0) x <- maketmpimg()
    return(x)
  }
  if (fileid == "rand") {
    x <- system.file("extdata", "rand.jpg", package = "ANTsRCore")
    if (nchar(x) == 0) x <- maketmpimg()
    return(x)
  }

  if (fileid == "multi_component_image") {
    fname <- system.file("extdata", "multi_component_image.nii.gz",
      package = "ANTsRCore"
    )
    return(fname)
  }

  # ch2b = brodmann ch2a = aal mnib = brodmann mnia = all mnit = tracts

  myurl <- switch(fileid,
    KK = "http://placid.nlm.nih.gov/download?items=10766",
    ADNI = "http://placid.nlm.nih.gov/download?folders=238",
    K1 = "http://www.nitrc.org/frs/downloadlink.php/2201",
    BT = "http://placid.nlm.nih.gov/download?items=10767",
    AB = "http://placid.nlm.nih.gov/download?items=10753",
    ch2 = "https://ndownloader.figshare.com/files/3888853",
    ch2b = "https://ndownloader.figshare.com/files/3676050",
    ch2a = "https://ndownloader.figshare.com/files/3674664",
    finn = "https://ndownloader.figshare.com/files/4249681",
    finncsv = "https://ndownloader.figshare.com/files/4454071",
    mni = "https://ndownloader.figshare.com/files/3674667",
    mnib = "https://ndownloader.figshare.com/files/3676050",
    mnia = "https://ndownloader.figshare.com/files/3674664",
    mninat = "https://ndownloader.figshare.com/files/3676062",
    mnijhu1 = "https://ndownloader.figshare.com/files/3676056",
    mnijhu2 = "https://ndownloader.figshare.com/files/3676059",
    mnidfn = "https://ndownloader.figshare.com/files/3676047",
    mniyeo = "https://ndownloader.figshare.com/files/4863193",
    # nki = "http://files.figshare.com/1363201/NKI.zip",
    nki = "https://ndownloader.figshare.com/files/3133826",
    pcasl = "http://files.figshare.com/1862041/101_pcasl.nii.gz",
    pcaslseg = "http://files.figshare.com/1862040/101_seg.nii.gz",
    atroposseg = "http://files.figshare.com/1893339/atroposseg.nii",
    rsbold = "https://ndownloader.figshare.com/files/2119176",
    rsboldmask = "https://ndownloader.figshare.com/files/2119177",
    rsboldseg = "https://ndownloader.figshare.com/files/2119178",
    rsboldpts = "http://files.figshare.com/2126379/bold_rois.csv",
    decslice = "https://ndownloader.figshare.com/files/2156224",
    dtislice = "https://ndownloader.figshare.com/files/2157786",
    fmrinetworks = "https://ndownloader.figshare.com/files/3639108",
    adfmri = "https://ndownloader.figshare.com/files/4901371",
    surf = paste0(
      "https://github.com/stnava/antsSurf/blob/",
      "master/ADNI_137_S_0158_MR_MPR__GradWarp_",
      "_N3__Scaled_Br_20070306171702344_S20209_",
      "I42985BrainSegmentation.nii.gz?raw=true"
    ),
    blob = paste0(
      "https://github.com/stnava/antsSurf/blob/",
      "master/blob.nii.gz?raw=true"
    )
  )

  myext <- ".nii.gz"
  if (fileid == "ADNI" | fileid == "K1" | fileid == "nki" |
    fileid == "fmrinetworks") {
    myext <- ".zip"
  }
  tdir <- tempdir() # for temporary storage
  tfn <- tempfile(pattern = "antsr", tmpdir = tdir, fileext = myext) # for temporary storage
  if (usefixedlocation == TRUE) {
    tdir <- system.file(package = "ANTsR") # for a fixed location
    tfn <- paste(tdir, "/html/", fileid, myext, sep = "") # for a fixed location
  }
  if (!file.exists(tfn)) {
    download.file(myurl, tfn, method = method)
  }
  if (fileid == "fmrinetworks") {
    inms <- c(
      "IFG_middle_temporal",
      "anterior_cingulate_precun", "auditory",
      "default", "left_executive",
      "left_right_exec_combined_network",
      "motor", "parietal_association_cortex",
      "posterior_default", "right_executive",
      "salience", "supplementary_motor", "visual"
    )
    selinds <- c(4, 1, 2, 3, 6, 7, 8, 11, 12, 13)
    unzip(tfn, exdir = tdir)
    flist <- Sys.glob(paste(tdir, "/rsfmrinetwork_*nii.gz", sep = ""))
    ilist <- imageFileNames2ImageList(flist)
    if (verbose) {
      lochelp <- "Map the mni template to your image then transform these
      network files to the individual space.\n"
      cat(lochelp)
    }
    return(list(
      networkNames = inms[selinds],
      images = ilist[selinds]
    ))
  }
  if (fileid == "surf" | fileid == "blob" | fileid == "ADNI" | fileid == "K1" |
    fileid == "nki") {
    return(tfn)
  }
  # could use md5sum
  mymd5 <- switch(fileid,
    KK = "397a773658558812e91c03bbb29334bb",
    BT = "eb1f8ee2bba81fb80fed77fb459600f0",
    AB = "d38b04c445772db6e4ef3d2f34787d67",
    ch2 = "501c45361cf92dadd007bee55f02e053",
    ch2b = "5db6c10eb8aeabc663d10e010860465f",
    mnit = "dab456335a4bfa2b3bc31e9882699ee9",
    pcasl = "e59716ae76a853465efacf3cfb53bc58",
    pcaslseg = "3872d709475ee2d51d90aa7f4df5af8f",
    atroposseg = "f40680bd1de0592c9fa9e380bd9e29be",
    rsbold = "dfba022d1f715c00e042a79c6250eb16",
    rsboldmask = "15c9c6b38ce8e7f69fd4b64c110a17b4",
    rsboldseg = "d50ceabc2bffbf12cee8b16e2216bfbc",
    rsboldpts = "9eefc8d78fae07bfc0b5ef2da45b5d39",
    decslice = "08f41703ac7398319c6dba66701c0653",
    dtislice = "fd9bf3f50bad39833ec78b8dd3677df0"
  )

  if (!is.null(mymd5)) {
    if (tools::md5sum(tfn) != mymd5) {
      stop("checksum failure")
    }
  }
  return(tfn)
}
