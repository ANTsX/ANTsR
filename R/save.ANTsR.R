#' @title save.ANTsR
#' @description Save and load ANTsR sessions.
#' @usage save.ANTsR(filename="./.ANTsRsession", objects=NA,
#'   env=as.environment(1), overwrite=F, clonediskfiles=T, ...)
#' @param filename Prefix for folder to store data.
#' @param objects Vector of character names of objects to store.  Can be antsImages.
#' @param env Environment to save from or load to.
#' @param overwrite logical to select whether overwriting existing data is allowed.
#' @param clonediskfiles logical enables the copying of disk files that are not loaded
#'      in workspace but where character variables point. Set to TRUE if moving sessions 
#'      from one computer to another or if you save antsRegistration() outputs. Set to 
#'      FALSE if you save temporary sessions to continue later on the same computer.
#' @param ... Additional arguments to pass to \code{save}.
#' 
#' @author Pustina D
#' 
#' @examples
#' \dontrun{ # causes problems with devtools::run_examples()
#' # a <- 1
#' # b <- c( 2, 3, 4)
#' # save.ANTsR(objects=c('b', 'a'))
#' # load.ANTsR("./.ANTsRsession")
#' }
#' @rdname save.ANTsR
#' @export
save.ANTsR <- function(filename=file.path('.','.ANTsRsession'),
                       objects=NA,
                       env=as.environment(1),
                       overwrite=F, 
                       clonediskfiles=T,
                       ...) {
  # convert to absolute path
  filename = suppressWarnings(file.path(dirname(normalizePath(filename)),basename(filename)))
  
  # create or empty the target folder
  if (file.exists(file.path(filename,'ANTSLOAD.Rdata')) & overwrite ) {
    fnames = list.files(filename)
    assign('fremove1234567890', file.path(filename,fnames) , envir = env)
  } else {
    dir.create(filename,showWarnings = F)
  }
  if (file.exists(file.path(filename,'ANTSLOAD.Rdata')) & ! overwrite ){
    stop(paste('Folder', filename, 'not empty and overwrite is false.'))
  }
  
  
  antslist = as.list(env)
  if(all(!is.na(objects))) {
    index = match(objects,names(antslist))
    antslist = antslist[index]
  }
  
  
  funimgS = function(x,fold=filename) {
    file = paste0(paste(sample(c(0:9, letters, LETTERS), 20, replace=T),collapse=''),'.nii.gz')
    fn = file.path(fold,file)
    antsImageWrite(x, fn)
    return(paste0('ANTSload',file))
  }
  
  funimgSf = function(x,fold=filename) {
    index = which(file.exists(x))
    if (length(index) == 0) return(x)
    
    nocopy = file.exists(file.path(fold,basename(x[index])))
    noremovef = file.path(fold,basename(x[index[nocopy]]))
    noremoveindx = match(noremovef, fremove1234567890 )
    assign('fremove1234567890', fremove1234567890[-(noremoveindx)], envir = env)
    x[index[nocopy]] = paste0('ANTSrepl', basename(x[index[nocopy]]) )
    index = index[! nocopy]
    if (length(index) == 0) return(x)
    
    for (indx in index) {      
      file = paste0(paste(sample(c(0:9, letters, LETTERS), 20, replace=T),collapse=''),
                    '_', basename(x[indx]))
      fn = file.path(fold,file)
      file.copy(x[indx],fn)
      x[indx] = paste0('ANTSrepl', file)
    }
    return(x)
  }
  
  if (clonediskfiles) antslist = rapply(antslist, funimgSf, classes='character', how='replace')
  ANTSLOAD = rapply(antslist, funimgS, classes='antsImage', how='replace')
  
  if (file.exists(file.path(filename,'ANTSLOAD.Rdata')) && overwrite && length(fremove1234567890)>0) {
    drop=file.remove(fremove1234567890) # cleanup remaining files in folder
    rm(fremove1234567890,envir = env)
  }
  
  save(ANTSLOAD,file=file.path(filename,'ANTSLOAD.Rdata'), ...)
  
}




#' @usage load.ANTsR(filename="./.ANTsRsession", env=as.environment(1))
#' @rdname save.ANTsR
#' @export
load.ANTsR <- function(filename=file.path('.','.ANTsRsession'),
                       env=as.environment(1)) {
  # convert to absolute path
  filename = file.path(dirname(normalizePath(filename)),basename(filename))
  
  funimgL = function(x,fold=filename) {
    if (length(x) == 1 && substr(x,1,8) == 'ANTSload') {
      fn = file.path(fold, substr(x,9,nchar(x)) )
      x = antsImageRead(fn)
      return(x)
    } 
    x = file.path(fold,gsub('^ANTSrepl','',x))
    return(x)
  }
  
  load(file.path(filename,'ANTSLOAD.Rdata'))
  antslist = rapply(ANTSLOAD, funimgL, classes='character', how='replace')
  envir = list2env(antslist, envir = env)
  
}
