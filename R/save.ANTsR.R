#' @title save.ANTsR
#' @description Save and load ANTsR sessions.
#' @usage save.ANTsR(filename="./.ANTsRsession", objects=NA,
#'   env=as.environment(1), ...)
#' @param filename Prefix for folder to store data.
#' @param objects Vector of character names of objects to store.  Can be antsImages.
#' @param env Environment to save from or load to.
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
                       overwrite=F, ...) {
  
  # create or empty the target folder
  if (file.exists(file.path(filename,'temp.Rdata')) & overwrite ) {
    fnames = list.files(filename)
    drop = file.remove(file.path(filename,fnames) )
  } else {
    dir.create(filename,showWarnings = F)
  }
  if (file.exists(file.path(filename,'temp.Rdata')) & ! overwrite ){
    stop(paste('Folder', filename, 'not empty and overwrite is false.'))
  }
  
  
  antslist = as.list(env)
  if(all(!is.na(objects))) {
    index = match(objects,names(antslist))
    antslist = antslist[index]
  }
  
  
  funimgS = function(x,fold=filename) {
    file = paste0(proc.time()[3],'.nii.gz')
    fn = file.path(fold,file)
    antsImageWrite(x, fn)
    return(paste0('ANTSload',file))
  }
  
  temp = rapply(antslist, funimgS, classes='antsImage', how='replace')
  save(temp,file=file.path(filename,'temp.Rdata'), ...)
  
}



#' @usage load.ANTsR(filename="./.ANTsRsession", env=as.environment(1))
#' @rdname save.ANTsR
#' @export
load.ANTsR <- function(filename=file.path('.','.ANTsRsession'),
                       env=as.environment(1)) {
  
  funimgL = function(x,fold=filename) {
    if (length(x) > 1) return(x)
    if (substr(x,1,8) == 'ANTSload') {
      fn = file.path(fold, substr(x,9,nchar(x)) )
      x = antsImageRead(fn)
    }
    return(x)
  }
  
  load(file.path(filename,'temp.Rdata'))
  antslist = rapply(temp, funimgL, classes='character', how='replace')
  envir = list2env(antslist, envir = env)
  
}
