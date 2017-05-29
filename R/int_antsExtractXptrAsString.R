setGeneric(name = ".int_antsExtractXptrAsString", def = function(object) standardGeneric(".int_antsExtractXptrAsString"))

#' @aliases .int_antsExtractXptrAsString,antsImage-method
setMethod(f = ".int_antsExtractXptrAsString", signature = c("antsImage"), definition = function(object) {
  return(as.character(c(object@pointer)))
})

#' @aliases .int_antsExtractXptrAsString,antsMatrix-method
setMethod(f = ".int_antsExtractXptrAsString", signature = c("antsMatrix"), definition = function(object) {
  return(as.character(c(object@pointer)))
})

#' @aliases .int_antsExtractXptrAsString,numeric-method
setMethod(f = ".int_antsExtractXptrAsString", signature = c("numeric"), definition = function(object) {
  return(object)
})

#' @aliases .int_antsExtractXptrAsString,character-method
setMethod(f = ".int_antsExtractXptrAsString", signature = c("character"), definition = function(object) {
  return(object)
})
