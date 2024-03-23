#' Simple plotPrettyGraph function saves to png.
#'
#' PlotPrettyGraph given inputs from the makeGraph function.  adapted from
#' http://is-r.tumblr.com/.
#'
#' @param adjacencyMatrix igraph adjacencyMatrix
#' @param functionToPlot igraph node-level graph value e.g. degree,
#' page.rank, etc
#' @param pngfn filename for output png or to screen if NA
#' @param scaleText relative size of text to vertices
#' @param vertexSize cex size of vertices
#' @param figScale the figure will be of square size 2^figScale in pixels
#' @param layoutmode see gplot.layout in sna package
#' @param hueval controls the hue in hsv
#' @return no output
#' @author Avants BB, Christopher DeSante and David Sparks
#' @examples
#'
#' data("bold_correlation_matrix", package = "ANTsR")
#' dmat <- data.matrix(bold_correlation_matrix)
#' if (usePkg("igraph")) {
#'   gg <- makeGraph(dmat, 0.1)
#'   rownames(gg$adjacencyMatrix) <- colnames(bold_correlation_matrix)
#'   plotPrettyGraph(gg$adjacencyMatrix, gg$degree, figScale = 12, scaleText = 5)
#' }
#'
#' @export plotPrettyGraph
plotPrettyGraph <- function(adjacencyMatrix, functionToPlot,
                            pngfn = tempfile(fileext = ".png"), scaleText = 0.5, vertexSize = NA, figScale = 11,
                            layoutmode = "eigen", hueval = 0) {
  # adapted from http://is-r.tumblr.com/
  if (!usePkg("igraph")) {
    print("Need igraph package")
    return(NULL)
  }
  if (!usePkg("sna")) {
    print("Need sna package")
    return(NULL)
  }
  as.matrix(sort(functionToPlot))
  # Now, to make the prettiest graph we can:
  if (!is.na(pngfn)) {
    png(pngfn, height = 2^figScale, width = 2^figScale)
  }
  par(mai = c(0, 0, 0, 0))
  frange <- (functionToPlot - min(functionToPlot)) /
    (max(functionToPlot) - min(functionToPlot))
  alpharange <- (frange * 2)
  alpharange[alpharange > 1] <- 1
  functionToPlotColor <- hsv(hueval, 1, frange, alpha = alpharange)
  functionToPlotScaler <- abs(functionToPlot) / max(abs(functionToPlot)) * 0.5 + 1 / 2
  if (is.na(vertexSize)) vertexSize <- functionToPlotScaler
  # gplot is from sna
  prettyPlot <- sna::gplot(
    dat = adjacencyMatrix,
    label = rownames(adjacencyMatrix),
    mode = layoutmode,
    pad = 0,
    label.pad = 1,
    boxed.labels = TRUE,
    label.pos = 1, # Below vertex
    label.bg = "#ffffff99",
    vertex.cex = vertexSize, # size of vertex
    vertex.sides = 100, # Basically circular
    arrowhead.cex = functionToPlotScaler,
    label.cex = functionToPlotScaler * scaleText,
    edge.col = "#00000011", # To make translucent bounding box
    label.col = functionToPlotColor,
    vertex.col = functionToPlotColor,
    label.border = "#ffffff00", # To hide borders
    vertex.border = "#ffffff00"
  ) # To hide borders
  if (!is.na(pngfn)) dev.off()
  detach("package:sna", unload = TRUE)
  if (!is.na(pngfn)) {
    return(pngfn)
  } else {
    return(invisible(NULL))
  }
}
