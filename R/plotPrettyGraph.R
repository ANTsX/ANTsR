plotPrettyGraph <- function( graphObject, adjacencyMatrix, functionToPlot, pngfn="graph.png", scaleText=0.5, vertexSize = NA, figScale=11 , layoutmode = "kamadakawai") {
# adapted from http://is-r.tumblr.com/
doInstall <- FALSE
toInstall <- c("sna", "igraph")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)
as.matrix(sort(functionToPlot)) 
# Now, to make the prettiest graph we can:
png( pngfn , h = 2^figScale, w = 2^figScale ) # , type = "cairo-png")
par(mai = c(0, 0, 0, 0))
functionToPlotColor <- hsv(0, 1, (functionToPlot - min(functionToPlot)) /
                       (max(functionToPlot) - min(functionToPlot)))
functionToPlotScaler <- functionToPlot/max(abs(functionToPlot)) * 0.5 + 1/2
if ( is.na( vertexSize ) ) vertexSize <- functionToPlotScaler
prettyPlot <- gplot(dat = adjacencyMatrix,
                    label = rownames(adjacencyMatrix),
                    mode = layoutmode,
                    pad = 0,
                    label.pad = 1,
                    boxed.labels = TRUE,
                    label.pos = 1,  # Below vertex
                    label.bg = "#ffffff99",
                    vertex.cex=vertexSize, # size of vertex
                    vertex.sides = 100,  # Basically circular
                    arrowhead.cex = functionToPlotScaler,
                    label.cex = functionToPlotScaler*scaleText,
                    edge.col = "#00000011",  # To make translucent bounding box
                    label.col = functionToPlotColor,
                    vertex.col = functionToPlotColor,
                    label.border = "#ffffff00",  # To hide borders
                    vertex.border = "#ffffff00")  # To hide borders
dev.off()
}
