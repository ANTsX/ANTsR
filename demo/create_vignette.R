pipeline_file = readLines( system.file( "demo/pipeline.Rnw" , package = "ANTsR" ) ) ;
for( l in 1:length(pipeline_file) )
{
	pipeline_file[l] = sub( "usepackage{Sweave}" , paste( "usepackage{" , system.file( package = "ANTsR" ) , "/demo/Sweave}" , sep = "" ) , pipeline_file[l] , fixed=TRUE ) ;
}
writeLines( pipeline_file , "pipeline.Rnw" ) ;
Sweave( "pipeline.Rnw" , stylepath=FALSE ) ;
tools::texi2dvi( "pipeline.tex" , pdf=TRUE ) ;