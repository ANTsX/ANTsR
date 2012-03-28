Sweave( system.file( "demo/pipeline.Rnw" , package = "ANTsR" ) ) ;
tools::texi2dvi( "pipeline.tex" , pdf=TRUE ) ;