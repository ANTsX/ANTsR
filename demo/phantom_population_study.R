

opts_chunk$set(echo=FALSE)



options(replace.assign=TRUE,width=70)
# do not use the sourcecode directive
render_rst(strict=TRUE)

# global chunk options
knit_hooks$set(par=function(before, options, envir){if (before) par(mar=c(4,4,.1,.1),cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)})
opts_chunk$set(echo=TRUE,fig.path='figs/antsr-',  fig.width=8, fig.height=8, fig.align='center', fig.show='hold', par=TRUE, cache=FALSE )
read_chunk('../../demo/phantom_population_study.R')
# cache.path='cache/hw8-',  dev='tikz',



opts_chunk$set(echo=TRUE,verbose=FALSE)






image(as.array(antsImageRead(maskfn,'float',mydim)))






library(ggplot2)
qdata<-data.frame(qvals)
m <- ggplot(qdata, aes(x=qvals))
m + geom_histogram(aes(y = ..density..),binwidth=0.05,colour="black",fill="white") + geom_density(alpha=.2, fill="#FF6666") 



if (  isucceeded ) print("SUCCESS") 
if ( !isucceeded ) print("FAILURE") 


