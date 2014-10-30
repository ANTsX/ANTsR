

fn <- "../../demo/example_images/phantomtemplate.jpg"
img <- antsImageRead(fn, 2)
plotANTsImage(myantsimage = img)



simg <- antsImageClone(img)
SmoothImage(2, img, 3, simg)
plotANTsImage(myantsimage = img, functional = simg, threshold = "150x260", color = "red", 
  axis = 1)

 
