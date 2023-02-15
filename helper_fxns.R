# message('Loading helper functions...')
# equalize_image <- function(img) {
#   ### helper function to equalize one channel
#   hist_eq <- function(i) as.cimg(ecdf(i)(i), dim = dim(i))
#   
#   ### Split image across colour channels
#   cn <- imsplit(img, "c")  ### we now have a list of images
#   
#   cn_eq <- map_il(cn, hist_eq) #run hist_eq on each
#   
#   img_eq <- imappend(cn_eq, "c")
#   return(img_eq)
# }