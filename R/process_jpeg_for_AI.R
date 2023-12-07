#' Padding and resize images for AI
#'
#' Interactively get data from a converted photo and associate it with item type, item name and reflectance file.
#'
#' @param geometry Image conversion size.
#' @param pattern Image extension.
#' @param path.images Path to access photos.
#' @import magick
#' @export
resize_for_AI<-function(
    geometry="512x512",
    pattern=".jpg",
    path.images) {

  old<-getwd()
  setwd(path.images)

  have.dir<-dir.exists("resized")
  if (have.dir==FALSE) {dir.create("resized")}

  files<-list.files(path.images, pattern = pattern, ignore.case = TRUE)
  for (i in 1:length(files)) {
  # print(paste(i, "out of", length(files)))
    img<-magick::image_read(files[[i]])
    img.dim<-dim(img[[1]])[c(2,3)]

    if(img.dim[[1]]>img.dim[[2]]) {
      geometry1<-paste("0x", img.dim[[1]], sep="")
    }

    if(img.dim[[1]]<img.dim[[2]]) {
      geometry1<-paste(img.dim[[2]], "x0", sep="")
    }

    img<-magick::image_extent(img, geometry=geometry1, gravity = "center", color = "black")
    img<-magick::image_resize(img, geometry = geometry, filter = NULL)
    path<-file.path(path.images, "resized", files[[i]])
    magick::image_write(
      image=img,
      path = path,
      format = "jpeg",
    )
  }
  setwd(old)
}
