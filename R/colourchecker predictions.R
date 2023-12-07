#Function to extract grey values from the colourchecker using AI
#' Extract grey values using AI
#'
#' Extract grey values from the colourchecker using an AI model. This function is used internally.
#'
#' @param image.model Array of image to apply the AI model, same size as model training. Usually a 512x512px or 256x256px image.
#' @param image.raw The raw version of the 'image.model'. Usually a larger and with different proportions. Those are adjusted internally to proper data extraction.
#' @param model The Keras Unet model used for predictions.
#' @param save.colorchecker.image Whether the extracted colorchecker image should be saved.
#' @param colorchecker_y_lim Relative position of the bottom and top vertex of each grey scale value to be extracted. Used when method = "AI".
#' @param colorchecker_x_lim Relative position of the left and rigth vertex of each grey scale value to be extracted. Used when method = "AI".
#' @param colorchecker_y_size The size of the colorchecker rectangle minor side.
#' @param colorchecker_x_size The size of the colorchecker rectangle largest side.
#' @param save.name Name of file to be saved.
#' @return A list containing: 1. Median of R,G, and B scale values; 2. MAD of R,G, and B pixel scale values; and 3. Image scale in pixel/cm2 and pixel/cm. Those are calculated based on the colorchecker size.
#' @export
colorchecker.scale.data.extraction<-function(
    image.model, #image to be predicted, same size as model training,
    image.raw, #the raw image
    model, #the unet model used for predictions
    save.colorchecker.image = TRUE,
    save.name,
    colorchecker_y_lim,
    colorchecker_x_lim,
    colorchecker_y_size,
    colorchecker_x_size

) {

  #The JPEG function (readJPEG) rotates the
  #image when in landscape. Code below fix this
  #problem
  #print("AI colorchecker prediction running...")
  image.raw.dim<-dim(image.raw[,,,])
  dim.image.model<-dim(image.model)
  jpeg.rotatation <- FALSE
  if (image.raw.dim[[1]]>image.raw.dim[[2]]) {
    print("-Image model rotation to avoid AI prediction misplacement")
    for (i in 1:dim.image.model[[3]]) {
      image.model[,,i]<-apply(t(image.model[,,i]),2,rev)
    }
    jpeg.rotatation <- TRUE
  }


  #convert the image to keras default
  image.model.array<-array(dim=c(1,
                                 dim.image.model[[1]],
                                 dim.image.model[[2]],
                                 dim.image.model[[3]]))
  image.model.array[1,,,]<-image.model
  #terra::plot(terra::rast(image.model.array[1,,,1]))

  #make the predictions
  #the prediction is either 1 or 0 for
  #each pixel of the original image
  #1 == colorchecker
  #print(" making prediction based on model")
  predictions <- keras::predict_on_batch(object=model,
                                  x=image.model.array[1,,, , drop=FALSE])
  #str(predictions)
  #terra::plot(terra::rast(predictions[1,,,1]))
  predictions_binary <- round(predictions) #here any >0.5 -> 1
  predictions_binary[predictions_binary==0]<-NA

  #Dimensoes do colorchecker
  #12.23mm x 87.38mm
  area_colorchecker<-colorchecker_y_size * colorchecker_x_size
  pixels<-sum(predictions_binary, na.rm = TRUE)
  scale_pixels_cm2 <- pixels/area_colorchecker

  y_x_ratio<-colorchecker_y_size/colorchecker_x_size
  #x*x*ratio = N pixels
  #x = sqrt(N pixels/ratio)
  #pixels/cm = x/8.738
  scale_pixels_cm <- (sqrt (pixels/y_x_ratio)) / colorchecker_x_size

  #convert to raster so that it can be used
  #to estimate a polygon
  prediction.raster<-terra::rast(predictions_binary[1,,,])
  #terra::plot(prediction.raster)

  #extent the image to have the same dimension as the raw image
  terra::ext(prediction.raster)<-c(0,max(image.raw.dim),0,max(image.raw.dim))

  #convert to polygon
  polyg<-terra::as.polygons(prediction.raster,
                   aggregate=TRUE)

  #transform into the polygon into a rectangle
  rectangle<-terra::minRect(polyg)

  #find rectangle coordinates
  #so that the angle of rotation can be found
  coords<-terra::crds(rectangle)
  coords<-coords[1:4,]

  #A.find angle of rotation
  #A.1.first find the longest side of the rectangle:
  distance<-matrix(nrow=4,ncol=5)
  colnames(distance)<-c("x1","y1","x2","y2","deltaS")
  distance[1:4,1:2]<-coords
  distance[1:4,3:4]<-coords[c(2,3,4,1),]
  distance[,"deltaS"]<-sqrt((distance[,"x1"]-distance[,"x2"])^2+(distance[,"y1"]-distance[,"y2"])^2)
  distance[,"deltaS"]<-round(distance[,"deltaS"],3)
  longest.side<-which(distance[,"deltaS"]==max(distance[,"deltaS"]))[[1]]

  #A.2. Find the slope angle
  #in a system of two equations (y=a+bx):
  #b = (y2-y1)/(x2-x1)
  b <- (distance[longest.side,"y2"]-distance[longest.side,"y1"])/(distance[longest.side,"x2"]-distance[longest.side,"x1"]) #coeficiente angular

  #A.2. if x2==x1 its a vertical line and the angle is 90 degrees
  if (b == Inf) {
  #A.2. if not, the angle can be found by
   slope <-90 } else {
     slope <- atan(b)*180/pi #(180/pi to convert from rad to degrees)
    }


  #Based on the angle above,
  #spin the rectangle to align with x-axis
  rectangle2<-terra::spin(x=rectangle, angle=slope,
                   x0=distance[longest.side,"x1"],
                   y0=distance[longest.side,"y1"])
  coords2<-terra::crds(rectangle2)
  coords2<-coords2[1:4,]

  #add padding to match the training image ratio (a square)
  #print(" padding image")
  image.raw.padding<-array(dim=c(max(image.raw.dim), max(image.raw.dim), image.raw.dim[[3]]))

  if(image.raw.dim[[1]]<image.raw.dim[[2]]) {
    x1 <- round((image.raw.dim[[2]]-image.raw.dim[[1]])/2) + 1
    x2 <- x1 + image.raw.dim[[1]] - 1
    image.raw.padding[x1:x2,,]<-image.raw[,,,1]
  }
  if(image.raw.dim[[1]]>image.raw.dim[[2]]) {
    y1 <- round((image.raw.dim[[1]]-image.raw.dim[[2]])/2) + 1
    y2 <- y1 + image.raw.dim[[2]] - 1
    image.raw.padding[,y1:y2,]<-image.raw[,,,1]
  }
  #gc()
  #print(" converting image to raster")
  image.raw.rast<-terra::rast(image.raw.padding[,,])
  rm(image.raw.padding)
  rm(image.raw)
  #gc()

  #crop the original image with the rectangle
  #aqui vou ter que fazer o corte da foto raw
  #print(" cropping image")
  cropped<-terra::crop(image.raw.rast, rectangle,
                     snap="out", mask=TRUE,
                     touches=FALSE, extend=FALSE)
  #terra::plotRGB(cropped, scale=255^2)

  #number of pixels raw image
  pixels.raw<-length(as.vector(terra::values(cropped[[1]]))[is.na(as.vector(terra::values(cropped[[1]]))==FALSE)])
  scale_pixels_cm2.raw <- pixels.raw/area_colorchecker
  scale_pixels_cm.raw <- (sqrt (pixels.raw/y_x_ratio)) / colorchecker_x_size

  #convert to points and spin to align with x-axis
  cropped.points<-list()
  for (i in 1:terra::nlyr(cropped)) {
    #it is a list because the image has 3 layers (R,G,B)
    cropped.points[[i]]<-terra::as.points(cropped[[i]],
                               values=TRUE)
  }

  #find the rectangle centroid to
  #be used in the rotation
  centroid<-terra::crds(terra::centroids(rectangle))

  #make the alignment for each layer (R,G, B)
  cropped.aligned<-list()
  for (i in 1:length(cropped.points)) {
    cropped.aligned[[i]]<-terra::spin(x=cropped.points[[i]], angle=slope,
                             x0=centroid[1,"x"],
                             y0=centroid[1,"y"]
                             )
  }

  df.points<-data.frame(x=terra::crds(cropped.aligned[[1]])[,1],
           y=terra::crds(cropped.aligned[[1]])[,2])

        #normalize (from 0 to 1)
  df.points$x<-df.points$x-min(df.points$x)
  df.points$y<-df.points$y-min(df.points$y)
  df.points$x<-df.points$x/max(df.points$x)
  df.points$y<-df.points$y/max(df.points$y)

  #add RGB values
  for (i in 1:length(cropped.aligned)) {
    df.points[,i+2]<-terra::values(cropped.aligned[[i]])
  }


  colorchecker<-list(
   colorchecker1=subset(df.points,
                         y>=colorchecker_y_lim[[1]]&
                         y<=colorchecker_y_lim[[2]]&
                         x>=colorchecker_x_lim[[1]]&
                         x<=colorchecker_x_lim[[2]]),
    colorchecker2=subset(df.points,
                       y>=colorchecker_y_lim[[1]]&
                         y<=colorchecker_y_lim[[2]]&
                         x>=colorchecker_x_lim[[3]]&
                         x<=colorchecker_x_lim[[4]]),
    colorchecker3=subset(df.points,
                       y>=colorchecker_y_lim[[1]]&
                         y<=colorchecker_y_lim[[2]]&
                         x>=colorchecker_x_lim[[5]]&
                         x<=colorchecker_x_lim[[6]]),
    colorchecker4=subset(df.points,
                       y>=colorchecker_y_lim[[1]]&
                         y<=colorchecker_y_lim[[2]]&
                         x>=colorchecker_x_lim[[7]]&
                         x<=colorchecker_x_lim[[8]]),
    colorchecker5=subset(df.points,
                       y>=colorchecker_y_lim[[1]]&
                         y<=colorchecker_y_lim[[2]]&
                         x>=colorchecker_x_lim[[9]]&
                         x<=colorchecker_x_lim[[10]]),
    colorchecker6=subset(df.points,
                       y>=colorchecker_y_lim[[1]]&
                         y<=colorchecker_y_lim[[2]]&
                         x>=colorchecker_x_lim[[11]]&
                         x<=colorchecker_x_lim[[12]])
)

  #Median
  colorchecker.median<-lapply(colorchecker, FUN=function(x){
    colorchecker.median<-NULL
    for (i in 3:ncol(x)) {
      colorchecker.median<-c(colorchecker.median,median(x[,i]))
    }
    return(colorchecker.median)
  })

  #Median Absolute Deviation
  #See ?mad
  colorchecker.mad<-lapply(colorchecker, FUN=function(x){
    colorchecker.mad<-NULL
    for (i in 3:ncol(x)) {
      colorchecker.mad<-c(colorchecker.mad,mad(x[,i]))
    }
    return(colorchecker.mad)
  })

  #Save colorchecker image
  #with location of points extracted
  #Used to check quality
  #This is done for a sample of point only,
  #to save time and memory.
  if (save.colorchecker.image == TRUE) {
  sample.points<-sample(1:nrow(df.points),
                        size=round(0.15*nrow(df.points)))

  values <- as.matrix(df.points[,3:ncol(df.points)])
  values <- rbind(values, matrix(0,ncol=3,
                                 nrow=length(colorchecker)*length(sample.points)))
  x1<-nrow(df.points)+1
  x2<-nrow(values)
  values[x1:x2,1] <- 255^2

  x <- df.points[,1]
  y <- df.points[,2]
  for (i in 1:length(colorchecker)) {
    x <- c(x,colorchecker[[i]]$x[sample.points]) #* 360 - 180
    y <- c(y,colorchecker[[i]]$y[sample.points]) #* 180 - 90
  }

  x<-x/y_x_ratio
  if (jpeg.rotatation == TRUE) {
    r <- terra::rast(ncols=round(ncol(cropped)),
                    nrows=round(nrow(cropped)*y_x_ratio),
                    extent=c(0,1/y_x_ratio,0,1))
  } else {
    r <- terra::rast(ncols=round(ncol(cropped)*y_x_ratio),
                     nrows=round(nrow(cropped)),
                     extent=c(0,1/y_x_ratio,0,1))
  }


  xy <- cbind(x, y)
  r0 <- terra::rasterize(xy, r,
                  values=sqrt(values)) #sqrt because raw is in 16bits

  #create new dir
  have.dir<-dir.exists("colorchecker")
  if (have.dir==FALSE) {dir.create("colorchecker")}

  #save image
  grDevices::jpeg(file.path("colorchecker", paste("colorchecker_", save.name, ".jpg", sep="")), width = 1000, height = round(1000*y_x_ratio))
  terra::plotRGB(r0)
  dev.off()

  }
  #print("Finished colorchecker processing.")
  results<-list(colorchecker.median=colorchecker.median,
                colorchecker.mad=colorchecker.mad,
                scale_pixels_cm2 = scale_pixels_cm2,
                scale_pixels_cm = scale_pixels_cm,
                Npixels = pixels,
                scale_pixels_cm2.raw = scale_pixels_cm2.raw,
                scale_pixels_cm.raw = scale_pixels_cm.raw,
                Npixels.raw = pixels.raw)
  return(results)

}


#' Get photo data using AI
#'
#' Get data from a converted photo using an AI model.
#'
#' @param method Get data from a converted tif file ('file') or process RAW photo on the fly ('pred')
#' @param scale Object from the procphotos functions. Used when method='pred'.
#' @param scale.size Object from the procphotos functions used to set the scale of the image (i.e. pixel/cm2). It must be provided.
#' @param path Path to access raw photos.
#' @param path.image.model Path to access images to apply the AI model, same size as model training. Usually a 512x512px or 256x256px image.
#' @param model The Keras Unet model used for predictions.
#' @param item.name A vector of item types to be selected by user.
#' @param files Vector of file names to analyze. If 'files="All"', all files are processed.
#' @return A data frame in each lines represent data of one extracted patch.
#'  One image of each patch is also saved. Results and images are saved in a 'results' folder.
#' @export
getPhotoDataAI <- function (
                            method=c("file","pred"),
                            scale=NULL,
                            scale.size=scale,
                            path,
                            path.image.model,
                            model,
                            item.name = "butterfly",
                            files="All"
                           )
{

  old.path<-getwd()
  setwd(path)

  have.dir <- dir.exists(paste("results",item.name,sep="-"))
  if (have.dir == FALSE) {
    dir.create(paste("results",item.name,sep="-"))
  }
  have.dir <- dir.exists(file.path(paste("results",item.name,sep="-"), "png"))
  if (have.dir == FALSE) {
    dir.create(file.path(paste("results",item.name,sep="-"), "png"))
  }
  have.dir <- dir.exists(file.path(paste("results",item.name,sep="-"), "raw"))
  if (have.dir == FALSE) {
    dir.create(file.path(paste("results",item.name,sep="-"), "raw"))
  }

  if (method[[1]] == "pred" & files[[1]] == "All") {
    files <- names(scale$scale)
  }
  if (method[[1]] == "file" & files[[1]] == "All") {
    files <- list.files(path)
  }

  data.frame.final<-data.frame(image=NULL,
                               item.image=NULL,
                               item.location=NULL,
                               item.type=NULL,
                               item=NULL,
                               specfile=NULL,
                               scale_pixels_cm2.model.image=NULL,
                               scale_pixels_cm.model.image=NULL,
                               item.N.pixels.model.image=NULL,
                               item.N.pixels.raw.image=NULL,
                               scale_pixels_cm2.raw.image=NULL,
                               scale_pixels_cm.raw.image=NULL,
                               item.area_cm2=NULL,
                               mean.L=NULL,
                               median.L=NULL,
                               sd.L=NULL,
                               mad.L=NULL,
                               medianR=NULL,
                               medianG=NULL,
                               medianB=NULL,
                               meanR=NULL,
                               meanG=NULL,
                               meanB=NULL,
                               kernelR=NULL,
                               kernelG=NULL,
                               kernelB=NULL,
                               sdR=NULL,
                               sdG=NULL,
                               sdB=NULL,
                               madR=NULL,
                               madG=NULL,
                               madB=NULL,
                               X=NULL,
                               Y=NULL)

  for (k in 1:length(files)) {

    if (method == "pred") {

      scale_pixels_cm2<-scale.size$scale_pixels_cm2[[ files[[k]] ]]
      scale_pixels_cm<-scale.size$scale_pixels_cm[[ files[[k]] ]]
      Npixels<-scale.size$Npixels[[ files[[k]] ]]

      scale_pixels_cm2.raw<-scale.size$scale_pixels_cm2.raw[[ files[[k]] ]]
      scale_pixels_cm.raw<-scale.size$scale_pixels_cm.raw[[ files[[k]] ]]
      Npixels.raw<-scale.size$Npixels.raw[[ files[[k]] ]]


      modelR<-stats::lm(scale$scale[[ files[[k]] ]]$modelR$model[,1]~medianR, data=scale$scale[[ files[[k]] ]]$modelR$model)
      modelG<-stats::lm(scale$scale[[ files[[k]] ]]$modelG$model[,1]~medianG, data=scale$scale[[ files[[k]] ]]$modelG$model)
      modelB<-stats::lm(scale$scale[[ files[[k]] ]]$modelB$model[,1]~medianB, data=scale$scale[[ files[[k]] ]]$modelB$model)

      tags<-dcraw.tags(files[[k]])
      img.size<-as.numeric(strsplit(x=tags$`Output size`, split=" x ")[[1]])
      options(fftempdir = tempdir())
      img.temp<-ff::ff(as.vector(read.dcraw(file=files[[k]])),
                       dim=c(img.size[[2]],img.size[[1]],3,1))


      img<-ff::ff(0, dim = c(attr(attr(img.temp, "virtual"), "Dim")[[1]],
                             attr(attr(img.temp, "virtual"), "Dim")[[2]],
                             1,3))

      img[,,1,1]<-predict(object=modelR, newdata=data.frame(medianR=as.vector(img.temp[,,1,1])))
      img[,,1,2]<-predict(object=modelG, newdata=data.frame(medianG=as.vector(img.temp[,,2,1])))
      img[,,1,3]<-predict(object=modelB, newdata=data.frame(medianB=as.vector(img.temp[,,3,1])))
      rm(img.temp)
    }

    if (method == "file") {

      #Se nao tiver o arquivo scale, pode dar pau aqui
      scale_pixels_cm2<-scale.size$scale_pixels_cm2[[ files[[k]] ]]
      scale_pixels_cm<-scale.size$scale_pixels_cm[[ files[[k]] ]]

      img<-ijtiff::read_tif(paste(files[[k]]))

    }

    print(paste("Processing image", files[[k]]))

    #Read the JPEG image used in the AI model
    setwd(path.image.model)
    ext.model <- strsplit(files[[k]], split="\\.")[[1]][[2]]
    file.name.model<-sub(pattern=ext.model, replacement="JPG", x=files[[k]],
                         ignore.case = TRUE)
    image.model<-jpeg::readJPEG(file.name.model)
    setwd(path)

    image.raw.dim<-dim(img[,,,])
    dim.image.model<-dim(image.model)
    jpeg.rotatation <- FALSE
    if (image.raw.dim[[1]]>image.raw.dim[[2]]) {
      print(" Image model rotation to avoid AI prediction misplacement")
      for (i in 1:dim.image.model[[3]]) {
        image.model[,,i]<-apply(t(image.model[,,i]),2,rev)
      }
      jpeg.rotatation <- TRUE
    }

    #convert the image to keras default
    image.model.array<-array(dim=c(1,
                                   dim.image.model[[1]],
                                   dim.image.model[[2]],
                                   dim.image.model[[3]]))
    image.model.array[1,,,]<-image.model

    #make the predictions
    #the prediction is either 1 or 0 for
    #each pixel of the original image
    #1 == colorchecker
    #print(" making prediction based on model")
    predictions <- keras::predict_on_batch(object=model,
                                    x=image.model.array[1,,, , drop=FALSE])
    predictions_binary <- round(predictions) #here any >0.5 -> 1
    predictions_binary[predictions_binary==0]<-NA

    #Number of pixels
    pixels<-sum(predictions_binary, na.rm = TRUE)

    #convert to raster so that it can be used
    #to estimate a polygon
    prediction.raster<-terra::rast(predictions_binary[1,,,])

    #extent the image to have the same dimension as the raw image
    terra::ext(prediction.raster)<-c(0,max(image.raw.dim),0,max(image.raw.dim))

    #convert to polygon
    polyg<-terra::as.polygons(prediction.raster,
                              aggregate=TRUE)
    polyg.coords<-terra::crds(polyg)

    #add padding to match the training image ratio (a square)
    #print(" padding image")
    image.raw.padding<-array(dim=c(max(image.raw.dim), max(image.raw.dim), image.raw.dim[[3]]))

    if(image.raw.dim[[1]]<image.raw.dim[[2]]) {
      x1 <- round((image.raw.dim[[2]]-image.raw.dim[[1]])/2) + 1
      x2 <- x1 + image.raw.dim[[1]] - 1
      image.raw.padding[x1:x2,,]<-img[,,,1]
    }

    if(image.raw.dim[[1]]>image.raw.dim[[2]]) {
      y1 <- round((image.raw.dim[[1]]-image.raw.dim[[2]])/2) + 1
      y2 <- y1 + image.raw.dim[[2]] - 1
      image.raw.padding[,y1:y2,]<-img[,,,1]
    }
    #gc()
    #print(" converting image to raster")
    image.raw.rast<-terra::rast(image.raw.padding[,,])
    rm(image.raw.padding)
    rm(img)
    #gc()

    #crop the original image with the rectangle
    #aqui vou ter que fazer o corte da foto raw
    #print(" cropping image")
    cropped<-terra::crop(image.raw.rast, polyg,
                         snap="near", mask=TRUE,
                         touches=FALSE, extend=FALSE)
    #plotRGB(cropped, scale=110)

    new.image<-as.array(cropped)

    new.image.name<-paste(paste(files[[k]],item.name,sep="_"),sep="")
    new.image.name<-paste(new.image.name,".tiff", sep="")
    ijtiff::write_tif(img=new.image,
                      path=file.path(paste("results",item.name,sep="-"), "raw", new.image.name),
                      bits_per_sample = "auto",
                      compression = "none",
                      overwrite = TRUE, msg = TRUE)

    #SAVE PNG IMAGE
    new.image.PNG<-new.image
    new.image.PNG[,,1]<-sqrt((new.image.PNG[,,1]/100)*(256^2-1))
    new.image.PNG[,,2]<-sqrt((new.image.PNG[,,2]/100)*(256^2-1))
    new.image.PNG[,,3]<-sqrt((new.image.PNG[,,3]/100)*(256^2-1))

    col<-grDevices::convertColor(color=cbind(new.image.PNG[,,1][1:length(new.image.PNG[,,1])]/255,
                                             new.image.PNG[,,2][1:length(new.image.PNG[,,2])]/255,
                                             new.image.PNG[,,3][1:length(new.image.PNG[,,3])]/255),
                                 from="CIE RGB",
                                 to="sRGB",
                                 from.ref.white="E",
                                 to.ref.white="D65",
                                 scale.in = 1, scale.out = 1, clip = TRUE)

    new.image.PNG[,,1][1:length(new.image.PNG[,,1])]<-col[,1]#*255
    new.image.PNG[,,2][1:length(new.image.PNG[,,2])]<-col[,2]#*255
    new.image.PNG[,,3][1:length(new.image.PNG[,,3])]<-col[,3]#*255

    png::writePNG(image=new.image.PNG,
                  target=file.path(paste("results",item.name,sep="-"), "png", paste(new.image.name,".png", sep="")))

        R<-as.vector(new.image[,,1])
        G<-as.vector(new.image[,,2])
        B<-as.vector(new.image[,,3])
        R<-R[is.na(R)==FALSE]
        G<-G[is.na(G)==FALSE]
        B<-B[is.na(B)==FALSE]

        #find.multimax error
        #kRGB <- find.multimax(matrix=cbind(R, G, B))
        kRGB <- c(NA, NA, NA)

        L<-(R+G+B)/3

        item.N.pixels.raw.image<-length(as.vector(new.image[,,1])[is.na(as.vector(new.image[,,1]))==FALSE])
        item.area_cm2<-item.N.pixels.raw.image/scale_pixels_cm2.raw
        scale_pixels_cm2.raw.image<-scale_pixels_cm2.raw
        scale_pixels_cm.raw.image<-scale_pixels_cm.raw

        data.frame.temp<-data.frame(image=files[[k]],
                                    item.image=paste(files[[k]],item.name,".tif",sep="_"),
                                    item.location=NA,
                                    item.type=NA,
                                    item=item.name,
                                    specfile=NA,
                                    scale_pixels_cm2.model.image=scale_pixels_cm2,
                                    scale_pixels_cm.model.image=scale_pixels_cm,
                                    item.N.pixels.model.image=pixels,
                                    item.N.pixels.raw.image=item.N.pixels.raw.image,
                                    scale_pixels_cm2.raw.image=scale_pixels_cm2.raw.image,
                                    scale_pixels_cm.raw.image=scale_pixels_cm.raw.image,
                                    item.area_cm2=item.area_cm2,
                                    mean.L=mean(L),
                                    median.L=median(L),
                                    sd.L=sd(L),
                                    mad.L=mad(L),
                                    medianR=stats::median(as.vector(new.image[,,1]),na.rm = TRUE),
                                    medianG=stats::median(as.vector(new.image[,,2]),na.rm = TRUE),
                                    medianB=stats::median(as.vector(new.image[,,3]),na.rm = TRUE),
                                    meanR=mean(as.vector(new.image[,,1]),na.rm = TRUE),
                                    meanG=mean(as.vector(new.image[,,2]),na.rm = TRUE),
                                    meanB=mean(as.vector(new.image[,,3]),na.rm = TRUE),
                                    kernelR=kRGB[[1]],
                                    kernelG=kRGB[[2]],
                                    kernelB=kRGB[[3]],
                                    sdR=stats::sd(as.vector(new.image[,,1]),na.rm = TRUE),
                                    sdG=stats::sd(as.vector(new.image[,,2]),na.rm = TRUE),
                                    sdB=stats::sd(as.vector(new.image[,,3]),na.rm = TRUE),
                                    madR=stats::mad(as.vector(new.image[,,1]),na.rm = TRUE),
                                    madG=stats::mad(as.vector(new.image[,,2]),na.rm = TRUE),
                                    madB=stats::mad(as.vector(new.image[,,3]),na.rm = TRUE),
                                    X=paste0(polyg.coords[,1], collapse=","),
                                    Y=paste0(polyg.coords[,2], collapse=",")
        )
        data.frame.final<-rbind(data.frame.final,data.frame.temp)

        if (k>1) {unlink(file.path(paste("results",item.name,sep="-"), results.temp.name))}
        results.temp.name<-paste(files[[1]],"-",files[[k]],"resultsTEMP.csv",sep="")
        utils::write.csv(x=data.frame.final, file=file.path(paste("results",item.name,sep="-"),results.temp.name), row.names = F)

      }


  results.name<-paste(files[[1]],"-",files[[k]],"collected_data.csv",sep="")

  is.there.a.file <- results.name%in%list.files(paste("results",item.name,sep="-"))

  if (is.there.a.file==TRUE) {
    ask<-tcltk::tk_messageBox(type = c("yesnocancel"),
                       message="There is already a results file. Append results?", caption = "", default = "no")
    if(ask=="yes") {
      old_result<-utils::read.csv(file= file.path(paste("results",item.name,sep="-"), results.name))
      data.frame.final<-rbind(old_result,data.frame.final)
      utils::write.csv(x=data.frame.final, file= file.path(paste("results",item.name,sep="-"), results.name), row.names = F)
      print(paste("Data saved to", results.name))

    }

    if(ask=="no") {
      utils::write.csv(x=data.frame.final, file=file.path(paste("results",item.name,sep="-"), results.name), row.names = F)
      print(paste("Data saved to", results.name))

    }

    if(ask=="cancel") {
      return(data.frame.final)
    }

    if(is.null(ask)==TRUE) {
      return(data.frame.final)
    }

  }

  if (is.there.a.file==FALSE) {
    utils::write.csv(x=data.frame.final, file=file.path(paste("results",item.name,sep="-"), results.name), row.names = F)
    print(paste("Data saved to", results.name))
  }

  unlink(file.path(paste("results",item.name,sep="-"), results.temp.name))
  setwd(old.path)
  return(data.frame.final)
}
