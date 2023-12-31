#' Plot image from R,G,B tif file
#'
#' Mostly for visualisation.
#'
#' @param file A tiff image. Usually generated by the Argiope package.
#' @param fix.values Fix negative and >100% values for better presentation.
#' @export
plotIMG<-function(file,
                  fix.values=TRUE){

  img<-ijtiff::read_tif(file)

  if(fix.values==TRUE) {
    img[,,1,1][img[,,1,1]<0]<-0
    img[,,1,1][img[,,1,1]>100]<-100
    #img[,,1,1][is.na(img[,,1,1])==TRUE]<-0
    img[,,1,2][img[,,1,2]<0]<-0
    img[,,1,2][img[,,1,2]>100]<-100
    #img[,,1,2][is.na(img[,,1,2])==TRUE]<-0
    img[,,1,3][img[,,1,3]<0]<-0
    img[,,1,3][img[,,1,3]>100]<-100
    #img[,,1,3][is.na(img[,,1,3])==TRUE]<-0
  }

  rr<-terra::rast(sqrt((img[,,1,1]/100)*(256^2-1)))
  gg<-terra::rast(sqrt((img[,,1,2]/100)*(256^2-1)))
  bb<-terra::rast(sqrt((img[,,1,3]/100)*(256^2-1)))

  color<-cbind(terra::values(rr)/255, terra::values(gg)/255, terra::values(bb)/255)
  col<-grDevices::convertColor(color=color,
                    from="CIE RGB",
                    #to="sRGB",
                    to="Apple RGB",
                    from.ref.white="E",
                    to.ref.white="D65",
                    scale.in = 1, scale.out = 1, clip = TRUE)

  terra::values(rr)<-col[,1]*255
  terra::values(gg)<-col[,2]*255
  terra::values(bb)<-col[,3]*255

  rgb<-c(rr,gg,bb)
  terra::plotRGB(rgb,r=1, g=2, b=3, scale=255)
}


#' Find the most common RGB pixel triplet values.
#'
#' Find the most common RGB pixel triplet values using a Kernel density estimation.
#'
#' @param matrix An array with R, G and B values.
#' @export
find.multimax<-function(matrix) {
  k.M<-ks::kde(x=matrix, binned=TRUE)
  maxs<-which(k.M$estimate == max(k.M$estimate), arr.ind = TRUE)
  r<-vector(length = length(maxs))
  for (i in 1:length(maxs)) {
    r[[i]]<-k.M$eval.points[[i]][[ maxs[[i]] ]]
  }
  return(r)
}

#' Extract colour data from tif images.
#'
#' Extract and calculates several colour metrics from tif images.
#'
#' @param files A vector with tif image names.
#' @export
SDimages<-function(files) {
  results<-data.frame(

    file="none",
    path="none",

    N_pixels=0,

    median.L=0,
    mean.L=0,
    sd.L=0,
    q.L=0,

    median.logL=0,
    mean.logL=0,
    sd.logL=0,
    q.logL=0,

    median.sqrtL=0,
    mode.sqrtL=0,
    mean.sqrtL=0,
    sd.sqrtL=0,
    CV.sqrtL=0,
    q.sqrtL=0,

    median.RG=0,
    mode.RG=0,
    mean.RG=0,
    sd.RG=0,

    median.sqrtRG=0,
    mean.sqrtRG=0,
    sd.sqrtRG=0,

    median.RB=0,
    mode.RB=0,
    mean.RB=0,
    sd.RB=0,

    median.GB=0,
    mode.GB=0,
    mean.GB=0,
    sd.GB=0,

    kernel.R=0,
    kernel.G=0,
    kernel.B=0
  )

  for (i in 1:length(files)) {

    print(paste(i,"out of", length(files)))

    img<-ijtiff::read_tif(files[[i]])

    R<-as.vector(img[,,1,1])
    G<-as.vector(img[,,1,2])
    B<-as.vector(img[,,1,3])

    R<-R[is.na(R)==FALSE]
    G<-G[is.na(G)==FALSE]
    B<-B[is.na(B)==FALSE]

    #convert negative values to zero
    R[R<0]<-0
    G[G<0]<-0
    B[B<0]<-0

    sqrtR<-sqrt(R)
    sqrtG<-sqrt(G)
    sqrtB<-sqrt(B)

    L<-(R+G+B)/3
    median.L<-stats::median(L)
    mean.L<-mean(L)
    sd.L<-sd(L)
    q.L<-quantile(L,probs=c(0.75))-quantile(L,probs=c(0.25))


    logL<-log(((R+G+B)/3)+1)
    median.logL<-stats::median(logL)
    mean.logL<-mean(logL)
    sd.logL<-sd(logL)
    q.logL<-quantile(logL,probs=c(0.75))-quantile(logL,probs=c(0.25))

    sqrtL<-sqrt((R+G+B)/3)
    median.sqrtL<-stats::median(sqrtL)/sqrt(100)
    mode.sqrtL<-MCMCglmm::posterior.mode(mcmc(sqrtL))/sqrt(100)
    mean.sqrtL<-mean(sqrtL)/sqrt(100)
    sd.sqrtL<-sd(sqrtL/sqrt(100))
    CV.sqrtL<-sd(sqrtL)/mean(sqrtL)
    q.sqrtL<-quantile(sqrtL,probs=c(0.75))-quantile(sqrtL,probs=c(0.25))

    RG<-(R-G)/(R+G)
    RG[R==0&G==0]<-0

    sqrtRG<-(sqrtR-sqrtG)/(sqrtR+sqrtG)
    sqrtRG[sqrtR==0&sqrtG==0]<-0

    RB<-(R-B)/(R+B)
    RB[R==0&B==0]<-0

    GB<-(G-B)/(G+B)
    GB[G==0&B==0]<-0

    median.RG<-stats::median(RG)
    mode.RG<-MCMCglmm::posterior.mode(mcmc(RG))
    mean.RG<-mean(RG)
    sd.RG<-sd(RG)

    median.sqrtRG<-stats::median(sqrtRG)
    mean.sqrtRG<-mean(sqrtRG)
    sd.sqrtRG<-sd(sqrtRG)

    median.RB<-stats::median(RB)
    mode.RB<-MCMCglmm::posterior.mode(mcmc(RB))
    mean.RB<-mean(RB)
    sd.RB<-sd(RB)

    median.GB<-stats::median(GB)
    mode.GB<-MCMCglmm::posterior.mode(mcmc(GB))
    mean.GB<-mean(GB)
    sd.GB<-sd(GB)

    kRGB<-find.multimax(cbind(R,G,B))
    kernel.R<-kRGB[[1]]
    kernel.G<-kRGB[[2]]
    kernel.B<-kRGB[[3]]

    temp<-data.frame(

      file=basename(files[[i]]),
      path=files[[i]],

      N_pixels=length(R),

      median.L=median.L,
      mean.L=mean.L,
      sd.L=sd.L,
      q.L=q.L,

      median.logL=median.logL,
      mean.logL=mean.logL,
      sd.logL=sd.logL,
      q.logL=q.logL,

      median.sqrtL=median.sqrtL,
      mode.sqrtL=mode.sqrtL,
      mean.sqrtL=mean.sqrtL,
      sd.sqrtL=sd.sqrtL,
      CV.sqrtL=CV.sqrtL,
      q.sqrtL=q.sqrtL,

      median.RG=median.RG,
      mode.RG=mode.RG,
      mean.RG=mean.RG,
      sd.RG=sd.RG,

      median.sqrtRG=median.sqrtRG,
      mean.sqrtRG=mean.sqrtRG,
      sd.sqrtRG=sd.sqrtRG,

      median.RB=median.RB,
      mode.RB=mode.RB,
      mean.RB=mean.RB,
      sd.RB=sd.RB,

      median.GB=median.GB,
      mode.GB=mode.GB,
      mean.GB=mean.GB,
      sd.GB=sd.GB,

      kernel.R=kernel.R,
      kernel.G=kernel.G,
      kernel.B=kernel.B

    )

    results<-rbind(results,temp)

  }

  results<-results[-1,]
  return(results)

}

