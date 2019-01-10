require(XML) #for xmlToList and xmlParse
require(jpeg) #for readJPEG and rasterImage
require(geosphere) #for distm and areaPolygon
require(SDMTools) #for pnt.in.poly


#Extract the co-ordinates from an xml or kml shapefile named in text string file 
#(including path if necessary)
getXMLcoords <- function(file){
  dat <- unlist(xmlToList(xmlParse(file)))
  coords <- dat[grep("coordinates", names(dat))]
  i <- unlist(gregexpr("\t", coords))
  j <- which(diff(i)>1)
  coords <- substr(coords, i[j]+1, i[j+1]-3)
  coords <- as.numeric(unlist(strsplit(coords, "[, ]+")))
  k <- 3*(1:(length(coords)/3))
  data.frame(long=coords[k-2], lat=coords[k-1])
}

#Load a jpeg map file, georeferenced by long/lat of SW and NW corners from cornercsv file
#Return value:
#jpeg: image raster
#cnr,xycnr: longlat and xy coordinates of corners
#xyrng: x and y ranges
#pixpm: pixels per metre
loadmap <- function(jpeg, cornerkml){
  jpg <- readJPEG(jpeg, native=T)
  res <- dim(jpg)
  cnr <- getXMLcoords(cornerkml)
  cnr <- data.frame(long=sort(cnr[,1]), lat=sort(cnr[,2]))
  xycnr <- data.frame(x=c(1,res[2]), y=c(1,res[1]))
  xyrng <- as.list(apply(xycnr,2,diff))
  xm <- c(distm(cnr[1,], as.matrix(cnr)[2:3]))
  pixpm <- xyrng$x/xm
  list(jpg=jpg, cnr=cnr, xycnr=xycnr, xyrng=xyrng, pixpm=pixpm)
}

#Plot raster map created with loadmap
plotmap <- function(map){
  plot(1,1, xlim=map$xycnr$x, ylim=map$xycnr$y, type="n", asp=1,
       xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
  rasterImage(map$jpg,1,1,map$xycnr$x[2],map$xycnr$y[2])
}

#Add a scale
#map: a raster map created by loadmap
#label: label text
#metres: single number - length of scale in metres
#x,y: x,y coordinates of left end of scale bar, as proportions of the plot sides
#offset: distance to offset label from the bar, as a proportion of the plot side
addscale <- function(map, label, metres, x=0, y=-0.1, offset=0.05, ...){
  xx <- (1+x*map$xyrng$x) + c(0, metres * map$pixpm)
  yy <- 1+y*map$xyrng$y
  lines(xx, rep(yy,2), xpd=T, ...)
  text(mean(xx), yy+offset*map$xyrng$y, label, xpd=T, ...)
}

#Translate coords dataframe from oldrange to newrange
translate <- function(coords, oldrange, newrange) 
  newrange[1] + diff(newrange) * (coords-oldrange[1])/diff(oldrange)

#Project coords dataframe either way between xy and longlat given map created with loadmap
project <- function(coords, map, to=c("xy", "longlat")){
  to <- match.arg(to)
  if(to=="xy"){
    if(!all(c("long", "lat") %in% names(coords))) stop("Column headings for coords must be long and lat")
    res <- data.frame(translate(coords$long, map$cnr$long, map$xycnr$x),
                      translate(coords$lat, map$cnr$lat, map$xycnr$y) )
    names(res) <- c("x", "y")
  } else {
    if(!all(c("x", "y") %in% names(coords))) stop("Column headings for coords must be x and y")
    res <- data.frame(translate(coords$x, map$xycnr$x, map$cnr$long),
                      translate(coords$y, map$xycnr$y, map$cnr$lat) )
    names(res) <- c("long", "lat")
  }
  res
}

#Add a point, line or polygon to a map given coords dataframe
addshape <- function(map, coords, type=c("point","polygon","line"), plotpar){
  type <- match.arg(type)
  xy <- project(coords, map)
  if(type=="polygon") xy <- rbind(xy,xy[1,])
  xyarg <- list(x=xy$x, y=xy$y)
  if(type=="point") do.call(points, c(xyarg, plotpar)) else 
    do.call(lines, c(xyarg, plotpar))
}

#Plot complete grid map with optional scale
mapgrid <- function(map, points, boundary=NULL, scale.km=NULL,
                    pnt.par=list(col="white", pch=16), bnd.par=list(col="white")){
  plotmap(map)
  if(!is.null(boundary)) addshape(map, boundary, "poly", bnd.par)
  addshape(map, points, "point", pnt.par)
  if(!is.null(scale.km))
    addscale(map, paste(scale.km, "km"), scale.km*1000, lwd=3)
}

#Rotate coords dataframe by angle around centroid (if NULL, centroid of coords is used)
rotate <- function(coords, angle, centroid=NULL){
  if(is.null(centroid)) centroid <- apply(coords, 2, mean)
  diffs <- coords - rep(centroid, each=nrow(coords))
  r <- sqrt(apply(diffs^2, 1, sum))
  th <- angle*pi/180 + atan(diffs[,1]/diffs[,2]) + 
    ifelse(diffs[,2]<0, pi, ifelse(diffs[,1]<0, 2*pi, 0))
  res <- data.frame(centroid[1] + r*sin(th), 
                    centroid[2] + r*cos(th))
  names(res) <- names(coords)
  res
}

#Generate a grid of points on a map within bounds given by poly and with given spacing
#adj shifts grid starting point by given proportions of spacingl; default is bottom left corner of bounding box
#angle is an optional rotation angle; default aligns grid N-S/E-W
makegrid.s <- function(spacing, map, poly, adj=list(x=0,y=0), angle=0){
  xyspacing <- spacing * map$pixpm
  rng <- data.frame(apply(poly,2,range))
  xyrng <- project(rng, map)
  xypoly <- rotate(project(poly, map), -angle)
  xsq <- c(xyrng$x[1]+adj$x*xyspacing, xyrng$x[2])
  ysq <- c(xyrng$y[1]+adj$y*xyspacing, xyrng$y[2])
  x <- seq(min(xsq), max(xsq), xyspacing)
  y <- seq(min(ysq), max(ysq), xyspacing)
  xy <- data.frame(x=rep(x, length(y)), y=rep(y, each=length(x)))
  inout <- pnt.in.poly(xy, xypoly)
  xy <- rotate(xy[inout$pip==1, ], angle, apply(xypoly, 2, mean))
  list(grid=project(xy, map, "longlat"), spacing=spacing)
}

#Generate a grid of n points on a map within bounds given by poly
#angle is an optional rotation angle; default aligns grid N-S/E-W
makegrid.n <- function(n, map, poly, angle=0)
{ xyrng <- project(data.frame(apply(poly,2,range)), map)
  spc <- sqrt(prod(apply(xyrng, 2, diff))/n) / map$pixpm
  repeat{
    adj <- list(x=runif(1,0,1), y=runif(1,0,1))
    grd <- makegrid.s(spc, map, poly, adj, angle)
    nr <- nrow(grd$grid)
    if(nr==n) break else spc <- spc * (nr/n)^0.5
  }
  grd
}

makegrid <- function(map, boundary, n=NULL, space=NULL, angle=0, adj=list(x=0, y=0)){
  if(is.null(n) & is.null(space)) stop("Either n or space argument must be given")
  if(!is.null(n) & !is.null(space)) stop("Either n or space argument must be given, not both")
  if(is.null(n)) makegrid.s(space, map, boundary, adj, angle) else
    makegrid.n(n, map, boundary, angle)
}

exportgrid <- function(points, file)
  write.csv(data.frame(id=1:nrow(points$grid), points$grid), file, row.names=F)
