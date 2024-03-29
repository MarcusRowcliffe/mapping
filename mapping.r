require(jpeg) #for readJPEG and rasterImage
require(geosphere) #for distm and areaPolygon
require(data.table) #for rbindlist (switch to dplyr::bind_rows?)
require(sf) #for read_sf and st_coordinates
require(sp) #for point.in.polygon

#Extract the co-ordinates from a kml shapefile named in text string file 
#(including path if necessary) - getXMLcoords is deprecated
getKMLcoords <- function(file){
  dat <- sf::read_sf(file)
  res <- as.data.frame(sf::st_coordinates(dat)[, 1:2])
  names(res) <- c("long", "lat")
  res
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

#Takes a dataframe of long/lat data (dat) and returns the 
# corner co-ordinates in both long/lat and x/y,
# with x/y coords in m from the origin of ref (dat itself by default)
makemap <- function(dat, ref=dat){
  orig <- data.frame(long=min(ref$long), lat=min(ref$lat))
  rng <- data.frame(apply(dat,2,range))
  xyrng <- data.frame(t(sapply(1:2, function(i) 
    c(geosphere::distm(c(orig$long, rng$lat[i]), rng[i,]),
      geosphere::distm(c(rng$long[i], orig$lat), rng[i,]))
  )))
  names(xyrng) <- c("x","y")
  list(cnr=rng, xycnr=xyrng)
}

#Given dataframes of x,y co-ordinates, returns a logical vector indicating
# whether each of the xy points is within polygon xypoly. xypoly can be a
# list of polygons, in which case the result indicates whether xy points
# are within any of the polygon.
isinpoly <- function(xy, xypoly){
  
  f <- function(poly)
    sp::point.in.polygon(xy[,1], xy[,2], poly[,1], poly[,2])
  
  if(class(xypoly) != "list") xypoly <- list(xypoly)
  lst <- lapply(xypoly, f)
  df <- as.data.frame(lst)
  apply(df, 1, any)
}

#Project coords dataframe either way between xy and longlat given map created with loadmap
project <- function(coords, map=NULL, to=c("xy", "longlat")){
  
  #Translate coords dataframe from oldrange to newrange
  translate <- function(coords, oldrange, newrange) 
    newrange[1] + diff(newrange) * (coords-oldrange[1])/diff(oldrange)
  
  if(is.null(map)) map <- makemap(coords)
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
  if(type=="point") do.call(graphics::points, c(xyarg, plotpar)) else 
    do.call(graphics::lines, c(xyarg, plotpar))
}

#Plot grid map with optional boundary, basemap and scale
mapgrid <- function(grid, boundary=NULL, map=NULL, scale.km=NULL,
                    point.par=list(col="lightblue", pch=16), line.par=list(col="lightblue")){
  if(is.null(map)){
    if(!is.null(boundary)){
      map <- makemap(boundary)
      xygrid <- project(grid, map)/1000
      xypoly <- project(boundary)/1000
      xypoly <- list(x=xypoly$x, y=xypoly$y)
      do.call(graphics::plot, c(xypoly, type="n", asp=1, xlab="km", ylab="km"))
      do.call(graphics::lines, c(xypoly, line.par))
    } else{
      xygrid <- project(grid)/1000
      do.call(graphics::plot, c(xygrid, type="n", asp=1, xlab="km", ylab="km"))
    }
    xygrid <- list(x=xygrid$x, y=xygrid$y)
    do.call(graphics::points, c(xygrid, point.par))
  } else{
    plotmap(map)
    if(!is.null(boundary)) addshape(map, boundary, "poly", line.par)
    addshape(map, grid, "point", point.par)
    if(!is.null(scale.km))
      addscale(map, paste(scale.km, "km"), scale.km*1000, lwd=3)
  }
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

#Generate a grid of points with given spacing (in m) within bounds given by poly, 
# optionally excluding holes, given by hole. Poly and hole must be either dataframes
# with columns long and lat, or lists of such dataframes, in which case points
# are selected across all polygons excluding all holes.
# rotation is an optional grid rotation angle; default aligns grid N-S/E-W.
makegrid.s <- function(spacing, poly, hole=NULL, rotation=45){
  if(!class(poly)=="list") poly <- list(poly)
  allpoly <- data.table::rbindlist(poly)
  map <- makemap(allpoly)
  xypoly <- lapply(poly, project, map)
  cent <- apply(data.table::rbindlist(xypoly), 2, mean)
  xypoly <- lapply(xypoly, rotate, -rotation, cent)
  allxy <- data.table::rbindlist(xypoly)
  rng <- apply(allxy, 2, range)
  x <- seq(rng[1,"x"], rng[2,"x"], spacing) + runif(1)*spacing
  y <- seq(rng[1,"y"], rng[2,"y"], spacing) + runif(1)*spacing
  xy <- expand.grid(x, y)
  names(xy) <- c("x", "y")
  i <- isinpoly(xy, xypoly)
  if(!is.null(hole)){
    if(!class(hole)=="list") hole <- list(hole)
    xyhole <- lapply(hole, project, map)
    xyhole <- lapply(xyhole, rotate, -rotation, cent)
    i <- i & !isinpoly(xy, xyhole)
  }
  xy <- rotate(xy[i, ], rotation, cent)
  list(grid=project(xy, map, "longlat"), spacing=spacing)
}

#Generate a grid of n points on a map within bounds given by poly excluding any hole(s).
# As for makegrid.s, poly and hole can be lists for multiple polygons (see above).
# Rotation is an optional rotation angle; default aligns grid N-S/E-W.
makegrid.n <- function(n, poly, hole=NULL, rotation=0){
  if(!class(poly)=="list") poly <- list(poly)
  rng <- data.frame(apply(poly[[1]], 2, range))
  xyrng <- as.data.frame(rbind(0, c(distm(rng[1,], matrix(as.matrix(rng)[c(2,1,3,4)], nrow=2)))))
  spc <- sqrt(prod(apply(xyrng, 2, diff))/n)
  repeat{
    grd <- makegrid.s(spc, poly, hole, rotation)
    nr <- nrow(grd$grid)
    if(nr==n) break else spc <- spc * (nr/n)^0.5
  }
  grd
}

makegrid <- function(boundary, hole=NULL, n=NULL, space=NULL, rotation=0){
  if(is.null(n) & is.null(space)) stop("Either n or space argument must be given")
  if(!is.null(n) & !is.null(space)) stop("Either n or space argument must be given, not both")
  if(is.null(n)) 
    makegrid.s(space, boundary, hole, rotation) else
    makegrid.n(n, boundary, hole, rotation)
}

exportgrid <- function(points, file){
  write.csv(data.frame(id=1:nrow(points$grid), points$grid), file, row.names=F)
}

#Assigns points from a grid to a given number of phases with regular stepwise progression
#Requires that the grid is perfectly aligned N-S-E-W
phasegrid <- function(points, phases=2){
  latref <- as.numeric(as.factor(as.character((points$grid$lat)))) %% phases
  longref <- as.numeric(as.factor(as.character((points$grid$long)))) %% phases
  phase <- (latref+longref) %% phases + 1
  points$grid <- cbind(points$grid, phase)
  points
}
