#Notes on creating kml and raster image files in Google Earth and e.g. Paint

#In Google Earth:
#1. Zoom to study area
#2. Reset tilt and compass
#3. Save image as jpeg
#4. Add a folder, and within it:
# a. Add a polygon, digitise study site boundary, then press ok in properties dialogue
# b. Add a path between 2 diagonally opposite corners of the image after cropping
#5. Export both these objects as .kml files

#In image editing software (eg paint.net):
#2. Open or paste in the image
#3. Square-crop to the corners digitised above
#4. Save as .jpg

################
#Design survey
################
#Install required packages (needed once only on a given machine)
install.packages(c("XML","jpeg","geosphere","sp","SDMTools"))

#Load additional functions (needed every session)
# NB give path too if file not in working directory
source("mapping.r")

#Load mapping files (.jpg base plus georeferencing corners and boundary info)
baseJPG <- "C:/Users/rowcliffe.m/Documents/CameraTrapping/Hedgehogs/BushyPark17/BushyPark_cropped.jpg"
corKML1 <- "C:/Users/rowcliffe.m/Documents/CameraTrapping/Hedgehogs/BushyPark17/Corners.kml"
corKML2 <- "C:/Users/rowcliffe.m/Documents/CameraTrapping/Hedgehogs/BushyPark17/Corners2.kml"
corKML3 <- "C:/Users/rowcliffe.m/Documents/CameraTrapping/Hedgehogs/BushyPark17/Corners3.kml"
corKML4 <- "C:/Users/rowcliffe.m/Documents/CameraTrapping/Hedgehogs/BushyPark17/Corners4.kml"
bouKML <- "C:/Users/rowcliffe.m/Documents/CameraTrapping/Hedgehogs/BushyPark17/boundary.kml"

basemap1 <- loadmap(baseJPG, corKML1)
basemap2 <- loadmap(baseJPG, corKML2)
basemap3 <- loadmap(baseJPG, corKML3)
basemap4 <- loadmap(baseJPG, corKML4)

boundary <- getXMLcoords(bouKML)
areaPolygon(boundary)/1e6 #area within the boundary in km2

#Create a grid with n points within the boundary...
pnts1 <- makegrid(basemap1, boundary, 20)
pnts1$spacing #point spacing in m
#...or create grid with fixed 150 m spacing...
pnts2 <- makegrid(basemap1, boundary, space=150)
nrow(pnts2$grid) #Number of points produced
#...or download pre-existing co-ordinates
pnts3 <- read.csv("C:/Users/rowcliffe.m/Documents/CameraTrapping/Hedgehogs/BushyPark17/Bushy_camera_locations.csv")

#Make map with base, boundary, points and x km scale bar
plotmap(basemap1)
addshape(basemap1, pnts1$grid, pch=15, col="White")
addshape(basemap1, boundary, "poly", col="white")
addscale(basemap1, "1 km", 1000)
#or
mapgrid(basemap1, pnts1$grid, boundary, scale=1)

#Checking alternative corner configurations
mapgrid(basemap1, pnts3)
mapgrid(basemap2, pnts3)
mapgrid(basemap3, pnts3)
mapgrid(basemap4, pnts3)

#Export points for uploading to GPS / Googlemaps
exportgrid(pnts1, "surveypoints.csv")
