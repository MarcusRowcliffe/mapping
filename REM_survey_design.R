#Notes on creating kml and raster image files in Google Earth and e.g. Paint

#In Google Earth:
#1. Zoom to study area
#2. Reset tilt and compass
#3. Save image as jpeg
#4. Add a folder, and within it:
# a. Add a polygon, digitise study site boundary, then press ok in properties dialogue
# b. Add a path between 2 diagonally opposite corners of the image after cropping
#5. Export both these objects as .kml files

#In image editing software (eg paint):
#2. Open or paste in the image
#3. Square-crop to the corners digitised above
#4. Save as .jpg

################
#Design survey
################
#Install required packages (needed once only on a given machine)
install.packages(c("XML","jpeg","geosphere","sp","SDMTools"))

#Set working directory to that containing mapping.r source file, 
# base map .jpg and boundary and map corner .kml files
setwd("C:/Users/Rowcliffe.M/Documents/GitHub/mapping")

#Load additional functions (needed every session)
# NB give path too if file not in working directory
source("mapping.r")

#Load mapping files (.jpg base plus georeferencing corners and boundary info)
basemap <- loadmap("basemap.jpg", "Corners.kml")
boundary <- getXMLcoords("Boundary.kml")

#Create a regular square grid with 60 points within the boundary
#  (first argument is number of points)
pnts <- makegrid(basemap, boundary, 60)
#Check out metrics:
areaPolygon(boundary)/1e6 #area within the boundary in km2
pnts$spacing              #point spacing in m
#Make map with base, boundary, points and 0.5 km scale bar
mapgrid(basemap, boundary, pnts, 0.5)
#Export points for uploading to GPS / Googlemaps
exportgrid(pnts, "surveypoints.csv")

#ALTERNATIVELY...
#Grid with fixed 200 m spacing (number of points unpredictable)
pnts2 <- makegrid(basemap, boundary, space=200)
mapgrid(basemap, boundary, pnts2)
nrow(pnts2$grid) #Number of points produced
