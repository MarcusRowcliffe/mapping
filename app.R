library(shiny)
library(RgoogleMaps)
source("C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/GitHub/mapping/mapping.r")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("kmlfile", "Choose CSV File", accept = ".kml"),
      numericInput("npnts", "Number of points", 20),
      numericInput("psz", "Point size", 1),
      radioButtons("pch", "Point type", list("Filled", "Open")),
      numericInput("lwd", "Boundary thickness", 1),
      downloadButton("locationdata.csv", "Download locations")
    ),
    mainPanel(
      fluidRow(
        column(6, textOutput("space")),
        column(6, textOutput("area"))
      ),
      fluidRow(imageOutput("map"))
    )
  )
)

server <- function(input, output) {
  bdy <- reactive({
    inFile <- input$kmlfile
    if (is.null(inFile))
      return(NULL)
    getXMLcoords(inFile$datapath)
  })
  pnts <- reactive({
    makegrid(bdy(), input$npnts)
  })
  output$map <- renderImage({
    mids <- apply(bdy(), 2, mean)
    mids <- c(lat=mids["lat"], lon=mids["long"])
    rngs <- data.frame(apply(bdy(), 2, range))
    zoom <- MaxZoom(rngs$lat, rngs$long)
    loncnr <- rngs$lon + diff(rngs$lon)*0.05*c(-1,1)
    latcnr <- rngs$lat + diff(rngs$lat)*0.05*c(-1,1)
    basemap <- GetMap.bbox(loncnr, latcnr, maptype="satellite", MINIMUMSIZE=TRUE)
    png('MyTile.png',type='cairo-png')
    lat <- c(bdy()$lat,bdy()$lat[1])
    lon <- c(bdy()$long,bdy()$long[1])
    tmp <- PlotOnStaticMap(basemap, TrueProj = FALSE, 
                           lat=lat, lon=lon, FUN=lines, 
                           lwd=input$lwd, col=2)
    PlotOnStaticMap(basemap, TrueProj = FALSE, add=TRUE,
                    lat=pnts()$grid$lat, lon=pnts()$grid$lon, FUN=points, 
                    cex=input$psz, pch=switch(input$pch, Filled=16, Open=1), col=2)
    dev.off()
    list(src="MyTile.png")
  })

  output$space <- renderText({
    paste("Spacing:", round(pnts()$spacing/1e3, 2), "km")
  })
  
  output$area <- renderText({
    paste("Area:", round(areaPolygon(bdy())/1e6, 3), "sq km")
  })
  
  output$locationdata.csv <- downloadHandler(
    filename = function() {
      "locationdata.csv"
    },
    content = function(file) {
      write.csv(pnts()$grid, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)


