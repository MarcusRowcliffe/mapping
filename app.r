ui <- fluidPage(
  titlePanel("Generate a fixed number of grid points within a boundary"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a kml File",
                multiple = FALSE,
                accept = ".kml"),
      numericInput("lwd", "Boundary thickness", 1),
      tags$hr(),
      numericInput("npnts", "Number of points", 20),
      actionButton("go", "Generate grid points"),
      tags$hr(),
      downloadButton("locationdata.csv", "Download locations")
    ),
    mainPanel(
      fluidRow(column(6, textOutput("area")),
               column(6, textOutput("space"))
      ),
      fluidRow(imageOutput("map"))
    )  
  )
)

server <- function(input, output) {
  bdy <- reactive({
    req(input$file)
    getXMLcoords(input$file$datapath)
  })
  
  pnts <- eventReactive(input$go, {
    makegrid(bdy(), input$npnts)
  })
  
  output$map <- renderImage({
    mids <- apply(bdy(), 2, mean)
    mids <- c(lat=mids["lat"], lon=mids["long"])
    rngs <- data.frame(apply(bdy(), 2, range))
    loncnr <- rngs$lon + diff(rngs$lon)*0.05*c(-1,1)
    latcnr <- rngs$lat + diff(rngs$lat)*0.05*c(-1,1)
    lat <- c(bdy()$lat, bdy()$lat[1])
    lon <- c(bdy()$long, bdy()$long[1])
    basemap <- GetMap.bbox(loncnr, latcnr, maptype="satellite", MINIMUMSIZE=TRUE)
    png('MyTile.png',type='cairo-png')
    PlotOnStaticMap(basemap, TrueProj = FALSE, 
                    lat=lat, lon=lon, FUN=lines, 
                    lwd=input$lwd, col=2)
    PlotOnStaticMap(basemap, TrueProj = FALSE, add=TRUE, 
                    lat=pnts()$grid$lat, lon=pnts()$grid$lon, FUN=points, 
                    pch=16, col=2)
    dev.off()
    list(src="MyTile.png")
  })
  
  output$area <- renderText({
    paste("Covered area:", round(areaPolygon(bdy())/1e6, 3), "sq km")
  })
  
  output$space <- renderText({
    paste("Point spacing:", round(pnts()$spacing/1e3, 2), "km")
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


