library(shiny)
library(RgoogleMaps)
source("mapping.r")

ui <- fluidPage(
  titlePanel("Grid maker"),
  fluidRow(
    column(12,
           p("This app generates a regular grid of points with randomised starting point 
             within a boundary. The boundary must be supplied as a .kml polygon file, 
             which can be created in Google Earth Pro:"),
           p("1. Zoom to your region of interest"),
           p("2. Right-click on My Places in the Places pane -> Add -> Polygon"),
           p("3. Enter a name and digitise your site boundary on the map -> OK"),
           p("4. Right-click on the resulting polygon in the Places pane -> Save Place As -> 
             choose directory, enter a file name and select Save as type Kml (*.kml) -> Save"),
           p("Once the boundary polygon has been uploaded you can generate and inspect a 
             grid of points by clicking the Generate grid button. You can then download 
             the point locations (long/lat) by clicking the download button.")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a kml File", multiple = FALSE, accept = ".kml"),
      radioButtons("mode", NULL, list("Fixed number", "Fixed spacing"), inline=TRUE),
      conditionalPanel(condition = "input.mode=='Fixed number'",
                       numericInput("npnts", "Number of points", 50)),
      conditionalPanel(condition = "input.mode=='Fixed spacing'",
                       numericInput("spcng", "Point spacing (km)", 1)),
      sliderInput("rotn", "Grid orientation", -45, 45, 0),
      actionButton("go", "Generate grid"),
      tags$hr(),
      sliderInput("mapsz", "Map size", 0.5, 2, 1, 0.1),
      sliderInput("mapzm", "Map zoom", 0, 5, 0.05, 0.05),
      tags$hr(),
      downloadButton("locationdata.csv", "Download locations")
    ),
    mainPanel(
      fluidRow(column(5, textOutput("area")),
               column(5, textOutput("space"))
      ),
      fluidRow(imageOutput("map"))
    )  
  )
)

server <- function(input, output) {
  bdy <- reactive({
    req(input$file)
    res <- getXMLcoords(input$file$datapath)
    rbind(res, res[1, ])
  })

  pnts <- eventReactive(input$go, {
    if(input$mode=="Fixed number"){
      n <- round(input$npnts)
      if(is.na(n) | n<2) return(NULL) else
      return(makegrid(bdy(), n, rotation=input$rotn))
    } else
    if(input$mode=="Fixed spacing"){
      s <- input$spcng
      if(is.na(s) | s<=0) return(NULL) else
      return(makegrid(bdy(), space=s*1000, rotation=input$rotn))
    }
  })
  
  output$map <- renderImage({
    rngs <- data.frame(apply(bdy(), 2, range))
    cnrs <- data.frame(lon=rngs$lon + diff(rngs$lon)*input$mapzm*c(-1,1),
                       lat=rngs$lat + diff(rngs$lat)*input$mapzm*c(-1,1))
    basemap <- GetMap.bbox(cnrs$lon, cnrs$lat, MINIMUMSIZE=TRUE)
    sz <- input$mapsz * basemap$size
    png("MyTile.png", width=sz[1], height=sz[2], type="cairo-png")
    PlotOnStaticMap(basemap, TrueProj = FALSE,
                    lat=bdy()$lat, lon=bdy()$long,
                    FUN=lines, lwd=2, col=2)
    if(!is.null(pnts()))
       PlotOnStaticMap(basemap, TrueProj = FALSE, add=TRUE,
                       lat=pnts()$grid$lat, lon=pnts()$grid$lon, 
                       pch=16, col=2)
    dev.off()
    list(src="MyTile.png")
  })
  
  output$area <- renderText({
    paste("Covered area:", round(areaPolygon(bdy())/1e6, 3), "sq km")
  })
  
  output$space <- renderText({
    if(input$mode=="Fixed number")
      paste("Point spacing:", round(pnts()$spacing/1e3, 2), "km") else
      paste("Point number:", nrow(pnts()$grid))
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
