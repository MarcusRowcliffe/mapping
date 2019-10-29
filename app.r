library(shiny)
library(RgoogleMaps)
source("mapping.r")

ui <- fluidPage(
  titlePanel("Generate a fixed number of grid points within a boundary"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a kml File",
                multiple = FALSE,
                accept = ".kml"),
      numericInput("lwd", "Boundary thickness", 1),
      sliderInput("mapsz", "Map size", 0.5, 2, 1, 0.1),
      sliderInput("mapzm", "Map zoom", 0, 5, 0.05, 0.05),
      tags$hr(),
      numericInput("npnts", "Number of points", 50),
      actionButton("go", "Generate grid"),
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
    n <- round(input$npnts)
    if(is.na(n) | n<2) n <- 2
    makegrid(bdy(), n)
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
                    FUN=lines, lwd=input$lwd, col=2)
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
