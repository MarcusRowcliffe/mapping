library(shiny)
library(leaflet)
source("mapping.r")

ui <- fluidPage(
  titlePanel("Grid maker"),
  
  sidebarLayout(
    sidebarPanel(
      p("This app generates a regular grid of points with a randomised 
        starting point within a boundary. The boundary must be supplied 
        as a .kml polygon file (click the KML Help button below for instructions 
        on how to do this in Google Earth Pro). Once the boundary polygon 
        has been uploaded you can generate and inspect a grid of points by 
        clicking the Generate grid button. You can then download the point 
        long/lat locations by clicking the download button."),
      actionButton("howtodig", "KML Help"),
      p(), tags$hr(), p(),
      fileInput("file", "Choose a kml File", multiple = FALSE, accept = ".kml"),
      radioButtons("mode", NULL, list("Fixed number", "Fixed spacing"), inline=TRUE),
      fluidRow(column(4, conditionalPanel(condition = "input.mode=='Fixed number'",
                                          numericInput("npnts", "Number of points", 50, step=1)),
                      conditionalPanel(condition = "input.mode=='Fixed spacing'",
                                       numericInput("spcng", "Point spacing (km)", 1))),
               column(8, sliderInput("rotn", "Grid orientation", -45, 45, 0))
               ),
      fluidRow(
        column(6, actionButton("go", "Generate grid")),
        downloadButton("locationdata.csv", "Download locations")
      ),
      tags$hr(),
      textOutput("area"),
      textOutput("info")
    ),
    mainPanel(
      leafletOutput("map", height=700)
    )  
  )
)

server <- function(input, output, session) {
  observeEvent(input$howtodig, {
    showModal(modalDialog(
      p("1. Zoom to your region of interest"),
      p("2. Right-click on My Places in the Places pane -> Add -> Polygon"),
      p("3. Enter a name and digitise your site boundary on the map -> OK"),
      p("4. Right-click on the resulting polygon in the Places pane -> Save Place As -> 
        choose directory, enter a file name and select Save as type Kml (*.kml) -> Save"),
      title="How to create and export a polygon kml file in Google Earth Pro",
      easyClose = TRUE, footer = NULL
    ))
  })
  
  bdy <- reactive({
    req(input$file)
    res <- getXMLcoords(input$file$datapath)
    rbind(res, res[1, ])
  })

  pnt <- eventReactive(input$go, {
    if(input$mode=="Fixed number"){
      n <- input$npnts
      if(is.na(n) | n<2) res <- NULL else
        res <- makegrid(bdy(), n, rotation=input$rotn)
      updateNumericInput(session, "spcng", value=round(res$spacing/1000, 3))
      res
    } else
      if(input$mode=="Fixed spacing"){
        s <- input$spcng
        if(is.na(s) | s<=0) res <- NULL else
          res <- makegrid(bdy(), space=s*1000, rotation=input$rotn)
        updateNumericInput(session, "npnts", value=nrow(res$grid))
        res
      }
  })
  
  output$map <- renderLeaflet({
    rng <- apply(bdy(), 2, range)
    lng <- rng[,"long"]
    lat <- rng[,"lat"]
    leaflet() %>%
      setView(mean(bdy()$long), mean(bdy()$lat), 10) %>%
      fitBounds(lng[1], lat[1], lng[2], lat[2]) %>% 
      addPolygons(bdy()$long, bdy()$lat, fill=FALSE) %>%
      addTiles()
  })
  
  observe({
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(pnt()$grid$long, pnt()$grid$lat, radius=2.5, weight=2)
  })
  
  output$area <- renderText({
    paste("Covered area:", round(areaPolygon(bdy())/1e6, 3), "sq km")
  })
  
  output$info <- renderText({
    if(input$mode=="Fixed number")
      paste("Point spacing:", round(pnt()$spacing/1e3, 3), "km") else
        if(input$mode=="Fixed spacing")
          paste("Point number:", nrow(pnt()$grid))
  })
  
  output$locationdata.csv <- downloadHandler(
    filename = function() {
      "locationdata.csv"
    },
    content = function(file) {
      write.csv(pnt()$grid, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
