library(shiny)
library(shinyjs)
library(leaflet)
source("mapping.r")

ui <- fluidPage(
  useShinyjs(),
  titlePanel("GridMaker"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("info", "Click for app info"),
      tags$hr(),
      fileInput("bfile", "Choose bounday kml file(s)", multiple = TRUE, accept = ".kml"),
      checkboxInput("holes", "Add polygon holes"),
      conditionalPanel(
        condition = "input.holes==true", 
        fileInput("hfile", "Choose hole kml file(s)", multiple = TRUE, accept = ".kml")
      ),
      textOutput("area"),
      tags$hr(),
      radioButtons("mode", NULL, list("Fixed number", "Fixed spacing"), inline=TRUE),
      fluidRow(column(4, numericInput("npnts", "Number of points", 50, step=1)),
               column(4, numericInput("spcng", "Point spacing (km)", 1, step=0.1))
               ),
      sliderInput("rotn", "Grid orientation", -45, 45, 0),
      actionButton("go", "Generate grid"),
      tags$hr(),
      downloadButton("locationdata.csv", "Download locations")
    ),
    mainPanel(
      leafletOutput("map", height=700)
    )  
  )
)

server <- function(input, output, session) {
  observeEvent(input$info, {
    showModal(modalDialog(
      p("This app generates a regular grid of points with a randomised 
        starting point within one or more boundaries, optionally with holes. 
        The boundaries and holes must be supplied as .kml polygon files 
        (see below for instructions on creating these). Once the boundary and 
        any hole polygons have been uploaded, and number, spacing and/or 
        orientation of points adjusted to taste, you can generate and inspect a 
        randomised grid of points by clicking the Generate grid button. You can then 
        download the point long/lat locations by clicking the download button."),
      p(),
      p("To create kml files in Google Earth Pro:"),
      p("1. Zoom to your region of interest"),
      p("2. Right-click on My Places in the Places pane -> Add -> Polygon"),
      p("3. Enter a name and digitise your site boundary on the map -> OK"),
      p("4. Right-click on the resulting polygon in the Places pane -> Save Place As -> 
        choose directory, enter a file name and select Save as type Kml (*.kml) -> Save"),
      title="Purpose and use of the GridMaker app",
      easyClose = TRUE, footer = NULL
    ))
  })
  
  observeEvent(input$mode, {
    if(input$mode=="Fixed number"){
      enable("npnts")
      disable("spcng")
    } else if(input$mode=="Fixed spacing"){
      enable("spcng")
      disable("npnts")
    }
  })
  
#  bdy <- reactive({
#    req(input$file)
#    res <- getXMLcoords(input$file$datapath)
#    rbind(res, res[1, ])
#  })
  bdy <- reactive({
    req(input$bfile)
    lapply(input$bfile$datapath, getXMLcoords)
  })
  hol <- reactive({
    lapply(input$hfile$datapath, getXMLcoords)
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
  
#  output$map <- renderLeaflet({
#    rng <- apply(bdy(), 2, range)
#    lng <- rng[,"long"]
#    lat <- rng[,"lat"]
#    leaflet() %>%
#      setView(mean(bdy()$long), mean(bdy()$lat), 10) %>%
#      fitBounds(lng[1], lat[1], lng[2], lat[2]) %>% 
#      addPolygons(bdy()$long, bdy()$lat, fill=FALSE) %>%
#      addTiles()
#  })
  output$map <- renderLeaflet({
    mp <- leaflet() %>% addTiles()
    for(i in 1:length(bdy())) 
      mp <- mp %>% addPolygons(bdy()[[i]]$long, bdy()[[i]]$lat, fill=FALSE)
    if(length(hol())>0)
      for(i in 1:length(hol()))
        mp <- mp %>% addPolygons(hol()[[i]]$long, hol()[[i]]$lat, fill=FALSE)
    mp
  })
  
  observe({
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(pnt()$grid$long, pnt()$grid$lat, radius=2.5, weight=2)
  })
  
  output$area <- renderText({
    A <- sum(unlist(lapply(bdy(), areaPolygon)))/1e6
    if(length(hol())>0) A <- A-sum(unlist(lapply(hol(), areaPolygon)))/1e6
    paste("Covered area:", round(A, 3), "sq km")
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
