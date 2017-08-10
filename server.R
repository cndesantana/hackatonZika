library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) # make the jsonlite suggested dependency explicit
library(maptools)
library(maps)
library(htmltools)


municBR   <- readShapePoly("./geodata/municipios_2010.shp")
municBA <- municBR[(municBR@data$estado_id)==5,] #Bahia
cidades <- read.table("./modeldata/nodes.csv", header = TRUE, sep = ';', dec=",")
edges <- read.table("./modeldata/edges.csv", header = TRUE, sep = ';', dec=",")
edges <- edges %>% select(Origem, Destino, MunDestino, PESO, tau, Destino_LAT, Destino_LONG)
#edges <- as.data.frame(subset(edges, select = c('Origem', 'Destino', 'Distância', 'MunDestino','PESO', 'Destino_LAT', 'Destino_LONG')))
colnames(edges)[colnames(edges) == 'Destino_LAT'] <- 'tlat'
colnames(edges)[colnames(edges) == 'Destino_LONG'] <- 'tlon'
maxw = max(edges$PESO, na.rm = TRUE)
maxd = max(edges$tau, na.rm = TRUE)

m0 <- map(municBA, plot=FALSE);
mymap <- leaflet(m0)%>%
   addTiles('http://korona.geog.uni-heidelberg.de/tiles/roads/x={x}&y={y}&z={z}') %>%
   addPolygons(data=m0, fillColor = NULL, stroke = FALSE);

find_target_city <- function(source, weight, days){
  # message("EDGES: ",is.atomic(edges))
  # message("SOURCE: ",typeof(source))
  targets <- as.data.frame(edges[edges$Origem == source & edges$PESO >= weight & edges$tau <= days, ])
}

function(input, output, session) {

  output$citySelect <- renderUI({
    selectizeInput('cities', 'Cidades', cidades$name, selected = NULL, multiple = TRUE,
               options = NULL)
  })

  output$wEdge <- renderUI({
    sliderInput("weight", "Repetições ao longo do tempo:",
        min = 1, max = maxw, value = maxw
    )
  })
  
  output$dEdge <- renderUI({
     sliderInput("days", "Movimento na Janela Temporal:",
                 min = 0, max = maxd, value = 0
     )
  })

  # output$targetsTable <- renderUI({
  #     cities <- citiesSelected()
  #     for(city in cities$code) {
  #       targets <- find_target_city(city, weight)
  #     }
  # })

  weightSelected <- reactive({
    if (is.null(input$weight))
      return()
    as.numeric(input$weight)
  })
  
  daysSelected <- reactive({
     if (is.null(input$days))
        return()
     as.numeric(input$days)
  })

  citiesSelected <- reactive({
    if (is.null(input$cities))
      return()
    cities <- cidades[which(cidades$name %in% input$cities),]
  })

  # Store last zoom button value so we can detect when it's clicked
  lastZoomButtonValue <- NULL

  output$bahiamap <- renderLeaflet({
    cities <- citiesSelected()
    weight <- weightSelected()
    days <-   daysSelected()
    if (length(cities) != 0){
      mymap <- mymap %>% addMarkers(as.numeric(cities$lon),
                                    as.numeric(cities$lat),
                                    label = ~htmlEscape(cities$name))
    }
    for(city in cities$code) {
      targets <- find_target_city(city, weight, days)
      if (nrow(targets) > 0){
          mymap <- mymap %>% addCircleMarkers(as.numeric(as.character(targets$tlon)), 
                              as.numeric(as.character(targets$tlat)), 
                              color = colorRampPalette(brewer.pal(9,"Reds"))(maxw+1)[(targets$PESO)%/%1], 
                              label = ~htmlEscape(targets$MunDestino))
          }
      # output$targetsTable <- renderTable(targets)
    }

    rezoom <- "first"
    # If zoom button was clicked this time, and store the value, and rezoom
    if (!identical(lastZoomButtonValue, input$zoomButton)) {
      lastZoomButtonValue <<- input$zoomButton
      rezoom <- "always"
    }

    mymap <- mymap %>% mapOptions(zoomToLimits = rezoom)

    mymap
  })
}
