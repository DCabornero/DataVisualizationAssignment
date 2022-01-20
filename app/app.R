library(shiny)
library(tmap)
library(dplyr)
library(tidyverse)
library(rgdal)
library(ggstream)
library(leaflet)

ui <- fluidPage(
  titlePanel("Accidents by districts"),
  # Districts
  fluidRow(
    # First parameter is the length (maximum: 12)
    column(4, offset=1,
      helpText("Create density maps that distributes accidents by districts."),
      
      selectInput("victims1", 
                  label = "Type of victims",
                  choices = c("All", 
                              "Pedestrians",
                              "Drivers",
                              "Difference between pedestrians and drivers"), # The last one is not implemented
                  selected = "All"),
      
      selectInput("proportion1", 
                  label = "Number of accidents",
                  choices = c("Per Capita", 
                              "Total amount"),
                  selected = "Total amount")
      
    ),
    
    
    column(4, offset=2,
           helpText("Create density maps that distributes accidents by districts."),
           
           selectInput("victims2", 
                       label = "Type of victims",
                       choices = c("All", 
                                   "Pedestrians",
                                   "Drivers",
                                   "Difference between pedestrians and drivers"), # The last one is not implemented
                       selected = "All"),
           
           selectInput("proportion2", 
                       label = "Number of accidents",
                       choices = c("Per Capita", 
                                   "Total amount"),
                       selected = "Total amount")
    )
  ),
  
  column(6, tmapOutput('map1')),
  column(6, tmapOutput('map2')),
  
  # Cluster map
  titlePanel("Sort accidents by clusters"),
  fluidRow(
    column(10,offset=1,
      leafletOutput("cluster"))
  ),
  
  # Streamgraph
  titlePanel("Streamgraph"),
  fluidRow(
    column(10,offset=1,
           selectInput("year", 
                       label = "Current year",
                       choices = c("2019", 
                                   "2020",
                                   "2021"),
                       selected = "2021"),
           checkboxGroupInput(
             "checkbox",
             "Weather selected",
             choices = c("Clear","Hailing","Light rain","Heavy rain",
                        "Cloudy", "Unknown"),
             selected = c("Clear","Cloudy")
           ),
           plotOutput("stream"))
  )
)

server <- function(input, output) {
  ###########################################################
  # Districts
  ###########################################################
  dataf1 <- reactive({
    districts <- readOGR( 
      dsn= paste0(getwd(),"/data/districts"),
      layer= "Distritos_20210712",
      verbose=FALSE
    )
    # First map
    if(input$victims1 == "Pedestrians"){
      accidents.filtered <- accidents[accidents$tipo_persona == "Peatón",]
    }
    else if(input$victims1 == "Drivers"){
      accidents.filtered <- accidents[accidents$tipo_persona == "Conductor",]
    }
    else{
      accidents.filtered <- accidents
    }
    
    counts <- accidents.filtered %>% count(distrito)
    
    districts$NOMBRE <- str_replace_all(toupper(districts$NOMBRE), " - ", "-")

    df <- merge(districts, counts, by.x="NOMBRE", by.y="distrito")
    
    population <- read.csv("data/population.csv")
    df <- merge(df, population, by.x="NOMBRE", by.y='distrito_nombre')
    df$proportion <- df$n / df$poblacion * 1e5
    df
  })
  
  column1 <- reactive({
    if(input$proportion1 == "Per Capita"){
      'proportion'
    }
    else{
      'n'
    }
  })
  
  legend1 <- reactive({
    if(input$proportion1 == "Per Capita"){
      'Accidents per 100.000 habitants'
    }
    else{
      'Accidents'
    }
  })
  
  output$map1 <- renderTmap({
    tm_shape(dataf1()) + 
      tm_polygons(col=column1(), title = legend1()) + 
      tm_scale_bar(position = c("right", "top")) +
      tm_text("NOMBRE", size = 0.7)
  })
  
  # Second map
  dataf2 <- reactive({
    districts <- readOGR( 
      dsn= paste0(getwd(),"/data/districts"),
      layer= "Distritos_20210712",
      verbose=FALSE
    )
    if(input$victims2 == "Pedestrians"){
      accidents.filtered <- accidents[accidents$tipo_persona == "Peatón",]
    }
    else if(input$victims2 == "Drivers"){
      accidents.filtered <- accidents[accidents$tipo_persona == "Conductor",]
    }
    else{
      accidents.filtered <- accidents
    }
    
    counts <- accidents.filtered %>% count(distrito)
    
    districts$NOMBRE <- str_replace_all(toupper(districts$NOMBRE), " - ", "-")
    
    df <- merge(districts, counts, by.x="NOMBRE", by.y="distrito")
    
    population <- read.csv("data/population.csv")
    df <- merge(df, population, by.x="NOMBRE", by.y='distrito_nombre')
    df$proportion <- df$n / df$poblacion * 1e5
    df
  })
  
  column2 <- reactive({
    if(input$proportion2 == "Per Capita"){
      'proportion'
    }
    else{
      'n'
    }
  })
  
  legend2 <- reactive({
    if(input$proportion2 == "Per Capita"){
      'Accidents per 100.000 habitants'
    }
    else{
      'Accidents'
    }
  })
  
  output$map2 <- renderTmap({
    tm_shape(dataf2()) + 
      tm_polygons(col=column2(), title = legend2()) + 
      tm_scale_bar(position = c("right", "top")) +
      tm_text("NOMBRE", size = 0.7)
  })
  
  ##########################################
  # Cluster map
  ##########################################
  # geodata <- reactiveFileReader(1e4,NULL,"data/AccidentesLatLong.csv",read.csv)
  geodata <- reactive({
    df <- read.csv("data/AccidentesLatLong.csv")
    df
  })

  output$cluster <- renderLeaflet({
    leaflet(geodata()) %>% addTiles() %>% addMarkers(
      lat = ~coordenada_x_utm, lng= ~coordenada_y_utm,
      clusterOptions = markerClusterOptions()
    )
  })
  
  #########################################
  # Streamgraph
  ########################################
  counts <- reactive({
    df <- geodata() %>%
      filter(geodata()$estado_meteorológico %in% input$checkbox)
    df$fecha <- as.Date(df$fecha,'%d/%m/%Y')
    df <- df %>% 
      filter(format(df$fecha, "%Y") == input$year)%>%
      group_by(fecha, estado_meteorológico) %>%
      tally()
    df
  }) 

  output$stream <- renderPlot(
    ggplot(counts(), aes(x = fecha, y = n, fill = estado_meteorológico)) +
    geom_stream() + guides(fill=guide_legend(title="Weather"))
  )
}

shinyApp(ui, server)