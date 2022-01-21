library(shiny)
library(tmap)
library(dplyr)
library(tidyverse)
library(rgdal)
library(ggstream)
library(leaflet)
library(fmsb)
library(lubridate)

ui <- fluidPage(
  navbarPage("Navigation bar",
             tabPanel("Accidents by districts",
                      titlePanel("Accidents by districts"),
                      helpText("Create density maps that distributes accidents by districts. Two maps have
                               been juxtaposed in order to allow the user to compare different situations."),
                      # Districts
                      fluidRow(
                        # First parameter is the length (maximum: 12)
                        column(4, offset=1,
                               
                               
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
                               
                               selectInput("victims2", 
                                           label = "Type of victims",
                                           choices = c("All", 
                                                       "Pedestrians",
                                                       "Drivers"),
                                           selected = "All"),
                               
                               selectInput("proportion2", 
                                           label = "Number of accidents",
                                           choices = c("Per Capita", 
                                                       "Total amount"),
                                           selected = "Total amount")
                        )
                      ),
                      
                      column(6, tmapOutput('map1')),
                      column(6, tmapOutput('map2'))),
             # Cluster map
             tabPanel("Cluster map",
                      titlePanel("Sort accidents by clusters"),
                      helpText("Accidents have been distributed in auto-generated clusters. Zooming the map
                               cause a recalculation of these clusters."),
                      fluidRow(
                        column(10,offset=1,
                               leafletOutput("cluster"))
                      )),
             # Streamgraph
             tabPanel("Streamgraph",
                      titlePanel("Streamgraph"),
                      helpText("This graph allow the user to explore the relevance of the weather 
                               during different years."),
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
                      )),
             # Spider chart
             tabPanel("Radar chart",
                      titlePanel("Seasonality of accidents by district"),
                      helpText('This radar chart allows the user to explore the relationship between
                               seasonality, districts and number of accidents.'),
                      fluidRow(
                        column(4, offset=1,
                               selectInput("districts", "Select a district", c("All")))
                      ),
                      fluidRow(
                        column(10,offset=1,
                               plotOutput("spider"))
                      )
                      )
             )
)

server <- function(input, output, session) {
  ###########################################################
  # Districts
  ###########################################################
  dataf1 <- reactive({
    districts <- readOGR( 
      dsn= paste0(getwd(),"/data/districts"),
      layer= "Distritos_20210712",
      verbose=FALSE
    )
    accidents <- read.csv("data/AccidentesBicicletas.csv")
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
    accidents <- read.csv("data/AccidentesBicicletas.csv")
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
  
  #############################################
  # Radial chart
  #############################################
  # District selection
  # districts <- 
    
    df <- read.csv("data/population.csv")
    updateSelectInput(session, "districts",
                      choices = c("ALL",as.vector(df$distrito_nombre))
                      # selected = tail(x, 1)
    )
  
  season <- reactive({
    # Language must be English
    Sys.setlocale("LC_TIME", "C")
    
    df <- read.csv("data/AccidentesBicicletas.csv")
    if(input$districts != "ALL"){
      df <- df %>% filter(df$distrito == input$districts)
    }
    df <- df %>%
      mutate(fecha = as.Date(fecha, "%d/%m/%Y"), acc_id = 1, fecha = as.Date(fecha, "%d/%m/%Y"), month = months(fecha), year =  as.character(year(fecha))) %>% 
      group_by(year, month) %>%
      summarise(accident_num = sum(acc_id, na.rm = TRUE)) %>%
      ungroup() %>%
      spread(month, accident_num) %>% as.data.frame()
    
    reorder <- rev(c("year",month.name))
    df <- df[,reorder]
    
    df <- mutate_all(df, ~replace(., is.na(.), 0))
    
    max_accidents <- df %>% gather(month, acc_n, -year) %>% select(acc_n) %>% filter(acc_n == max(acc_n)) %>% pull()
    
    rownames(df) <- select(df, year) %>% pull()
    
    df_2 <- df %>% select(-year)
    
    df_2 <- rbind(rep(max_accidents,12) , rep(0,5) , df_2)
    
    df_2
  })
  
  output$spider <- renderPlot({
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
    
    radarchart( season()  , axistype=1 , 
                #custom polygon
                pcol=colors_border , 
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                #custom labels
                vlcex=0.8 
    )
    legend(x=1.3, y=1.2, legend = rownames(season()[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
  })
}

shinyApp(ui, server)