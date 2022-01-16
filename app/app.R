library(shiny)
library(tmap)
library(dplyr)
library(tidyverse)
library(rgdal)

ui <- fluidPage(
  titlePanel("Accidents by districts"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create density maps that distributes accidents by districts."),
      
      selectInput("victims", 
                  label = "Type of victims",
                  choices = c("All", 
                              "Pedestrians",
                              "Drivers",
                              "Difference between pedestrians and drivers"), # The last one is not implemented
                  selected = "All"),
      
      selectInput("proportion", 
                  label = "Number of accidents",
                  choices = c("Per Capita", 
                              "Total amount"),
                  selected = "Total amount") # Not implemented yet
    ),
    
    mainPanel(
      tmapOutput('map')
    )
  )
)

server <- function(input, output) {
  dataf <- reactive({
    districts <- readOGR( 
      dsn= paste0(getwd(),"/data/districts"),
      layer= "Distritos_20210712",
      verbose=FALSE
    )
    
    
    if(input$victims == "Pedestrians"){
      accidents.filtered <- accidents[accidents$tipo_persona == "Peatón",]
    }
    else if(input$victims == "Drivers"){
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
    df$proportion <- df$n / df$poblacion
    df
  })
  
  column <- reactive({
    if(input$proportion == "Per Capita"){
      'proportion'
    }
    else{
      'n'
    }
  })
  
  output$map <- renderTmap({
    tm_shape(dataf()) + 
      tm_polygons(col=column(), title = "Accidents") + 
      tm_scale_bar(position = c("right", "top")) +
      tm_text("NOMBRE", size = 0.7)
  })
  
}

shinyApp(ui, server)