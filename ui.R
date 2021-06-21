library(sf)
library(sp)
library(rgdal)
library(leaflet)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggtern)
library(tricolore)
library(shiny)
library(grid)
library(png)
library(stringr)
library(Ternary)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%;padding:2%;font-size: 16px}",  
             ".leaflet-container { background: #fff; }"),
  
  # leafletOutput("mymap", width = "100%", height = "100%"),
  fluidRow(
    
    column(width =3,  uiOutput('triadChoices')),
    column(width =2, uiOutput('versionChoices')),
    column(width =3, uiOutput('timeChoices')),
    column(width =2, uiOutput('geoChoices')),
    column(width =1, checkboxInput("discrete", "Discrete Triad", FALSE))
  ), 
  
  fluidRow(
    column(4, plotOutput("gg"),
           # p('Each point above represents responese aggregated at selected geography level', style = 'font-size:8', align = 'center')
           ), 
    column(8, leafletOutput("mymap"))
  ),
  

  fluidRow(
    
    column(4, plotOutput("ggtriads", click = "triad_click")),
    column(8, htmlOutput("text"))

                
                # htmlOutput("text",),
                # # htmlOutput("story"),
                # cellArgs = list(style='white-space: normal;')
    
  )

  
)
