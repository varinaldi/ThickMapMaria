
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
library(compositions)



# set up server
source('functions.R')
source('ericv4.R')


server <- function(input, output, session) {
  
  # Build data, would be replaced by the csv loading in your case
  
  selectTriad = list('Impact the event had on' = "X2.1", 
                     'People responded by' = "X2.2", 
                     'What was valued in community' = "X2.3", 
                     'Situation could have been improved with better' = "X2.4", 
                     'Critical to recovery' = "X2.5")
  
  selectGeo = list('Municipio', 'PREPA', 'Elevation', 'Landcover')
  
  selectVersion = list('Maria', 'Earthquake 2020')
  
  

  ## ----------------------------------- get geography -------------------------------------
  
  f= file.path('geography',list.files('geography', pattern = '.rds'))
  
  pc = read_rds(f[7])
  nl = read_rds(f[6])
  cdcsvi = read_rds(f[1])
  
  # pc$Pop_1km = as.numeric(as.character(pc$PR_Populat))/as.numeric(as.character(pc$area_km))
  
  pal = colorNumeric("YlGnBu", pc$Pop_1km, n= 6)

  palElev = colorNumeric("YlGn",  pc$X_mean, n=6)
  
  palBRI = colorNumeric("PuBu",  pc$BRI, n=6)
  
  palCC = colorNumeric("BuPu",  pc$BRI_CC, n=6)
  
  palNL = colorNumeric("magma",  nl$months_t_3, n=6, reverse = TRUE)
  
  palCDC = colorNumeric("inferno",  cdcsvi$RPL_THEMES, n=6, reverse = TRUE)

  
  ## --------------------------------- where the actual server is lol -----------------------------------------
  # Render selectInput 
  output$timeChoices <- renderUI({
    choices <- as.vector(unique(sm$storyTime) )
    selectInput("choices","Story Time", choices=choices, multiple=TRUE)    
  })
  
  # Render selectInput 
  output$triadChoices <- renderUI({
    selectInput("triadChoices","Triad Question", choices=selectTriad, multiple=FALSE)    
  })
  
  output$geoChoices <- renderUI({
    selectInput("geoChoices","Geography", choices=selectGeo, multiple=FALSE)    
  })
  
  output$versionChoices <- renderUI({
    selectInput("versionChoices","Collection", choices=selectVersion, multiple=TRUE)    
  })
  
  
  ## -------- in-server functions -------
  
  createLabel = function(n){
    return(str_wrap(n %>% 
                      str_remove_all('X|[0-9]\\.') %>%
                      str_replace_all('\\.', ' ') %>%
                      str_replace(names(
                        Filter( function(x) x == input$triadChoices, selectTriad)), '')%>% 
                      str_replace_all(' n ', '\n') %>% str_to_sentence(), width = 15 )
    )
  }
  
  geommean = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
  
  
  
  ## ------------ set up reactives ------------
  geo <- reactive({
    
    readRDS(f %>% str_subset(input$geoChoices %>% str_to_lower()))
    
  })
  
  # SPATIAL JOIN
  smgeo <- reactive({
  
    # sm here is the final data from ericv4.R
    sm %>% st_intersection(geo()) %>% st_drop_geometry()
    
  })
  
  versed <- reactive({
    
    if(!is.null(input$versionChoices)){
      
      filter(smgeo(),  v %in% input$versionChoices)
      
    } else( smgeo())
    
    
  })
  
  
  # Subset so that only the selected rows are in time
  filtered <- reactive({
    
    if(!is.null(input$choices) ){
      
      filter(versed(),  storyTime %in% input$choices)
      
    } else( versed())

    
  })
  
  
  
  
  # Subset data for the triad
  allData <- reactive({
    
    validate(
      need(!is.na(smgeo()), "Loading")
    )
    
    # see if time variable is added
    if (!is.null(input$choices) || !is.null(input$versionChoices)){
      
      filtered()
      
    } else( smgeo() )

  })
  
  # summarise based on triads
  triad <- reactive({
    
    allData() %>% select('name',starts_with(input$triadChoices)) %>% 
      filter( is.na(select(., contains('.NA'))) ) %>%
      group_by(name) %>%  summarise_all( geommean,  na.rm = TRUE)
    
  })
  
  # Using the function of tricolore to get colors
  tric <- reactive({
    d = triad()[1:4]
    
    if (input$discrete != TRUE) {
      
      Tricolore(d, names(d)[2], names(d)[3], names(d)[4], 
                center = NA, show_center = FALSE)
      
    } else ( Tricolore(d, names(d)[2], names(d)[3], names(d)[4]))
    
  })
  
  
  
  ## store selected municipio or geometry
  rv <- reactiveValues()
  rv$selectedMuni = NULL
  
  ## ----------------- map obviously ------------------
  output$mymap <- renderLeaflet({
    
    d = triad()[1:4] %>% mutate(rgb = tric()$rgb)
    
    muni = geo() %>% inner_join(d , by = 'name') 
    
    muni%>%
      leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 8) ) %>%
      setView(-66.517637, 18.236296, zoom = 8.5) %>%
      setMaxBounds(-67.287183,17.925067,-65.576062,18.561544)%>%
      # addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addPolygons(data = geo(), smoothFactor = 0.1, weight = 0.5,
                  color = 'grey',
                  group= 'Stories',
                  fillOpacity = 0.2) %>%
      
      # ------------ map the stories --------------
      addCircleMarkers(data = filtered(),
                       lng = filtered()$Longitude,
                       lat = filtered()$Latitude,
                       group = 'Stories',
                       radius = 0.05, color = 'black', opacity = 0.5) %>%
      
      # ---------- add extra layers ---------------------
      addPolygons(data = pc, smoothFactor = 0.1, weight = 0.2,
                  color = 'white',
                  # layerId = ~municipio,
                  fillColor = ~pal(Pop_1km),
                  fillOpacity = 1,
                  group = 'Pop Density', 
                  popup =
                    paste0( '<b>', pc$municipio, '</b></br>', 
                            'Population Density: ', pc$Pop_1km)) %>%
      
      addPolygons(data = pc, smoothFactor = 0.1, weight = 0.2,
                  color = 'white',
                  # layerId = ~municipio,
                  fillColor = ~palBRI(BRI),
                  fillOpacity = 1,
                  group = 'BRI',
                  popup =
                    paste0( '<b>', pc$municipio, '</b></br>',
                            'Baseline Resilience Index: ', pc$BRI)) %>%

      addPolygons(data = pc, smoothFactor = 0.1, weight = 0.2,
                  color = 'white',
                  # layerId = ~municipio,
                  fillColor = ~palCC(BRI_CC),
                  fillOpacity = 1,
                  group = 'Community Capital',
                  popup =
                    paste0( '<b>', pc$municipio, '</b></br>', 
                            'Community Capital: ', pc$BRI_CC)) %>%
      
      addPolygons(data = nl, smoothFactor = 0.1, weight = 0.2,
                  color = 'white',
                  # layerId = ~municipio,
                  fillColor = ~palNL(months_t_3),
                  fillOpacity = 1,
                  group = 'Months to 70 Capacity') %>%
      
      addPolygons(data = cdcsvi, smoothFactor = 0.1, weight = 0.2,
                  color = 'white',
                  # layerId = ~municipio,
                  fillColor = ~palCDC(RPL_THEMES),
                  fillOpacity = 1,
                  group = 'CDC SVI') %>%
      
      addPolygons(data = pc, smoothFactor = 0.1, weight = 0.2,
                  color = 'white',
                  # layerId = ~municipio,
                  fillColor = ~palElev(X_mean),
                  fillOpacity = 1,
                  group = 'Elevation'
                  ) %>%
      
      addPolygons(smoothFactor = 0.1, weight = 0,
                  layerId = ~name,
                  fillColor = muni$rgb,
                  fillOpacity = 1,
                  group = 'Triad',
                  popup =
                    paste0( '<b>', muni$name, '</b></br>' )) %>%
      
    
      # ## legends
      addLegend("bottomright",data = pc, 
                pal = pal, values = ~Pop_1km, group = "Pop Density", 
                title = 'Population Density') %>%
      addLegend("bottomright",data = pc,
                pal = palBRI, values = ~BRI, group = "BRI", 
                title = 'BRI') %>%
      addLegend("bottomright", data = pc, 
                pal = palCC, values = ~BRI_CC, group = "Community Capital", 
                title = 'BRI Community Capital') %>%
      addLegend("bottomright", data = cdcsvi, 
                pal = palCDC, values = ~RPL_THEMES, group = 'CDC SVI', title = 'CDC VSI') %>%
      addLegend("bottomright", data = pc, 
                pal = palElev, values = ~X_mean, group = 'Elevation', title = 'Meters') %>%
      addLegend("bottomright", data = nl, 
                pal = palNL, values = ~months_t_3, group = "Months to 70 Capacity",
                na.label = '', 
                title = 'Months') %>%
      # 
      ## layer control
      addLayersControl(

        overlayGroups = c('Triad', 'Stories',
                          'Pop Density',
                          'BRI',
                          'Community Capital',
                          'CDC SVI',
                          'Elevation',
                          'Months to 70 Capacity'),
        options = layersControlOptions(collapsed = T)) %>% 
      
      hideGroup(c('Stories', 'Pop Density',
                  'BRI',
                  'Community Capital', 
                  'CDC SVI',
                  'Elevation',
                  'Months to 70 Capacity')) 
    
  })
  
  
  clickedIds <- reactiveValues(ids = vector())
  
  ## ------------- on click event - add municipio to storage --------------
  observeEvent(input$mymap_shape_click, {
    

    
    click <- input$mymap_shape_click
    rv$selectedMuni <- click$id
    # print(rv$selectedMuni)
    
    d = triad()[1:4] %>% mutate(rgb = tric()$rgb)
    
    # muni = geo() %>% inner_join(d , by = 'name')
    
    clickedIds$ids <- c(clickedIds$ids, click$id)
    
    print(clickedIds$ids )

    if(any(duplicated(clickedIds$ids ))){
      
      dd = clickedIds$ids[duplicated(clickedIds$ids)]
      
      clickedIds$ids  = clickedIds$ids[!clickedIds$ids %in% dd]
      

    } 
    
    print(clickedIds$ids)
    
    sl = geo() %>% filter(name %in% clickedIds$ids)
    
    leafletProxy("mymap") %>%
      addPolygons(data = sl,
                  smoothFactor = 0.1, weight = 3,
                  # layerId = ~name,
                  fillOpacity = 0,
                  group = 'Selected',
                  color = 'white') %>%
      addLayersControl(
        
        overlayGroups = c('Selected','Triad', 'Stories',
                          'Pop Density',
                          'BRI',
                          'Community Capital',
                          'CDC SVI',
                          'Months to 70 Capacity'),
        options = layersControlOptions(collapsed = T)) %>% 
      
      hideGroup(c('Stories', 'Pop Density',
                  'BRI',
                  'Community Capital', 
                  'CDC SVI',
                  'Months to 70 Capacity')) 
 
    
  })
  
  observeEvent(input$mymap_click,{
    rv$selectedMuni <- NULL
    
    leafletProxy("mymap") %>% clearGroup('Selected')
      
    clickedIds$ids <- c()
    
    print(clickedIds$ids)
    
  })
  
  ## use to create selected geography
  muniFiltered <- reactive({filter(triad(), name %in% clickedIds$ids ) })
  
  
  ## ----------- Render triad for colorkey ------------
  output$gg <- renderPlot({
    
    d = triad()[1:4]
    
    t = tric()$key +
      labs(L = createLabel(names(d)[2]),
           T = createLabel(names(d)[3]),
           R = createLabel(names(d)[4]), 
           subtitle = 'Each point represents responese aggregated \nat selected geography level') + 
      theme(tern.axis.title.T = element_text(angle = 0, size = 14 ),
            tern.axis.title.L = element_text(angle = 0, size = 14 ),
            tern.axis.title.R = element_text(angle = 0, size = 14),
            plot.subtitle= element_text(vjust = 10 , size = 10),
            plot.margin = unit(c(1, 1, 1, 1), "mm")) + 
      theme_hidelabels() + theme_hideticks()

    
    t$layers[[3]]$aes_params$size = 3
    
    
    
    gt <- ggplotGrob(t)
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    
    
    if (!is.null(rv$selectedMuni)){
      
      addP = annotate(geom  = 'point',
                      x     = unlist(muniFiltered()[2]),
                      y     = unlist(muniFiltered()[3]),
                      z     = unlist(muniFiltered()[4]),
                      size = 3,
                      color = c("red"))
      
      addL = annotate(geom  = 'text',
                      x     = unlist(muniFiltered()[2]),
                      y     = unlist(muniFiltered()[3]),
                      z     = unlist(muniFiltered()[4]),
                      vjust = -1,
                      hjust = -0.05,
                      size = 5,
                      color = c("red"),
                      label = muniFiltered()$name )
      
      gt <- ggplotGrob(t + addP + addL)
      gt$layout$clip[gt$layout$name == "panel"] <- "off"
      
      grid.draw(gt)
      
    } else(grid.draw(gt))
    
  })
  
  # ---------------------------------------------------------------------------
  # -------------------------------- LOWER HALF -------------------------------
  # ---------------------------------------------------------------------------
  

  
  
  # background all respondent
  smgeoT <- reactive({
    
    allData() %>% select(c(starts_with(input$triadChoices), storyTime))  %>% 
      filter( is.na(select(., contains('.NA'))) )
    
 
  })
  
  
  ## point for selected municipio
  muniT <- reactive({
    
    validate(
      need(clickedIds$ids != "", "Click on a geography to see respective data below")
    )
    
    smM =allData() %>% select('name',starts_with(input$triadChoices),
                            "X1.1.FragmentEntry","X1.2.StoryTitle",
                            contains(c('X6.1', 'X6.2', 'X6.4', 'X6.6', 'X6.7')), 
                            'v' ) %>% 
      filter( is.na(select(., contains('.NA'))) ) %>%
      filter(name %in% clickedIds$ids ) %>% 
      drop_na(!!sym(names(.)[2]))
    
    coord = t(apply(smM[c(3,4,2)] , 1, TernaryCoords))
    
    smM %>% bind_cols(x_var = coord[,1], y_var = coord[,2])
    
  })
  
  
  
  ## --------------- render triad for the selected municipio ------------------
  output$ggtriads <- renderPlot({
    
    par(mar = c(5, 0, 1, 0))
    TernaryPlot(grid.lines=5, grid.lty='dotted',
                padding = 0, 
                grid.minor.lines=1, grid.minor.lty='dotted', 
                axis.labels = FALSE,
                axis.tick = FALSE)
    
    mtext(createLabel(names(triad())[4]), side=1, line =-1, adj = 1,  cex = 1.2)
    mtext(createLabel(names(triad())[2]), side=1, line =-1, adj = 0,  cex = 1.2)
    mtext(createLabel(names(triad())[3]), side=3,  line = -2,  cex = 1.2)
    
    # TernaryDensityContour(muniT()[c(3,4,2)], resolution = 500L, col = 'red' )
    TernaryPoints(na.omit(smgeoT()[c(2,3,1)]),  col = '#7F7F7E', pch = 1, cex = 0.8) 
    TernaryPoints(muniT()[c(3,4,2)],  col = 'black', pch = 19) 
    
    
    mtext(paste("Responses from", 'selected municipios', 'in black dots \n compared to all responses in grey circle'), line =2, side=1, cex = 1)
    

  })
  
  ## ----------------------- info -----------------------
  output$text <- renderUI({
    
    data = nearPoints(muniT(), input$triad_click, xvar = 'x_var', yvar = 'y_var',)[c(10:15)]
    story = nearPoints(muniT(), input$triad_click, xvar = 'x_var', yvar = 'y_var')[c(9:8)]
    
    municipal = paste( '<b> Selected point on', rv$selectedMuni,  'triad </b>')
    str1 <- paste("Age Group: ", data[1,1])
    str2 <- paste("Gender: " , data[1,2])
    str3 <- paste("Marital Status: " , data[1,3])
    str4 <- paste("Annual Household Income: ", data[1,4])
    str5 <- paste("Highest Educational Attainment: ", data[1,5])
    str6 <- paste("SM Version: ", data[1,6])
    
    tt = paste("<br/>", "<b>" , story[1,1], '</b>')
    tts = paste(story[1,2])
    
    HTML(
      if (!is.null(input$triad_click) ){
        
        paste( municipal, str1, str2, 
               str3, str4, str5, str6,
               tt, tts,
               sep = '<br/>')
        
      } else ( "<b> Select a point on triad to the left to get details </b>")
      
    )
    
  })
  
  
}
