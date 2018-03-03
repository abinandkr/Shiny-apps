library(shiny)
library(sp)
library(leaflet)
library(leaflet)
library(dplyr)
library(rgdal)

spiti_dat <- read.csv('spiti_lists.csv')

spiti_dat <- spiti_dat %>% filter(DURATION.MINUTES > 10 & CATEGORY %in% c("species","domestic","issf","form") & APPROVED == 1 & ALL.SPECIES.REPORTED ==1)

spiti_grid <- readOGR('.','Spiti5kmGridClipped')
spiti_grid <- spTransform(spiti_grid, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))

spiti_grid@data$ID %in% spiti_dat$gr

ui <- shinyUI(fluidPage(
   

   titlePanel("Spiti Birds"),
   
   sidebarLayout(
      sidebarPanel(
         uiOutput('box1')
      ),
      
      mainPanel(
         leafletOutput("mymap")
      )
   )
))


server <- shinyServer(function(input, output) {
  
  spiti_lists <- reactive({
    
    coordinates(spiti_dat) <- ~LONGITUDE+LATITUDE
    
    proj4string(spiti_dat) <- proj4string(spiti_grid)
    
    spiti_list <- over(spiti_grid,spiti_dat, returnList = T)
    
    spiti_list <- do.call(rbind,spiti_list)
    
    spiti_list$grid <- do.call('rbind', strsplit(rownames(spiti_list), split = '.',fixed = T))[,1]
    
    spiti_list <- spiti_list %>% group_by(group.id,SPECIES.NAME) %>% slice(1) %>% ungroup()
  })
   
  output$box1 <- renderUI({
    spiti_list <- spiti_lists()
  
    spiti_list$SPECIES.NAME <- factor(spiti_list$SPECIES.NAME)

    selectInput('species', label = h4('Select Species'), choices = levels(spiti_list$SPECIES.NAME))
  })
  
  output$mymap <- renderLeaflet({
    
    visits <- spiti_lists() %>% group_by(grid) %>% summarise(visits = n_distinct(group.id))
    
    spiti_freq <- data.frame(table(spiti_lists()$SPECIES.NAME,spiti_lists()$grid))
    
    colnames(spiti_freq) <- c('species.name','grid','frequency')
    
    colnames(visits) <- c('grid','visits')
    
    spiti_freq <- left_join(spiti_freq,visits, by = c('grid' = 'grid'))
    
    spiti_freq_sp <- spiti_freq %>% filter(species.name == input$species)
    
    spiti_freq_sp$percentage = round(spiti_freq_sp$frequency/spiti_freq_sp$visits*100,2)
    
    rownames(spiti_freq_sp) <- spiti_freq_sp$grid  
    
    spiti_grid1 <- spiti_grid
    
    spiti_grid <- spiti_grid[unique(spiti_freq$grid),]

    spitimap_spdf <- SpatialPolygonsDataFrame(spiti_grid,spiti_freq_sp, match.ID = T)
    
    binpal <- colorNumeric("Reds",domain = c(1,100), na.color = 'white')
    
    leaflet() %>% 
      addTiles() %>% 
      addPolylines(data = spitimap_spdf, color = 'grey',weight = 1) %>%
      addPolygons(data = spiti_grid1, weight = 1, color = 'grey', fillOpacity = 0, group = 'All Grids') %>%
      addPolygons(data = spitimap_spdf, layerId = as.vector(spitimap_spdf$grid),weight = 1, fillColor = ~binpal(percentage), fillOpacity = .7, stroke = F, color = 'gray', popup = paste(paste('Grid = <b>', spitimap_spdf$grid,'</b>', sep = ''), paste('Percentage = <b>', spitimap_spdf$percentage,'%</b>', sep = ''), paste('Visits = <b>', spitimap_spdf$visits,'</b>'),sep = '<br/>'), group = 'Species Distribution') %>%
      addLegend('topright',binpal,values = c(0,100), labFormat = labelFormat(suffix = '%')) %>%
      hideGroup('All Grids') %>%
      addLayersControl(overlayGroups = c('All Grids','Species Distribution'), position = 'bottomleft', options = layersControlOptions(collapsed = FALSE))

    
  }) 
  
  
})

# Run the application 
shinyApp(ui = ui, server = server)

