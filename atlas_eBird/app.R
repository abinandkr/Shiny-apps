library(shiny)
library(leaflet)
library(dplyr)
library(rgdal)
library(DT)
library(sp)
library(tidyr)

geo_grid <- function(lt){
  long1 <- as.numeric(lt[1])
  long2 <- as.numeric(lt[2])
  lat1 <- as.numeric(lt[3])
  lat2 <- as.numeric(lt[4])
  t <- matrix(c(long1,lat1,long1,lat2,long2,lat2,long2,lat1,long1,lat1), ncol = 2,byrow = T)
  t
}


grid_polygon <- function(cells){
  
  mp <- list()
  
  for(i in 1:nrow(cells)){
    ts <- Polygons(list(Polygon(geo_grid(cells[i,1:4]))),ID = cells[i,5])
    mp <- c(mp,ts)
  }
  
  polys <- SpatialPolygons(mp)
  proj4string(polys) = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
  polys
}


kl <- read.csv('lists_species.csv')
kl.loc <- read.csv('lists_location.csv') 

all<- read.csv('Kerala Sub-cells and Cells.csv', skip = 1, stringsAsFactors = TRUE)


# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  
  
  # Application title
  titlePanel("Kerala Atlas Species Distribution"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.page == "View Data"',
        fileInput('file2', 'Enter csv with lists matched to Subcell ID',accept = '.csv'),
        downloadButton('downloadData', 'Download')
      ),
      
      conditionalPanel(
        'input.page == "Species Distribution"',
        uiOutput('box1')
        
      )
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        id = 'page',
        tabPanel('View Data', dataTableOutput('outable')),
        tabPanel('Species Distribution',
                 fluidRow(
                   column(5,leafletOutput('mymap', height = '600px')),
                   column(5,leafletOutput('ebirdmap', height = '600px'))
                 ),
                 helpText(h4('Click on a grid or Subcell to get the containing subcells and lists below. Click on a list ID to view it on eBird')), dataTableOutput('grid_det'))
      )  
    )
  )
))



# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  
  list_dat <- reactive({
    lsdat <- input$file2
    if (is.null(lsdat)) return(NULL)
    lsd <- read.csv(lsdat$datapath, col.names = c('subcell', 'listid'))
    lsd
  })
  
  output$outable <- renderDataTable({
    list_dat()
  })
  
  thisdata <- reactive({
    kld <- kl %>% filter(SAMPLING.EVENT.IDENTIFIER %in% list_dat()$listid)
    kld
  })
  
  output$box1 <- renderUI({
    
    kl1 <- kl %>% filter(SAMPLING.EVENT.IDENTIFIER %in% list_dat()$listid)
    
    kl1$SPECIES.NAME <- factor(kl1$SPECIES.NAME)
    
    selectInput('species', label = h4('Select Species'), choices = levels(kl1$SPECIES.NAME))
  })
  
  
  output$downloadData <- downloadHandler(
    filename = 'shiny_atlas_dataset.csv',
    content = function(file) {
      write.csv(thisdata(), file)
    }
  )
  
  
  
  map_dat_sp <- reactive({
    
    map_dat <- kl %>% filter(SAMPLING.EVENT.IDENTIFIER  %in% list_dat()$listid)
    
    map_dat <- left_join(map_dat,list_dat(), by = c('SAMPLING.EVENT.IDENTIFIER' = 'listid'))
    
    map_dat <- data.frame(table(map_dat$SPECIES.NAME,map_dat$subcell))
    
    colnames(map_dat) <- c('species.name','subcell','frequency')
    
    visits <- data.frame(table(list_dat()$subcell))
    
    colnames(visits) <- c('subcell','visits')
    
    map_dat <- left_join(map_dat,visits, by = c('subcell' = 'subcell'))
    
    map_dat$grid <- all$Cell[match(map_dat$subcell,all$Sub.Cell.Name)]
    
    map_dat2 <- map_dat %>% mutate(perc = frequency/visits*100) 
    
    map_dat_sp1 <- map_dat2 %>% filter(species.name == input$species)
    
    map_dat_sp1
    
  })
  
  grids <- reactive({
    cells <- all %>% filter(Sub.Cell.Name %in% list_dat()$subcell)
    
    cells$Cell <- as.character(cells$Cell)
    
    allcells <- cells[,9:14]

    gridcells <- allcells %>% group_by(Cell) %>% slice(1)
    
    grids <- grid_polygon(gridcells[,2:6])    
    
    grids
  })
  
  
  output$mymap <- renderLeaflet({
    
    if(is.null(list_dat()) == T) return(NULL)
    
    cells <- all %>% filter(Sub.Cell.Name %in% list_dat()$subcell)
    
    allsubcells <- cells[,c(1:4,9)]
    
    subc <- grid_polygon(allsubcells)
    
    map_dat_sp_sub <- map_dat_sp()
    
    rownames(map_dat_sp_sub) <- map_dat_sp_sub$subcell  
    
    map_dat_sp_subdf <- SpatialPolygonsDataFrame(subc,map_dat_sp_sub, match.ID = T)
    
    map_dat_sp_grid <- map_dat_sp() %>% group_by(grid) %>% summarise(percentage = round(mean(perc),2)) %>% ungroup()
    
    rownames(map_dat_sp_grid) <- map_dat_sp_grid$grid  
    
    map_dat_spdf <- SpatialPolygonsDataFrame(grids(),map_dat_sp_grid, match.ID = T)
    
    binpal <- colorNumeric("Reds",domain = c(1,100), na.color = 'white')
    
    leaflet() %>% 
      addTiles() %>% 
      addPolylines(data = map_dat_spdf, color = 'grey',weight = 1, group = 'Grid Lines') %>%
      addPolygons(data = map_dat_spdf, layerId = as.vector(map_dat_spdf$grid),weight = 1, fillColor = ~binpal(percentage), fillOpacity = .7, stroke = F, color = 'gray', popup = paste(paste('Grid = <b>', map_dat_spdf$grid,'</b>', sep = ''), paste('Percentage = <b>', map_dat_spdf$percentage,'%</b>', sep = ''), sep = '<br/>'), group = 'Species Distribution') %>%
      addPolygons(data = map_dat_sp_subdf,layerId = as.vector(map_dat_sp_subdf$subcell), fillColor = ~binpal(perc),  fillOpacity = .7, weight = 1, color = 'black', group = 'Subcells',popup = paste(paste('Subcell = <b>',map_dat_sp_subdf$subcell,'</b>',sep = ''), paste('Percentage = <b>',map_dat_sp_subdf$perc,'%</b', sep = ''), sep = '<br/>')) %>%
      addLegend('topright',binpal,values = c(0,100), labFormat = labelFormat(suffix = '%')) %>%
      hideGroup('Subcells') %>%
      addLayersControl(overlayGroups = c('Grid Lines','Species Distribution','Subcells'), position = 'bottomleft', options = layersControlOptions(collapsed = FALSE))
  })
  
  
  output$ebirdmap <- renderLeaflet({
    
    kl.loc <- kl.loc %>% filter(!SAMPLING.EVENT.IDENTIFIER %in% list_dat()$listid)
    
    kl.loc1 <- kl.loc
    
    coordinates(kl.loc) <- ~LONGITUDE+LATITUDE
    
    proj4string(kl.loc) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    
    kl.loc_gr <- over(grids(),kl.loc, returnList = T)
  
    kl.loc_gr <- do.call(rbind,kl.loc_gr)
    
    kl.loc_gr$grid <- rownames(kl.loc_gr)
    
    kl.loc1 <- kl.loc1 %>% filter(SAMPLING.EVENT.IDENTIFIER %in% kl.loc_gr$SAMPLING.EVENT.IDENTIFIER)
    
    kl.loc_gr <- kl.loc_gr %>% separate(grid, c('Grid','no'), sep = ']', remove = T)
    
    kl.loc_gr$grid <- paste(kl.loc_gr$Grid,']', sep = '')
    
    kl.loc_gr <- kl.loc_gr %>% select(SAMPLING.EVENT.IDENTIFIER,grid)
   
    grids1 <- grids()
    
    grids1 <- grids1[unique(kl.loc_gr$grid),]
    
    kl.sp_gr <- left_join(kl.loc_gr,kl, by = c("SAMPLING.EVENT.IDENTIFIER" = "SAMPLING.EVENT.IDENTIFIER"))
    
    visitse <- kl.sp_gr %>% group_by(grid) %>% summarise(visits = n_distinct(SAMPLING.EVENT.IDENTIFIER))
    
    ebird_freq <- data.frame(table(kl.sp_gr$SPECIES.NAME,kl.sp_gr$grid))
    
    colnames(ebird_freq) <- c('species.name','grid','frequency')
    
    colnames(visitse) <- c('grid','visits')
    
    ebird_freq <- left_join(ebird_freq,visitse, by = c('grid' = 'grid'))
    
    ebird_freq_sp <- ebird_freq %>% filter(species.name == input$species)
    
    ebird_freq_sp$percentage = round(ebird_freq_sp$frequency/ebird_freq_sp$visits*100,2)
    
    rownames(ebird_freq_sp) <- ebird_freq_sp$grid  
    
    ebirdmap_spdf <- SpatialPolygonsDataFrame(grids1,ebird_freq_sp, match.ID = T)
    
    binpal <- colorNumeric("Reds",domain = c(1,100), na.color = 'white')
    
    leaflet() %>% 
      addTiles() %>% 
      addPolylines(data = ebirdmap_spdf, color = 'grey',weight = 1, group = 'Grid Lines') %>%
      addPolygons(data = ebirdmap_spdf, layerId = as.vector(ebirdmap_spdf$grid),weight = 1, fillColor = ~binpal(percentage), fillOpacity = .7, stroke = F, color = 'gray', popup = paste(paste('Grid = <b>', ebirdmap_spdf$grid,'</b>', sep = ''), paste('Percentage = <b>', ebirdmap_spdf$percentage,'%</b>', sep = ''), paste('Visits = <b>', ebirdmap_spdf$visits,'</b>'),sep = '<br/>'), group = 'Species Distribution') %>%
      addCircleMarkers(data = kl.loc1, lng = kl.loc1$LONGITUDE, lat = kl.loc1$LATITUDE, color = 'darkred', opacity = 1,radius = 2, popup = kl.loc1$SAMPLING.EVENT.IDENTIFIER, group = 'lists') %>%
      addLegend('topright',binpal,values = c(0,100), labFormat = labelFormat(suffix = '%')) %>%
      hideGroup('lists') %>%
      addLayersControl(overlayGroups = c('Grid Lines','Species Distribution','lists'), position = 'bottomleft', options = layersControlOptions(collapsed = FALSE))
    })
  
  

  output$grid_det <- renderDataTable({
    gridid <- input$mymap_shape_click
    
    ebirdid <- input$ebirdmap_shape_click
    
    if (is.null(gridid) == T) return(NULL)
    
    scls <- all %>% filter(Cell == gridid$id)
    
    gridls <- list_dat() %>% filter(subcell %in% scls$Sub.Cell.Name) 
    
    if (nrow(scls) == 0) gridls <- list_dat() %>% filter(subcell == gridid$id)
    
    pres <- kl %>% filter(SPECIES.NAME == input$species & SAMPLING.EVENT.IDENTIFIER %in% gridls$listid)
    
    gridls <- gridls %>% mutate(Present = listid %in% pres$SAMPLING.EVENT.IDENTIFIER) %>% arrange(desc(subcell))
    
    gridls$listid <- paste0('<a href=http://ebird.org/ebird/view/checklist/',gridls$listid," target=\"_blank\">", gridls$listid,"</a>")
    
    DT::datatable(gridls, options = list(paging = F,searching = F), escape = F)
    
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)

