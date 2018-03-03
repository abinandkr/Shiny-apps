library(shiny)
library(leaflet)
library(dplyr)
library(rgdal)
library(DT)
library(ggplot2)
library(mapproj)
library(compare)

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


alist <- read.csv("Lists_Kerala - Wet.csv", col.names = c('subcell', 'listid'))

drylist <- read.csv("Dry Season Lists for Map_Original_NotReplaced (1).csv", col.names = c('subcell', 'listid'))

kl <- readRDS('kerala_dat_may2017.rds')

kl <- kl %>% filter(APPROVED == 1, !CATEGORY %in% c('spuh','slash'))

all<- read.csv('Kerala Sub-cells and Cells.csv', skip = 1, stringsAsFactors = TRUE)

k1 <- readOGR('.','Distrits with eBird code')

k2 <- k1[k1@data$ST_NM == 'Kerala',]



k2@data$DISTRICT <-  factor(k2@data$DISTRICT)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

  
   
   # Application title
   titlePanel("Kerala Atlas Species Distribution"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput('season','Select Season', choices  = c('Dry','Wet')),
        
        conditionalPanel(
          'input.page == "Upload Data"',
          fileInput('file2', 'Enter csv with lists matched to Subcell ID',accept = '.csv'),
          downloadButton('downloadData', 'Download')
        ),
        
        conditionalPanel(
          'input.page == "Species Distribution"',
          uiOutput('box1')

        ),
        
        conditionalPanel(
          'input.page == "Print Maps"',
          uiOutput('box2'),
          checkboxGroupInput('district','Select District',choices = levels(k2@data$DISTRICT), selected = levels(k2@data$DISTRICT)),
          downloadButton('downloadPlot', 'Download Map'),
          downloadButton('downloadAll', 'Download All')          
        )
        
      ),
      # Show a plot of the generated distribution
      mainPanel(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        tabsetPanel(
          id = 'page',
          tabPanel('Species Distribution',leafletOutput('mymap', height = '600px'),helpText(h4('Click on a grid or Subcell to get the containing subcells and lists below. Click on a list ID to view it on eBird')), dataTableOutput('grid_det')),
          tabPanel('Print Maps', plotOutput('print_map',height = '700px')),
          tabPanel('Upload Data', dataTableOutput('outable'))
        )  
      )
   )
))



# Define server logic required to draw a histogram
server <- shinyServer(function(input, output,session) {
  

  list_dat <- reactive({
    lsdat <- input$file2
    ifelse(is.null(lsdat), ifelse(input$season == 'Dry',lsd <- drylist, lsd <- alist) ,lsd <- read.csv(lsdat$datapath, col.names = c('subcell', 'listid')))
    lsd <- unique(lsd)
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
    
    selectInput('species1', label = h4('Select Species'), choices = c(levels(kl1$SPECIES.NAME),'All Species (for download)'))
  })
  
  output$box2 <- renderUI({
    
    kl1 <- kl %>% filter(SAMPLING.EVENT.IDENTIFIER %in% list_dat()$listid)
    
    kl1$SPECIES.NAME <- factor(kl1$SPECIES.NAME)
    
    selectInput('species2', label = h4('Select Species'), choices = c(levels(kl1$SPECIES.NAME),'All Species (for download)'))
  })
  
  
  output$downloadData <- downloadHandler(
    filename = 'shiny_atlas_dataset.csv',
    content = function(file) {
      write.csv(thisdata(), file)
    }
  )

  
  map_dat_sp <- function(spc){
    
    map_dat <- kl %>% filter(SAMPLING.EVENT.IDENTIFIER  %in% list_dat()$listid)
    
    map_dat <- left_join(map_dat,list_dat(), by = c('SAMPLING.EVENT.IDENTIFIER' = 'listid'))
    
    map_dat <- data.frame(table(map_dat$SPECIES.NAME,map_dat$subcell))
    
    colnames(map_dat) <- c('species.name','subcell','frequency')
    
    visits <- data.frame(table(list_dat()$subcell))
    
    colnames(visits) <- c('subcell','visits')
    
    map_dat <- left_join(map_dat,visits, by = c('subcell' = 'subcell'))
    
    map_dat$grid <- all$Cell[match(map_dat$subcell,all$Sub.Cell.Name)]
    
    map_dat <- map_dat[!is.na(map_dat$grid),]
    
    map_dat2 <- map_dat %>% mutate(perc = frequency/visits*100) 
    
    map_dat_sp1 <- map_dat2 %>% filter(species.name == spc)
    
    map_dat_sp1
    
  }
  
  grid_sp <- function(spc){
    if(is.null(list_dat()) == T) return(NULL)
    
    cells <- all %>% filter(Sub.Cell.Name %in% list_dat()$subcell)
    
    cells$Cell <- as.character(cells$Cell)
    
    allcells <- cells[,9:14]
    
    gridcells <- allcells %>% group_by(Cell) %>% slice(1)
    
    grids <- grid_polygon(gridcells[,2:6])
    
    map_dat_sp_grid <- map_dat_sp(spc) %>% group_by(grid) %>% summarise(percentage = round(mean(perc),2)) %>% ungroup()
    
    map_dat_sp_grid <- data.frame(map_dat_sp_grid)
    
    rownames(map_dat_sp_grid) <- map_dat_sp_grid$grid  
    
    map_dat_spdf <- SpatialPolygonsDataFrame(grids,map_dat_sp_grid, match.ID = T)
    
    map_dat_spdf
  }

  output$mymap <- renderLeaflet({
    
    if(is.null(list_dat()) == T) return(NULL)
      
    cells <- all %>% filter(Sub.Cell.Name %in% list_dat()$subcell)
    
    cells$Cell <- as.character(cells$Cell)
    
    allsubcells <- cells[,c(1:4,9)]
    
    subc <- grid_polygon(allsubcells)
    
    map_dat_sp_sub <- map_dat_sp(input$species1)
    
    rownames(map_dat_sp_sub) <- map_dat_sp_sub$subcell  
    
    map_dat_sp_subdf <- SpatialPolygonsDataFrame(subc,map_dat_sp_sub, match.ID = T)
    
    binpal <- colorNumeric("Reds",domain = c(0,100), na.color = 'white')
    
     
    grid_spt <- grid_sp(input$species1)
    
    leaflet() %>% 
      addTiles() %>% 
      addPolylines(data = grid_spt, color = 'grey',weight = 1, group = 'Grid Lines') %>%
      addPolygons(data = grid_spt, layerId = as.vector(grid_spt$grid),weight = 1, fillColor = ~binpal(percentage), fillOpacity = .7, stroke = F, color = 'gray', popup = paste(paste('Grid = <b>', grid_spt$grid,'</b>', sep = ''), paste('Percentage = <b>', grid_spt$percentage,'%</b>', sep = ''), sep = '<br/>'), group = 'Species Distribution') %>%
      addPolygons(data = map_dat_sp_subdf,layerId = as.vector(map_dat_sp_subdf$subcell), fillColor = ~binpal(perc),  fillOpacity = .7, weight = 1, color = 'black', group = 'Subcells',popup = paste(paste('Subcell = <b>',map_dat_sp_subdf$subcell,'</b>',sep = ''), paste('Percentage = <b>',map_dat_sp_subdf$perc,'%</b', sep = ''), sep = '<br/>')) %>%
      addLegend('topright',binpal,values = c(0,100), labFormat = labelFormat(suffix = '%')) %>%
      hideGroup('Subcells') %>%
      addLayersControl(overlayGroups = c('Grid Lines','Species Distribution','Subcells'), position = 'bottomleft', options = layersControlOptions(collapsed = FALSE))
    })
  
  
  plotInput <- function(spc){
      
    if(is.null(list_dat()) == T & is.null(input$district) == T) return(NULL)
      
    p <- ggplot() +
      theme_bw()+
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())+
      coord_map()
    
    dist <- k2[k2@data$DISTRICT %in% input$district,]
    
    dist_df <- fortify(dist)
    
    grid_spt <- grid_sp(spc)
    
    if(is.null(list_dat()) == F) {
    
      sp_grid <- fortify(grid_spt)
    
      sp_freq <- left_join(sp_grid, grid_spt@data, by = c('id'= 'grid'))
    
      sp_freq$percentage_discrete <- cut(sp_freq$percentage,c(0,1,20,40,60,80,100))
    
      p <- p + 
        ggtitle(spc) +
        geom_polygon(data = sp_freq, aes(x=long, y=lat, group=group, fill = percentage_discrete)) +
        geom_path(data = sp_freq, aes(x=long, y=lat, group=group), colour = 'black', size = 0.05) +
        scale_fill_brewer(palette = "YlGn", guide = F)

    }
      
    
    if(is.null(input$district) == F) {
      p <- p + geom_path(data = dist_df, aes(x=long, y=lat, group=group)) 
      
    } 
    
    p
  }
  
  output$print_map <- renderPlot({
    plotInput(input$species2)
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(input$district,'-', input$species2, '.svg', sep='') },
    content = function(file) {
      device <- function(...,width,height){
        grDevices::svg(..., width = width, height = height)
      }
      ggsave(file, plot = plotInput(input$species2),device = device)
      
    })
  
  output$downloadAll <- downloadHandler(
    filename = function() { paste(input$district,'.zip', sep='') },
    content = function(file1) {
      all <- c()
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      
      kl1 <- kl %>% filter(SAMPLING.EVENT.IDENTIFIER %in% list_dat()$listid)
    
      kl1 <- kl1[!kl1$SPECIES.NAME == "Eastern/Western Cattle Egret | Bubulcus ibis",]
      
      kl1 <- kl1[!kl1$SPECIES.NAME == "Indian/Eastern Jungle Crow (Large-billed Crow) | Corvus macrorhynchos",]
      
      kl1$SPECIES.NAME <- factor(kl1$SPECIES.NAME)
      
      
    
      for (i in levels(kl1$SPECIES.NAME)){
        print(i)
        path = paste(strsplit(i, split = ' | ', fixed = T)[[1]][1],'.svg',sep = '')
        all <- c(all,path)
        svg(paste(strsplit(i, split = ' | ', fixed = T)[[1]][1],'.svg',sep = ''))
        print(plotInput(i))
        dev.off()
      }
      zip(zipfile = file1, files = all)
    }, 
    contentType = 'application/zip'
  )


  output$grid_det <- renderDataTable({
    gridid <- input$mymap_shape_click
    
    if (is.null(gridid) == T) return(NULL)
    
    scls <- all %>% filter(Cell == gridid$id)
    
    gridls <- list_dat() %>% filter(subcell %in% scls$Sub.Cell.Name) 
    
    if (nrow(scls) == 0) gridls <- list_dat() %>% filter(subcell == gridid$id)
    
    pres <- kl %>% filter(SPECIES.NAME == input$species1 & SAMPLING.EVENT.IDENTIFIER %in% gridls$listid)
    
    gridls <- gridls %>% mutate(Present = listid %in% pres$SAMPLING.EVENT.IDENTIFIER) %>% arrange(desc(subcell))
    
    gridls$listid <- paste0('<a href=http://ebird.org/ebird/view/checklist/',gridls$listid," target=\"_blank\">", gridls$listid,"</a>")
    
    DT::datatable(gridls, options = list(paging = F,searching = F), escape = F)
  
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)
