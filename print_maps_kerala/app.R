library(shiny)
library(dplyr)
library(rgdal)
library(rgeos)
library(sp)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(gridExtra)
library(gridBase)
library(grid)
library(DT)
library(car)
library(mapproj)

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



allcells<- read.csv('Kerala Sub-cells and Cells.csv', stringsAsFactors = TRUE)

k1 <- readOGR('.','Distrits with eBird code')

k2 <- k1[k1@data$ST_NM == 'Kerala',]

k2@data$DISTRICT <-  factor(k2@data$DISTRICT)


cit <- data.frame(matrix(NA,nrow = 0, ncol = 2))
mert <- data.frame(matrix(NA,nrow = 0,ncol = 3))

ui <- fluidPage(
   
   titlePanel("Print Maps"),
   
   sidebarLayout(
      sidebarPanel(
        tags$head(tags$style(type="text/css", "
             #loadmessage {
                             position: fixed;
                             top: 0px;
                             left: 0px;
                             width: 100%;
                             padding: 5px 0px 5px 0px;
                             text-align: center;
                             font-weight: bold;
                             font-size: 100%;
                             color: #000000;
                             background-color: #6699cc;
                             z-index: 105;
                             }
                             ")),
        tags$style(type="text/css",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Loading...",id="loadmessage")),
        conditionalPanel(
          'input.page == "Data Upload"',
          uiOutput('distinp'),
          fileInput('ebdwld','Upload ebird download (Atlas group account)', accept = '.csv'),
          fileInput('liscel','Upload lists cell matching', accept = '.csv')
        ),
        
        conditionalPanel(
          'input.page == "species Maps" | input.page == "Seasonality Charts"| input.page == "Bar Plots"',
          uiOutput('spfilter')),
        
        conditionalPanel(
          'input.page == "species Maps"',
          radioButtons('maptyp','Select Map', choices = list('Species Distribution Map' = 1,'Richness Map' = 2)),
          checkboxInput('clipgr','Crop grid to district boundary'),
          fluidRow(
            column(6, numericInput('imwidth','Width (px)', value = 500, min = 500, width = 140)),
            column(6 ,numericInput('imheight','Height (px)', value = 500, min = 500, width = 140))),
          fluidRow(
            column(6,numericInput('imres','Resolution', value = 72, min = 72, width = 140)),
            column(6,numericInput('imzoom','Zoom', value = 0, min = -10, max = 10, width = 140))),
          sliderInput('imposupd','Move Down <----> Up',min = -30,max = 30, value = 0, step = .5, post = '%'),
          sliderInput('imposlr','Move Closer <----> Farther',min = -10,max = 10, value = 0, step = .1, post = '%'),
          sliderInput('legheight','Legend Height',min = -100,max = 100, value = 0, step = 5, post = '%'),
          sliderInput('legwidth','Legend Width',min = 20,max = 100, value = 100, step = 1, post = '%'),
          textInput('citll','Add long,lat of city', value = '', width = 140),
          actionButton("addc", "Add Marker"),
          textInput('colsc','Enter Colour scale', value = 'RdPu', width = 140),
          helpText(a('Check here for color schemes', href = 'http://moderndata.plot.ly/create-colorful-graphs-in-r-with-rcolorbrewer-and-plotly/', target = '_blank')),
          sliderInput('colhue','Colour darkness',min = 0, max = 4, step = 1, value = 0),
          downloadButton('dl_spmap','Download Map'),downloadButton('downloadAll','Download All Species Maps')),
        
        conditionalPanel(
          'input.page == "Sub-cell Replacement"',
          fileInput('screpfile','Upload Subcell Replacement File', accept = '.csv'),
          uiOutput('misssubc'),
          uiOutput('repsubc'),
          actionButton('repscbt','Replace')
        ),
        

        conditionalPanel(
          'input.page == "Bird Select"',
          fileInput('spmergefile','Upload Merge File',accept = '.csv'),
          helpText('Or Select species manually'),
          uiOutput('speciesdrop'),
          uiOutput('speciesmer1'),
          uiOutput('speciesmer2'),
          uiOutput('speciesmerf'),
          uiOutput('mergebutton')
        ),
        
        conditionalPanel(
          'input.page == "Seasonality Charts"',
          fileInput('seasdat','Enter eBird download for the region: zip or txt file'),
          dateRangeInput('seasdatfil','Use lists between dates', start = '2015-01-01'),
          radioButtons('durtyp','Filter Duration by', choices = list('Minimum Duration' = 1, 'Duration Range' = 2), selected = 1),
          uiOutput('durfil'),
          fluidRow(
            column(6, numericInput('sesimwidth','Width (px)', value = 1000, min = 500, width = 140)),
            column(6 ,numericInput('sesimheight','Height (px)', value = 150, min = 5, width = 140))),
          fluidRow(
            column(6,numericInput('sesimres','Resolution', value = 72, min = 72, width = 140)),
            column(6,sliderInput('sesimspac','Spacing', value = 0,min = -1.5,max = 1.5, step = .02))),
          downloadButton('dl_seaschar','Download Chart'),helpText('The month labels are for reference and will not be included in the download'),downloadButton('downloadAllseaschar','Download All')

        ),
        
        conditionalPanel(
          'input.page == "Bar Plots"',
          fluidRow(
            column(6, numericInput('barimwidth','Width (px)', value = 400, min = 50, width = 140)),
            column(6 ,numericInput('barimheight','Height (px)', value = 200, min = 5, width = 140))),
          fluidRow(
            column(6,numericInput('barimres','Resolution', value = 72, min = 72, width = 140)),
            column(6,sliderInput('barfontsize','Font Size', value = 3,min = 0,max = 6, step = .05))),
          sliderInput('textlr','Move text L <---> R', value = 40, min = 0, max = 100, step = 1),
          sliderInput('textud','Move text Down <---> Up', value = 1.4,min = 1,max = 2, step = .01),
          textInput('barpcol','Colour',value = 'purple',width = 140),
          downloadButton('dl_wetbar','Download Wet Barplot'),
          downloadButton('dl_drybar','Download Dry Barplot'),
          downloadButton('dl_allbar','Download All Barplots')

        ),
        width = 2
      ),
      
      mainPanel(
        tabsetPanel(
          id = 'page',
          tabPanel('Data Upload', textOutput('hello'), plotOutput('distplot')),
          tabPanel('Bird Select',h3(textOutput('speciesn')),dataTableOutput('specieslist'),dataTableOutput('mergetab')),
          tabPanel('Sub-cell Replacement', dataTableOutput('subcells'), dataTableOutput('subrepdtbl')),
          tabPanel('species Maps', imageOutput("map")),
          tabPanel('Seasonality Charts', dataTableOutput('seastab'), br(),br(),imageOutput('seasimdisp')),
          tabPanel('Bar Plots', fluidRow(column(6,imageOutput('wetbarimdisp')),column(6,imageOutput('drybarimdisp'))))
        )
      )
   )
)

server <- function(input, output) {
  options(shiny.maxRequestSize=100*1024^2)   
  
  kan_map <- function(){
    kan_map <- k2[k2@data$DISTRICT == input$distsel,]
    kan_map
  }
  
  dat1 <- reactive({
    dat <- read.csv(input$ebdwld$datapath)
    dat
  })
  
  klists <- reactive({
    klists <- read.csv(input$liscel$datapath)
    klists
  })
  
  mtab <- reactive({
      mtab <- read.csv(input$spmergefile$datapath)
      mtab
  })
  
  screp <- reactive({
    if(is.null(input$screpfile)) return(NULL)
    screp <- read.csv(input$screpfile$datapath)
    screp
  })
  
  
  
  splistt <- reactive({
    splist <- dat1() %>% group_by(Common.Name,Scientific.Name) %>% slice(1) %>% arrange(Taxonomic.Order) %>% select(Common.Name,Scientific.Name)  
    splist$Atlas.Name <- splist$Common.Name 
    splist$Atlas.Name <- as.character(splist$Atlas.Name)
    if(is.null(input$spmergefile) == F) {
      splist$Atlas.Name <- mtab()$Assigned.Name.for.Atlas[match(splist$Common.Name,mtab()$Common.Name)]
      splist$Atlas.Name[splist$Atlas.Name == ''] <- 'Removed'
    }
    splist <- splist[!splist$Atlas.Name == '',]
    splist$Atlas.Name[splist$Atlas.Name %in% input$speciesdr] <- 'Removed'
    splist$Atlas.Name[splist$Common.Name %in% mergedat()$Species1] <- mergedat()$Merged
    splist$Atlas.Name[splist$Common.Name %in% mergedat()$Species2] <- mergedat()$Merged
    splist$Atlas.Name <- gsub('/','-',splist$Atlas.Name)
    return(splist)
  })
  
  output$hello <- renderText({
    if(is.null(input$ebdwld)) return(NULL)
    if(is.null(input$liscel)) return(NULL)
    validate(need(splistt(),'Bird data format error, Please check uploaded file again.'))
    validate(need(outbb(),'List cell matching error, Please check uploaded file again.'))
    'Great, All the files seem to be in the right format and this is the region covered :'
  })
  
  output$specieslist <- renderDataTable({
    validate(need(input$ebdwld,'Please Upload Bird Data'))
    DT::datatable(splistt(), selection = 'single')
  })
  
  output$speciesn <-  renderText({
    paste0('Number of Species = ',length(unique(splistt()$Atlas.Name[!splistt()$Atlas.Name == 'Removed'])))
  })
  
  output$speciesdrop <- renderUI({
    validate(need(input$ebdwld,'Please Upload Bird Data'))
    selectInput('speciesdr','Select species to be dropped', choices = unique(dat1()$Common.Name), multiple = T)
  })
  
  output$speciesmer1 <- renderUI({
    validate(need(input$ebdwld,''))
    selectInput('mer1','Merge Species', choices = unique(dat1()$Common.Name), multiple = F)
  })
  
  output$speciesmer2 <- renderUI({
    validate(need(input$ebdwld,''))
    selectInput('mer2','With', choices = unique(dat1()$Common.Name), multiple = F)
  })
  
  output$speciesmerf <- renderUI({
    validate(need(input$ebdwld,''))
    selectInput('merf','To Species', choices = c(input$mer1,input$mer2, paste(input$mer1,input$mer2,sep = '-')), multiple = F)
  })
  
  output$mergebutton <- renderUI({
    actionButton('mergesp','Merge')
  })
  
  v1 <- reactiveValues(dat = 0)
  
  observeEvent(input$mergesp,{
    v1$dat <- 1
  })
  
  mergedat <- reactive({
    if(input$mergesp){
      if(v1$dat == 1){
        mert <- rbind(mert,c(input$mer1,input$mer2,input$merf))
        colnames(mert) <- c('Species1','Species2','Merged')
        mert <- mert  %>% arrange(desc(row_number())) %>% group_by(Species1,Species2) %>% slice(1)
        mert <<- data.frame(mert,stringsAsFactors = T)
        v1$dat <- 0
      }
    }
    mert
  })
  

  output$mergetab <- renderDataTable({
    validate(need(input$ebdwld,''))
    validate(need(input$mergesp,''))
    DT::datatable(mergedat())
  })
  
  subcl <- reactive({
    klists1 <- klists()
    klists1$Sub.cell <- paste0('[',klists1$Sub.cell,']')
    klists1$Sub.cell <- as.character(klists1$Sub.cell)
    if(is.null(input$screpfile) == F){
      for (i in 1:nrow(screp())) klists1$Sub.cell <- gsub(screp()$Sampled[i],screp()$Replaced[i],klists1$Sub.cell)
    }
    klists1$Sub.cell[klists1$Sub.cell %in% subrepdat$df$Sampled[-1]] <- subrepdat$df$Replaced[-1]
    kan_cells <- left_join(klists1,allcells, by = c("Sub.cell" = 'Sub.Cell.Name'))
    sctable <- kan_cells 
    sctable
  })
  
  output$subcells <- renderDataTable({
    validate(need(input$liscel,'Please Upload List to Subcell Matching'))
    subcl <- subcl() %>% select(Sub.cell,Season,List.ID,Cell)
    subcl
  })
  
  output$distinp <- renderUI({
    if(is.null(input$liscel) == F){
      distc <- names(sort(table(subcl()$district),decreasing = T))[1]}
    selectInput('distsel','Select District', choices = k2@data$DISTRICT, selected = ifelse(is.null(input$liscel) == F, distc,k2@data$DISTRICT[1]) )
  })
  
    
  output$misssubc <- renderUI({
    validate(need(input$liscel,''))
    selectInput('scdat','Subcell with no Cell (Sampled)', choices = ifelse(length(unique(subcl()$Sub.cell[is.na(subcl()$Cell)])) > 0, unique(subcl()$Sub.cell[is.na(subcl()$Cell)]), 'None'))
  })
  
  output$repsubc <- renderUI({
    validate(need(input$liscel,''))
    selectInput('repsc','Replace with (Replaced)',choices = allcells$Sub.Cell.Name)
  })
  
  subrepdat <- reactiveValues()
  
  subrepdat$df <- data.frame(Sampled = NA, Replaced = NA)
  
  v <- reactiveValues(dat = 0)
  
  observeEvent(input$repscbt,{
    v$dat <- 1
  })
  
  newEntry <- observe({
    if(input$repscbt > 0) {
      if(v$dat == 1){
        sc <- isolate(c(input$scdat, input$repsc))
        isolate(subrepdat$df <- rbind(subrepdat$df, sc))
        v$dat <- 0
      }
    }
  })
  
  output$subrepdtbl <- renderDataTable({
    validate(need(input$liscel,''))
    srd <- data.frame(subrepdat$df[-1,])
    if(is.null(input$screpfile) == F){ srd <- rbind(srd,screp())}
    srd <- srd[!duplicated(srd),]
    srd
    })
  
  output$spfilter <- renderUI({
    selectInput('spec','Select Species',choices = unique(splistt()$Atlas.Name))
  })
  
  
  kba_f <- reactive({
    dat1 <- dat1()
    dat1$Common.Name <- splistt()$Atlas.Name[match(dat1$Common.Name,splistt()$Common.Name)]
    dat1 <- dat1[!dat1$Common.Name == 'Removed',]
    dat1$Common.Name <- gsub('/','-',dat1$Common.Name)
    dat1$Common.Name <- factor(dat1$Common.Name)
    dlists <- subcl() %>% filter(Season == 'Dry')
    datd <- left_join(dat1, dlists, by = c("Submission.ID" = "List.ID") )
    datd <- datd[!duplicated(datd),]
    datd <- data.frame(datd)
    kba_d <- as.data.frame(table(datd$Common.Name,datd$Sub.cell))         #count of how many times a species was seen in each subcell
    colnames(kba_d) <- c('Species','Subcell','Frequency.dry')
    kba_d$Grid <- dlists$Cell[match(kba_d$Subcell,dlists$Sub.cell)]         #Assigning the grids to the subcell 
    dlists$Cell <- factor(dlists$Cell)
    gr <- table(dlists$Cell)
    kba_d$visits <- gr[match(kba_d$Grid,names(gr))]
    kba_d$percentage.dry <- kba_d$Frequency.dry/kba_d$visits
    kba_d <- with(kba_d, aggregate(percentage.dry~Species + Grid, FUN = sum))
    
    
    dlists <- subcl() %>% filter(Season == 'Wet')
    datw <- left_join(dat1, dlists, by = c("Submission.ID" = "List.ID") )
    datw <- datw[!duplicated(datw),]
    datw <- data.frame(datw)
    kba_w <- as.data.frame(table(datw$Common.Name,datw$Sub.cell))         #count of how many times a species was seen in each subcell
    colnames(kba_w) <- c('Species','Subcell','Frequency.wet')
    kba_w$Grid <- dlists$Cell[match(kba_w$Subcell,dlists$Sub.cell)]         #Assigning the grids to the subcell 
    dlists$Cell <- factor(dlists$Cell)
    gr <- table(dlists$Cell)
    kba_w$visits <- gr[match(kba_w$Grid,names(gr))]
    kba_w$percentage.wet <- kba_w$Frequency.wet/kba_w$visits
    kba_w <- with(kba_w, aggregate(percentage.wet~Species + Grid, FUN = sum))

    kba_f <- left_join(kba_d,kba_w)
    kba_f$percentage.dry <- kba_f$percentage.dry*100
    kba_f$percentage.wet <- kba_f$percentage.wet*100
    kba_f
  })
  

  kan_celldf <- reactive({
    kan_cells1 <- subcl() %>% group_by(Cell) %>% slice(1)
    kan_cells1 <- data.frame(kan_cells1)
    kan_cells1 <- kan_cells1[!is.na(kan_cells1$Longitude),]
    kan_cells1 <- kan_cells1 %>% select(Longitude.2,Longitude.3,Latitude.2,Latitude.3,Cell)
  })
    
  kan_grdf <- reactive({
    kan_gr <- grid_polygon(kan_celldf())
    if(input$clipgr == TRUE) kan_gr <- gIntersection(kan_gr,kan_map(), byid = T, drop_lower_td = F)
    kan_grdf <- fortify(kan_gr)
    kan_grdf$id <-  unlist(lapply(strsplit(kan_grdf$id," "), function(x) x[1]))
    kan_grdf
  })
  
  freqgrid <- function(species = input$spec){
    kba_s <- kba_f() %>% filter(Species == species)
    kba_s$percentage.dry <- cut(kba_s$percentage.dry,c(0,1,20,40,60,80,100))
    kba_s$percentage.wet <- cut(kba_s$percentage.wet,c(0,1,20,40,60,80,100))
    freqgrid <- left_join(kan_grdf(), kba_s, by = c('id' = "Grid"))    
    freqgrid
  }
  
  
  outbb <- reactive({
    maxlng <- max(c(kan_celldf()$Longitude.2,kan_celldf()$Longitude.3)) + 0.03 - (input$imzoom/100)
    minlng <- min(c(kan_celldf()$Longitude.2,kan_celldf()$Longitude.3)) -  0.03 + (input$imzoom/100)
    maxlat <- max(c(kan_celldf()$Latitude.2,kan_celldf()$Latitude.3)) +  0.03 - (input$imzoom/100)
    minlat <- min(c(kan_celldf()$Latitude.2,kan_celldf()$Latitude.3)) -  0.03 + (input$imzoom/100)
    outbb <- cbind(c(minlng,maxlng,maxlng,minlng, minlng), c(maxlat,maxlat,minlat,minlat,maxlat))
    outbb <- SpatialPolygons(list(Polygons(list(Polygon(outbb)),1)))
    proj4string(outbb) <- proj4string(k2)
    outbb
  })
  
  kan_clipdf <- reactive({
    kan_clip <- gIntersection(k2,outbb(),byid = T)
    kan_clipdf <- fortify(kan_clip)
    kan_clipdf
  })
  
  land_clipdf <- reactive({
    land_clip <- gIntersection(k1,outbb(),byid = T)
    land_clipdf <- fortify(land_clip)
    land_clipdf
  })
  
  outbbdf <- reactive({
    outbbdf <- fortify(outbb())
    outbbdf
  })
  
  cities <- reactive({
    if (input$addc){
      cit <- rbind(cit,c(as.numeric(strsplit(input$citll,',')[[1]][1]),as.numeric(strsplit(input$citll,',')[[1]][2])))
      colnames(cit) <- c('long','lat')
      cit <<- cit
    }
    cit
  })
  
  output$distplot <- renderPlot({
    validate(
      need(input$ebdwld,'Please Upload Bird Data'),
      need(input$liscel,'Please Upload List to Subcell Matching')
      )
    
    maxlngc <- max(c(kan_celldf()$Longitude.2,kan_celldf()$Longitude.3)) + 0.02 - (input$imzoom/100)
    minlngc <- min(c(kan_celldf()$Longitude.2,kan_celldf()$Longitude.3)) -  0.02 + (input$imzoom/100)
    maxlatc <- max(c(kan_celldf()$Latitude.2,kan_celldf()$Latitude.3)) +  0.02 - (input$imzoom/100)
    minlatc <- min(c(kan_celldf()$Latitude.2,kan_celldf()$Latitude.3)) -  0.02 + (input$imzoom/100)
    
    par(xpd = FALSE)
    
    dist <- ggplot() +
      geom_polygon(data = outbbdf(), aes(x=long, y=lat, group=group), fill = 'lightblue', col = 'white')+
      geom_polygon(data = land_clipdf(), aes(x=long, y=lat, group=group), fill = 'white', col = 'white') +
      geom_path(data = kan_clipdf(), aes(x=long, y=lat, group=group)) +
      geom_path(data = outbbdf(), aes(x=long, y=lat, group=group), size = 2) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      theme_bw()+
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin=unit(c(0,0,0,0), "cm"),
            panel.border = element_blank(),
            panel.background = element_blank())+
      coord_map(xlim = c(minlngc,maxlngc),ylim = c(minlatc,maxlatc))
    
    
    
    dist
  })  
   
   spmap <- function(spfreq = freqgrid()){
     maxlngc <- max(c(kan_celldf()$Longitude.2,kan_celldf()$Longitude.3)) + 0.02 - (input$imzoom/100)
     minlngc <- min(c(kan_celldf()$Longitude.2,kan_celldf()$Longitude.3)) -  0.02 + (input$imzoom/100)
     maxlatc <- max(c(kan_celldf()$Latitude.2,kan_celldf()$Latitude.3)) +  0.02 - (input$imzoom/100)
     minlatc <- min(c(kan_celldf()$Latitude.2,kan_celldf()$Latitude.3)) -  0.02 + (input$imzoom/100)

     mycol1 <- brewer.pal((5 + input$colhue), as.character(input$colsc))[(1+input$colhue):(5+input$colhue)]
     mycols <- c("#FFFFFF", mycol1)
     
     par(xpd = FALSE)
     
     k.dry <- ggplot() +
       geom_polygon(data = outbbdf(), aes(x=long, y=lat, group=group), fill = 'lightblue', col = 'white')+
       geom_polygon(data = land_clipdf(), aes(x=long, y=lat, group=group), fill = 'white', col = 'white') +
       geom_polygon(data = spfreq, aes(x=long, y=lat, group=group, fill = percentage.dry)) +
       geom_path(data = kan_clipdf(), aes(x=long, y=lat, group=group)) +
       geom_path(data = outbbdf(), aes(x=long, y=lat, group=group), size = 2) +
       scale_fill_manual(values = mycol1, guide = F) +
       scale_x_continuous(expand = c(0,0)) +
       scale_y_continuous(expand = c(0,0)) +
       theme_bw()+
       theme(axis.line=element_blank(),
             axis.text.x=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks=element_blank(),
             axis.title.x=element_blank(),
             axis.title.y=element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             plot.margin=unit(c(0,0,0,0), "cm"),
             panel.border = element_blank(),
             panel.background = element_blank())+
       geom_point(aes(x = cities()$long,y = cities()$lat),size = 4, shape = 15, show.legend = F)+
       coord_map(xlim = c(minlngc,maxlngc),ylim = c(minlatc,maxlatc))     
  

    k.wet <- ggplot() +
      geom_polygon(data = outbbdf(), aes(x=long, y=lat, group=group), fill = 'lightblue', col = 'white')+
      geom_polygon(data = land_clipdf(), aes(x=long, y=lat, group=group), fill = 'white', col = 'white') +
      geom_polygon(data = spfreq, aes(x=long, y=lat, group=group, fill = percentage.wet)) +
      geom_path(data = kan_clipdf(), aes(x=long, y=lat, group=group)) +
      geom_path(data = outbbdf(), aes(x=long, y=lat, group=group), size = 2) +
      scale_fill_manual(values = mycol1, guide = F) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      theme_bw()+
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin=unit(c(0,0,0,0), "cm"),
            panel.border = element_blank(),
            panel.background = element_blank())+
      geom_point(aes(x = cities()$long,y = cities()$lat),size = 4, shape = 15, show.legend = F) +
      coord_map(xlim = c(minlngc,maxlngc),ylim = c(minlatc,maxlatc))
    
     
     rleg <- cbind(c(1,seq(20,80,20)),0,seq(20,100,20),0.5 + (input$legheight/200))
     rleg <- rbind(c(0,0,1,0.5 + (input$legheight/200)),rleg)
     
     plot.new()
     grid.newpage()

     vp1 <- viewport(x=0 - (input$imposlr/100), y = 1 + (input$imposupd/100), height = 0.9, width = 0.5, just = c('left','top'))
     vp2 <- viewport(x=0.5 + (input$imposlr/100), y = 1 + (input$imposupd/100), height = 0.9, width = 0.5,just = c('left','top'))
     vp3 <- viewport(x = (1 - (input$legwidth/100))/2, y = 0, height = 0.1, width = (input$legwidth/100), just =  c('left','bottom'))
     
     
     pushViewport(vp1)
     par(new = TRUE, oma = c(0,0,0,0), xpd = T, mar = c(2,0,0,0))
     print(k.wet, newpage = FALSE)
     upViewport()
     pushViewport(vp2)
     print(k.dry, newpage = FALSE)
     upViewport()
     pushViewport(vp3)
     par(new = TRUE,fig = gridFIG(), xpd = NA)
     plot(x = NULL, y = NULL, xlim = c(0,100),ylim = c(0,1), yaxt = 'na',xaxt = 'na',xlab = NA, ylab = NA, bty = 'n')
     axis(1, at = seq(0,100,20), labels = NA)
     axis(1, at = seq(0,100,20), labels = seq(0,100,20), line = -0.5, tick = NA, lwd = 0)
     rect(rleg[,1],rleg[,2],rleg[,3],rleg[,4], col = mycols, bty = 'n')
     upViewport()

   }
   
   
   richmap <- function(){
     dat1 <- dat1()
     ck <- input$maptyp
     dat1$Common.Name <- splistt()$Atlas.Name[match(dat1$Common.Name,splistt()$Common.Name)]
     dat1 <- dat1[!dat1$Common.Name == 'Removed',]
     dat1$Common.Name <- gsub('/','-',dat1$Common.Name)
     dat1$Common.Name <- factor(dat1$Common.Name)
     dlists <- subcl() %>% filter(Season == 'Dry')
     datd <- left_join(dat1, dlists, by = c("Submission.ID" = "List.ID") )
     datd <- datd[!duplicated(datd),]
     datd <- data.frame(datd) 
     datd <- datd %>% group_by(Cell) %>% summarise(rich.dry = n_distinct(Common.Name))
     
     dlists <- subcl() %>% filter(Season == 'Wet')
     datw <- left_join(dat1, dlists, by = c("Submission.ID" = "List.ID") )
     datw <- datw[!duplicated(datw),]
     datw <- data.frame(datw)
     datw <- datw %>% group_by(Cell) %>% summarise(rich.wet = n_distinct(Common.Name))
     richdat <- left_join(datd,datw) %>% filter(!is.na(Cell))
     mx <- (max(c(richdat$rich.dry,richdat$rich.wet))%/%20+1)*20
     richdat$rich.dry <- cut(richdat$rich.dry, seq(0,mx,by = 20))
     richdat$rich.wet <- cut(richdat$rich.wet, seq(0,mx,by = 20))

     richgrid <- left_join(kan_grdf(), richdat, by = c('id' = "Cell"))    
     
     
     maxlngc <- max(c(kan_celldf()$Longitude.2,kan_celldf()$Longitude.3)) + 0.02 - (input$imzoom/100)
     minlngc <- min(c(kan_celldf()$Longitude.2,kan_celldf()$Longitude.3)) -  0.02 + (input$imzoom/100)
     maxlatc <- max(c(kan_celldf()$Latitude.2,kan_celldf()$Latitude.3)) +  0.02 - (input$imzoom/100)
     minlatc <- min(c(kan_celldf()$Latitude.2,kan_celldf()$Latitude.3)) -  0.02 + (input$imzoom/100)
     

     par(xpd = FALSE)
     
     k.dry <- ggplot() +
       geom_polygon(data = outbbdf(), aes(x=long, y=lat, group=group), fill = 'lightblue', col = 'white')+
       geom_polygon(data = land_clipdf(), aes(x=long, y=lat, group=group), fill = 'white', col = 'white') +
       geom_polygon(data = richgrid, aes(x=long, y=lat, group=group, fill = rich.dry)) +
       geom_path(data = kan_clipdf(), aes(x=long, y=lat, group=group)) +
       geom_path(data = outbbdf(), aes(x=long, y=lat, group=group), size = 2) +
       scale_fill_manual(values = mycol1, guide = F) +
       scale_x_continuous(expand = c(0,0)) +
       scale_y_continuous(expand = c(0,0)) +
       theme_bw()+
       theme(axis.line=element_blank(),
             axis.text.x=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks=element_blank(),
             axis.title.x=element_blank(),
             axis.title.y=element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             plot.margin=unit(c(0,0,0,0), "cm"),
             panel.border = element_blank(),
             panel.background = element_blank())+
       geom_point(aes(x = cities()$long,y = cities()$lat),size = 4, shape = 15, show.legend = F)+
       coord_map(xlim = c(minlngc,maxlngc),ylim = c(minlatc,maxlatc))     
     
     
     k.wet <- ggplot() +
       geom_polygon(data = outbbdf(), aes(x=long, y=lat, group=group), fill = 'lightblue', col = 'white')+
       geom_polygon(data = land_clipdf(), aes(x=long, y=lat, group=group), fill = 'white', col = 'white') +
       geom_polygon(data = richgrid, aes(x=long, y=lat, group=group, fill = rich.wet)) +
       geom_path(data = kan_clipdf(), aes(x=long, y=lat, group=group)) +
       geom_path(data = outbbdf(), aes(x=long, y=lat, group=group), size = 2) +
       scale_fill_manual(values = mycol1, guide = F) +
       scale_x_continuous(expand = c(0,0)) +
       scale_y_continuous(expand = c(0,0)) +
       theme_bw()+
       theme(axis.line=element_blank(),
             axis.text.x=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks=element_blank(),
             axis.title.x=element_blank(),
             axis.title.y=element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             plot.margin=unit(c(0,0,0,0), "cm"),
             panel.border = element_blank(),
             panel.background = element_blank())+
       geom_point(aes(x = cities()$long,y = cities()$lat),size = 4, shape = 15, show.legend = F) +
       coord_map(xlim = c(minlngc,maxlngc),ylim = c(minlatc,maxlatc))
     
     
     rleg <- cbind(c(1,seq(20,(mx-20),20)),0,seq(20,mx,20),0.5 + (input$legheight/200))
     rleg <- rbind(c(0,0,1,0.5 + (input$legheight/200)),rleg)
     
     plot.new()
     grid.newpage()
     
     vp1 <- viewport(x=0 - (input$imposlr/100), y = 1 + (input$imposupd/100), height = 0.9, width = 0.5, just = c('left','top'))
     vp2 <- viewport(x=0.5 + (input$imposlr/100), y = 1 + (input$imposupd/100), height = 0.9, width = 0.5,just = c('left','top'))
     vp3 <- viewport(x = (1 - (input$legwidth/100))/2, y = 0, height = 0.1, width = (input$legwidth/100), just =  c('left','bottom'))

     pushViewport(vp1)
     par(new = TRUE, oma = c(0,0,0,0), xpd = T, mar = c(2,0,0,0))
     print(k.wet, newpage = FALSE)
     upViewport()
     pushViewport(vp2)
     print(k.dry, newpage = FALSE)
     upViewport()
     pushViewport(vp3)
     par(new = TRUE,fig = gridFIG(), xpd = NA)
     plot(x = NULL, y = NULL, xlim = c(0,mx),ylim = c(0,1), yaxt = 'na',xaxt = 'na',xlab = NA, ylab = NA, bty = 'n')
     axis(1, at = seq(0,mx,20), labels = NA)
     axis(1, at = seq(0,mx,20), labels = seq(0,mx,20), line = -0.5, tick = NA, lwd = 0)
     rect(rleg[,1],rleg[,2],rleg[,3],rleg[,4], col = mycols, bty = 'n')
     upViewport()
     
   }

  output$map <- renderImage({
    outfile <- tempfile(fileext='.png')
    png(outfile, width = input$imwidth, height = input$imheight,units = 'px', res = input$imres)
    ifelse(input$maptyp == 1,spmap(),richmap())
    dev.off()
    
    
    list(src = outfile,
          width = paste0(input$imwidht,'px'), height = paste0(input$imheight,'px'),
          alt = "This is alternate text")
   }, deleteFile = TRUE)
  
  output$dl_spmap <- downloadHandler(
    filename = function() {     ifelse(input$maptyp == 1,paste(input$spec, '.tiff', sep=''),'richness.tiff') },
    content = function(file) {
      tiff(file, width = input$imwidth, height = input$imheight,units = 'px', res = input$imres)
      ifelse(input$maptyp == 1,print(spmap()),print(richmap()))
      dev.off()
  })
  
  output$downloadAll <- downloadHandler(
    filename = function() { paste('All_SpeciesMaps','.zip', sep='') },
    content = function(file1) {
      all <- c()
      tmpdir <- tempdir()
      setwd(tempdir())
      withProgress(message = 'Making plot', value = 0, {
        n <- length(unique(kba_f()$Species))
      for (i in unique(kba_f()$Species)){
        path = paste0(i,'.tiff')
        all <- c(all,path)
        tiff(paste0(i,'.tiff'), width = input$imwidth, height = input$imheight,units = 'px', res = input$imres)
        print(spmap(spfreq = freqgrid(species = i)))
        dev.off()
        incProgress(1/n, detail = paste("Species : ", i))
      }
      })            
      zip(zipfile = file1, files = all)
    }, 
    contentType = 'application/zip'
  )
  
  
  
  
  output$durfil <- renderUI({
    ifelse(input$durtyp == 1, dur <- 10, dur <- c(10,20))
    sliderInput('seasdur','Use lists of duration (min)',value = dur, min = 0, max = 600)
  })
  
  seasond <- reactive({
    if(tools::file_ext(input$seasdat)[1] == 'zip') seasond <- read.delim(unz(input$seasdat$datapath,gsub('zip','txt',input$seasdat$name)), na.strings = c("NA", "", "null"), as.is=TRUE, quote="")
    if(tools::file_ext(input$seasdat)[1] == 'txt') seasond <- read.delim(input$seasdat$datapath, na.strings = c("NA", "", "null"), as.is=TRUE, quote="")
    seasond$OBSERVATION.DATE <- as.Date(seasond$OBSERVATION.DATE) 
    seasond$year.month <- format(seasond$OBSERVATION.DATE, '%Y-%m')
    seasond$day <-  as.numeric(format(seasond$OBSERVATION.DATE,'%d'))
    seasond$week <- (seasond$day-1) %/% 7 
    seasond$week[seasond$week == 4] <- 3
    seasond$weekd <- format(as.Date(paste(seasond$year.month,((seasond$week*7) + 1),sep = '-')), '%m-%d')
    seasond
  })  
  
  sesd <- reactive({
    sesd <- seasond() %>% filter(!APPROVED == 0, !PROTOCOL.TYPE == "eBird - Casual Observation",ALL.SPECIES.REPORTED == 1,DURATION.MINUTES > min(input$seasdur), OBSERVATION.DATE >= as.Date(min(input$seasdatfil), origin = '1970-01-01'),OBSERVATION.DATE <= as.Date(max(input$seasdatfil), origin = '1970-01-01'), SUBNATIONAL2_CODE ==  as.character(k2@data$district10[k2@data$DISTRICT == input$distsel][1]))
    sesd$COMMON.NAME <- splistt()$Atlas.Name[match(sesd$COMMON.NAME,splistt()$Common.Name)]
    sesd <- sesd[!sesd$COMMON.NAME == 'Removed',]
    sesd
  })
  
  seasdat <- reactive({
    t <- data.frame(month.day =  as.character(c("01-01", "01-08", "01-15", "01-22", "02-01" ,"02-08", "02-15", "02-22", "03-01" ,"03-08", "03-15", "03-22" ,"04-01", "04-08" ,"04-15" ,"04-22" ,"05-01", "05-08","05-15", "05-22", "06-01", "06-08", "06-15", "06-22", "07-01", "07-08", "07-15" ,"07-22", "08-01", "08-08", "08-15", "08-22", "09-01", "09-08", "09-15", "09-22","10-01", "10-08", "10-15", "10-22", "11-01", "11-08", "11-15", "11-22", "12-01", "12-08", "12-15", "12-22")))
    week_list <- tapply(sesd()$SAMPLING.EVENT.IDENTIFIER, sesd()$weekd, function(x, na.rm = T) length(unique(x)))
    t$nlists <- as.numeric(week_list[match(t$month.day,names(week_list))])
    t$week <- rep(c(1:4),12)
    t$month <- as.character(rep(month.abb, each = 4))
    t$month <- factor(t$month, levels = month.abb)
    t
  })
  

  output$seastab <- renderDataTable({
    validate(need(input$seasdat,'Please Upload Bird Data'))
    seasdat()
  })
  
  seasplot <- function(species){
    t <- seasdat()
    dat_sp <- subset(sesd(), COMMON.NAME == species)
    sp_list <- tapply(dat_sp$SAMPLING.EVENT.IDENTIFIER, dat_sp$weekd, function(x, na.rm = T) length(unique(x)))
    t$sp <- sp_list[match(t$month.day,names(sp_list))]
    t$sp[is.na(t$sp) == T] <- 0
    t$sp <- t$sp/t$nlist*100
    t$sp <- as.numeric(as.character(factor(t$sp)))
    t$sp_discrete <- cut(t$sp,c(0,1,20,40,60,80,100))
    
    mycol1 <- brewer.pal((5 + input$colhue), as.character(input$colsc))[(1+input$colhue):(5+input$colhue)]
    mycols <- c("#FFFFFF", mycol1)

    p <- ggplot(t) +
      geom_tile(aes(x=week,y = 1, fill = sp_discrete ), stat = 'identity', colour = 'white') + 
      scale_fill_manual(values = mycol1, guide = F) +
      facet_grid(. ~ month) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
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
            panel.background = element_blank(),
            plot.margin=unit(c(0,0,0,0)-.1, "cm"),
            panel.spacing  = unit(0.4 + input$sesimspac, "cm"))
    
    p
    
  }
  
  output$seasimdisp <- renderImage({
    validate(need(input$seasdat,'Please Upload Bird Data'))
    outfile1 <- tempfile(fileext='.png')
    png(outfile1, width = input$sesimwidth, height = input$sesimheight,units = 'px', res = input$sesimres)
    print(seasplot(input$spec))
    dev.off()
    
    list(src = outfile1,
         width = paste0(input$sesimwidht,'px'), height = paste0(input$sesimheight,'px'),
         alt = "This is alternate text")
  }, deleteFile = TRUE)
    
  output$dl_seaschar <- downloadHandler(
    filename = function() { paste(input$spec, '.tiff', sep='') },
    content = function(file2) {
      tiff(file2, width = input$sesimwidth, height = input$sesimheight,units = 'px', res = input$sesimres)
      print(seasplot(input$spec) + theme(strip.text.x = element_blank()))
      dev.off()
    })
  
  output$downloadAllseaschar <- downloadHandler(
    filename = function() { paste('All_seasonalityCharts','.zip', sep='') },
    content = function(file3) {
      all1 <- c()
      tmpdir <- tempdir()
      setwd(tempdir())
      withProgress(message = 'Making plot', value = 0, {
        n <- length(unique(splistt()$Atlas.Name))
        for (i in unique(splistt()$Atlas.Name)){
          path = paste0(i,'.tiff')
          all1 <- c(all1,path)
          tiff(paste0(i,'.tiff'), width = input$sesimwidth, height = input$sesimheight,units = 'px', res = input$sesimres)
          print(seasplot(i)+ theme(strip.text.x = element_blank()))
          dev.off()
          incProgress(1/n, detail = paste("Species : ", i))
        }
      })            
      zip(zipfile = file3, files = all1)
    }, 
    contentType = 'application/zip'
  )
  
  bard <- reactive({
    dat1 <- dat1()
    dat1$Common.Name <- splistt()$Atlas.Name[match(dat1$Common.Name,splistt()$Common.Name)]
    dat1 <- dat1[!dat1$Common.Name == 'Removed',]
    dat1$Common.Name <- gsub('/','-',dat1$Common.Name)
    dat1$Common.Name <- factor(dat1$Common.Name)
    dlists <- subcl() 
    dat1 <- left_join(dat1, dlists, by = c("Submission.ID" = "List.ID") )
    dat1 <- dat1[!duplicated(dat1),]
    dat1 <- data.frame(dat1)
    datd <- dat1 %>% filter(Season == 'Dry')
    dlists <- datd %>% group_by(Common.Name) %>% summarise(dryf = n()/length(unique(datd$Submission.ID))*100)
    datw <- dat1 %>% filter(Season == 'Wet')
    wlists <- datw %>% group_by(Common.Name) %>% summarise(wetf = n()/length(unique(datw$Submission.ID))*100)
    tfreq <- data.frame(Common.Name = unique(splistt()$Atlas.Name))
    tfreq <- left_join(tfreq,wlists)
    tfreq <- left_join(tfreq,dlists)
    tfreq
  })
  
  barp <- function(species,season){
    freq <- bard() %>% filter(Common.Name == species)
    if (season == 'Dry') freq <- freq$dryf[1]
    if (season == 'Wet') freq <- freq$wetf[1]
    if(is.na(freq)) freq <- 0
    par(oma = c(0,0,4,0)+.1, mar = c(0,0,0,0) + .1, xpd = NA)
    barplot(freq,xlim = c(0,100), horiz = T, col = input$barpcol, xaxt = 'nan')
    rect(0,.2,100,1.2, lwd = 3)
    text(input$textlr,input$textud,paste0(ifelse(freq > .1 | freq == 0,round(freq,1),'< 0.1'),'% of lists'), cex = input$barfontsize)
  }
  
  output$wetbarimdisp <- renderImage({
    validate(
      need(input$ebdwld,'Please Upload Bird Data'),
      need(input$liscel,'Please Upload List to Subcell Matching')
    )
    outfile2 <- tempfile(fileext='.png')
    png(outfile2, width = input$barimwidth, height = input$barimheight,units = 'px', res = input$barimres)
    print(barp(input$spec,'Wet'))
    dev.off()
    
    list(src = outfile2,
         width = paste0(input$barimwidht,'px'), height = paste0(input$barimheight,'px'),
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  output$drybarimdisp <- renderImage({
    validate(
      need(input$ebdwld,'Please Upload Bird Data'),
      need(input$liscel,'Please Upload List to Subcell Matching')
    )
    outfile3 <- tempfile(fileext='.png')
    png(outfile3, width = input$barimwidth, height = input$barimheight,units = 'px', res = input$barimres)
    print(barp(input$spec,'Dry'))
    dev.off()
     
    list(src = outfile3,
         width = paste0(input$barimwidht,'px'), height = paste0(input$barimheight,'px'),
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  output$dl_wetbar <- downloadHandler(
    filename = function() { paste(input$spec, '_Wet.tiff', sep='') },
    content = function(file5) {
      tiff(file5, width = input$barimwidth, height = input$barimheight,units = 'px', res = input$barimres)
      print(barp(input$spec,'Wet'))
      dev.off()
    })
  
  output$dl_drybar <- downloadHandler(
    filename = function() { paste(input$spec, '_Dry.tiff', sep='') },
    content = function(file6) {
      tiff(file6, width = input$barimwidth, height = input$barimheight,units = 'px', res = input$barimres)
      print(barp(input$spec,'Dry'))
      dev.off()
    })
  
  output$dl_allbar <- downloadHandler(
    filename = function() { paste('All_BarCharts','.zip', sep='') },
    content = function(file7) {
      all2 <- c()
      tmpdir <- tempdir()
      setwd(tempdir())
      withProgress(message = 'Making plot', value = 0, {
        n <- length(unique(splistt()$Atlas.Name))
        for (i in unique(splistt()$Atlas.Name)){
          for ( j in c('Wet','Dry')){
            path = paste0(i,'_',j,'.tiff')
            all2 <- c(all2,path)
            tiff(paste0(i,'_',j,'.tiff'), width = input$barimwidth, height = input$barimheight,units = 'px', res = input$barimres)
            print(barp(i,j))
            dev.off()
          }
          incProgress(1/n, detail = paste("Species : ", i))
        }
      })            
      zip(zipfile = file7, files = all2)
    }, 
    contentType = 'application/zip'
  )
  
  
}

shinyApp(ui = ui, server = server)


