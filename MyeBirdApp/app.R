library(shiny)
library(dplyr)
library(leaflet)
library(rgdal)
library(shinythemes)
library(shinydashboard)
library(googleVis)
library(DT)
library(shinyBS)
library(RColorBrewer)
library(mapview)
library(devtools)



indsp <- read.csv('ebird_spclist.csv')

ebird_loc_code <- read.csv('ebird_subnational_codes.csv')

googloc <- makeIcon(
  iconUrl = "http://maps.google.com/mapfiles/ms/icons/purple.png",
  iconWidth = 25, iconHeight = 25,
  iconAnchorX = 12, iconAnchorY = 25)

source('calendarHeat.R')

ui <- dashboardPage(title = 'MyeBirdApp',

  
   
   dashboardHeader(title = "My eBird"),
   
   dashboardSidebar(
        img(src = 'BCI-logo-white.png', width = 230),
        conditionalPanel(
          'input.page == "Home"',
          fileInput('file1', h3('Upload',accept = c('.zip','.csv'))),
          helpText(h4("Just upload your data in exactly the same format (zip) that you got it from eBird; we'll unzip it for you. You can upload the unzipped '.csv' file as well"))
          ),
        
        conditionalPanel(
          'input.page == "Trip Report"',
          dateRangeInput("dates", label = h3("Date Range")),
          uiOutput('locs'),
          conditionalPanel(
            'input.locations',
            selectInput('splashtrip','Spuhs and Slashes', choices = c('Include','Exclude')),
            downloadButton('downloadData', 'Download')
          )
        ),
        
        conditionalPanel(
          'input.page == "My Locations (India)"',
          uiOutput('myloc_state'),
          uiOutput('myloc_dist')
        ),
        
        conditionalPanel(
          'input.page == "Birding Calendar"',
          uiOutput('yearsel')
        ),
        
        
        conditionalPanel(
          'input.page == "My Species"',
          selectInput('splash','Spuhs and Slashes', choices = c('Include','Exclude')),
          selectInput('filterby','Filter by', choices = c('','Location','Date'), selected = NULL),
          conditionalPanel(
            'input.filterby == "Location"',
            uiOutput('filterloc')
          ),
          conditionalPanel(
            'input.filterby == "Date"',
            uiOutput('filterdat')
          ),
          conditionalPanel(
            'input.filterby',
            actionButton('res','Reset Filter')
          )
        )
      ),
      

      dashboardBody(
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        tabsetPanel(
          id = 'page',
          tabPanel('Home', fluidRow(box(title = 'My Birding', solidHeader = T, status = 'primary',leafletOutput('map1', height = '700px'), width = 9,height = '800px', footer =  'The map above shows the locations you have birded at. The Locations are clustered and the number denotes the number of locations in that region. Zoom in to find the individual locations and their respective lists'), box(title = 'About', solidHeader = T, status = 'primary',width = 3, p('Welcome to the MyeBirdApp, brought to you by Bird Count India', a('(birdcount.in)', href = 'http://www.birdcount.in/')), h4(strong('About')),p('This app is designed for you to explore your own eBird data, supplementing the features available directly through your', a('eBird account',href = 'http://ebird.org/ebird/MyEBird?cmd=Start') ,'. Note that this app is particularly customized for use in India, although birders in other countries may find some of the functionality useful.
You use the app by downloading your data from eBird', a('http://ebird.org/ebird/downloadMyData',href = 'http://ebird.org/ebird/downloadMyData'), 'which is then sent to you in a zip file. Directly upload that zip file here.'), h5(strong('What you can do'),p('1. Explore the locations you have birded in India and view some basic details of your birding there.'),p("2. View the species you have seen, where you've seen them and your notes of them."),p("3. View a consolidated list of the species you've seen either on a trip, over a duration or at a specific location that you can then download and share."),p('4.  View a daily calendar of your birding effort and contribution to eBird in terms of the number of lists and species', br(), br(),p("Comments and suggestions are welcome! Please send them to",a('skimmer@birdcount.in',href = 'skimmer@birdcount.in'))))))),
          tabPanel('My Locations (India)',fluidRow(box(title ='My locations', solidHeader = T, status = 'primary',dataTableOutput('myloc_table'),h4('Click on location to view lists')), box(leafletOutput('myloc_map', height = '600px'))),uiOutput('mylocpopup'), height = '600px'),
          tabPanel('My Species',box(title = 'Species List', solidHeader = T, status = 'primary',h4('Click on species to view on map'),dataTableOutput('myspcs'), actionButton('unsp','Reset Selected'), width = 5), box(title = 'Species Map', solidHeader = T, status = 'primary',h4('Click on species for details'),dataTableOutput('myspcsdet'),uiOutput('mysppopup'),leafletOutput('spcsobs', height = '550px'), downloadButton('spcdl', 'Download Map'), width = 7)),
          tabPanel('Trip Report',fluidRow(box(title = 'Trip Species List', solidHeader = T, status = 'primary', br(),dataTableOutput('splist'), h4(textOutput('listwarn'))),box(title = 'Location summaries', solidHeader = T, status = 'primary',dataTableOutput('tripstat')),box(leafletOutput('trippath', height = '550px'), height = '600px')),uiOutput('sppopup')),
          tabPanel('Birding Calendar',box(
            title = h3('Daily Count'), id = 'heatmap',
            plotOutput('calhmapls',height = '300px'),
            plotOutput('calhmapsp',height = '300px'), 
            width = NULL))
        )
      )
   )


server <- function(input, output, session) {

options(shiny.maxRequestSize=30*1024^2)   
  
  mydata <- reactive({
    if(is.null(input$file1)) return(NULL)
    if(tools::file_ext(input$file1)[1] == 'zip') dat <- read.csv(unz(input$file1$datapath,'MyEBirdData.csv'))
    if(tools::file_ext(input$file1)[1] == 'csv') dat <- read.csv(input$file1$datapath)
    tt <- left_join(dat,indsp, by = c('Scientific.Name' = 'SCIENTIFIC.NAME'))
    tt$COMMON.NAME <- as.character(tt$COMMON.NAME)
    tt$COMMON.NAME[is.na(tt$COMMON.NAME)] <- as.character(tt$Common.Name[is.na(tt$COMMON.NAME)])
    tt$Date <- as.Date(tt$Date,'%m-%d-%Y')
    tt$year <- format(tt$Date, '%Y')
    tt <- left_join(tt,ebird_loc_code, by = c("State.Province" = "SUBNATIONAL1_CODE" ,"County" = "SUBNATIONAL2_NAME"))
    tt$Link <- paste0('<a href=http://ebird.org/ebird/view/checklist/',tt$Submission.ID," target=\"_blank\">",tt$Submission.ID,"</a>")
    tt
  })
  


  locst <- reactive({
    validate(need(input$file1, 'Please upload your data'))
    lowd <- min(input$dates)
    higd <- max(input$dates)
    locs <- mydata() %>% filter(Date >= lowd, Date <= higd)
    locs <- unique(locs$Location)
    if(length(locs) == 0) locs <- NULL
    locs
  })
  
  output$locs <- renderUI({
    selectInput('locations', label = h3('Select Locations'), choices = sort(locst()), multiple = T)
  })
  
  
  output$map1 <- renderLeaflet({
    validate(need(input$file1, 'Please upload your data'))
    loc <- mydata() %>% group_by(Submission.ID) %>% slice(1) %>% ungroup()
    loc$Submission.ID <- paste0('<a href=http://ebird.org/ebird/view/checklist/',loc$Submission.ID," target=\"_blank\">",loc$Date,"</a>")
    loc <- loc %>% group_by(Location) %>% arrange(desc(Date)) %>% mutate(nmore = ifelse(n() > 10, paste0('<b>+',n()-10,' more lists</b> (View in my locations)'),'')) %>% slice(1:10) %>% mutate(lists = paste(Submission.ID, collapse = '<br/>')) %>% slice(1)
    leaflet() %>% addTiles(urlTemplate = "http://a.tiles.mapbox.com/v3/openstreetmap.1b68f018/{z}/{x}/{y}.png",attribution = 'Mapbox <a href="http://mapbox.com/about/maps" target="_blank">Terms &amp; Feedback</a> ; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% addMarkers(data = loc, lng = loc$Longitude, lat = loc$Latitude,icon = googloc, popup = paste(paste0('<b>',loc$Location,'</b>'),loc$lists,loc$nmore, sep = '<br/>'), clusterOptions = markerClusterOptions(), options = popupOptions(maxHeight = 100), label = loc$Location)
  })
  
  spcslist <- reactive({
    if(is.null(input$locations)) return(NULL)
    lowd <- min(input$dates)
    higd <- max(input$dates)
    spc <- mydata() %>% filter(Date >= lowd, Date <= higd, Location %in% input$locations) %>% group_by(COMMON.NAME,Date,Time,Location) %>% slice(1) %>% ungroup() %>%  select(COMMON.NAME,Scientific.Name,Taxonomic.Order,Longitude,Latitude,Submission.ID,Count,Location,Date,Time,Species.Comments,CATEGORY,Link) %>% arrange(Taxonomic.Order)
    if(input$splashtrip == 'Exclude') spc <- spc %>% filter(!CATEGORY %in% c('spuh','slash') )
    spc <- data.frame(spc)
    spc
  })
  
  output$listwarn <- renderText({ifelse(is.null(locst()),'No lists in selected date range','Select locations and click on a species to view details')})

  output$splist <- DT::renderDataTable({
    validate(need(input$file1, 'Please upload your data'))
    spc <- spcslist()[,1:2]
    if(is.null(input$locations) == F) colnames(spc) <- c("English (India) name","Scientific Name")
    spc <- spc[!duplicated(spc),]
    if(is.null(input$locations) == F) row.names(spc) <- c(1:nrow(spc))
    DT::datatable(spc, selection = 'single')
  })
  
  output$trippath <- renderLeaflet({
    if(is.null(input$locations)) return(NULL)
    trpath <- spcslist() %>% group_by(Date,Location,Latitude,Longitude) %>% summarise(Lists = n_distinct(Submission.ID)) %>% arrange(Date)
    leaflet() %>% addTiles(urlTemplate = "http://a.tiles.mapbox.com/v3/openstreetmap.1b68f018/{z}/{x}/{y}.png", attribution = 'Mapbox <a href="http://mapbox.com/about/maps" target="_blank">Terms &amp; Feedback</a> ; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% addMarkers(data = trpath, lng = trpath$Longitude,lat = trpath$Latitude, popup = trpath$Location) %>% addPolylines(data = trpath, lng = trpath$Longitude,lat = trpath$Latitude)
  })
  
  output$tripstat <- DT::renderDataTable({
    if(is.null(input$locations)) return(NULL)
    lowd <- min(input$dates)
    higd <- max(input$dates)
    spc <- mydata() %>% filter(Date >= lowd, Date <= higd, Location %in% input$locations, !CATEGORY %in% c('spuh','slash')) %>% group_by(Location) %>% mutate(Richness = n_distinct(COMMON.NAME)) %>% group_by(Submission.ID) %>% slice(1) %>% group_by(Location) %>% summarise('Lists (Complete)' = paste0(n_distinct(Submission.ID),' (', sum(All.Obs.Reported),')'), Species = min(Richness), 'Duration (Mins)' = sum(Duration..Min., na.rm = TRUE))
    DT::datatable(spc, options = list(searching = F, paging = F))
  })

  output$downloadData <- downloadHandler(
    filename = 'mytriplist.csv',
    content = function(file) {
      downdat <- spcslist() %>% select(COMMON.NAME,Scientific.Name,Count,Location,Date,Time,Species.Comments,Submission.ID,Latitude,Longitude)
      downdat$Link <- paste0('http://ebird.org/ebird/view/checklist/',downdat$Submission.ID)
      write.csv(downdat, file)
  })
  
  output$yearsel <- renderUI({
    validate(need(input$file1, ''))
    selectInput('year','Select Year', choices = unique(mydata()$year[order(mydata()$year, decreasing = T)]))
  })
  
  output$calhmapls <- renderPlot({
    validate(need(input$file1, 'Please upload your data'))
    caldat <- mydata() %>% filter(year == input$year) %>% group_by(Date) %>% summarise(deff = round(n_distinct(Submission.ID),0))
    return(calendarHeat(caldat$Date,caldat$deff,varname = 'Lists',color = 'w2b'))
  })
  
  output$calhmapsp <- renderPlot({
    validate(need(input$file1, 'Please upload your data'))
    caldat <- mydata() %>% group_by(Date) %>% filter(!CATEGORY %in% c('spuh','slash'),year == input$year) %>% summarise(rich = round(n_distinct(parent),0))
    return(calendarHeat(caldat$Date,caldat$rich,varname = 'Species',color = 'w2r'))
  })

  output$myloc_state <- renderUI({
    validate(need(input$file1, 'Please upload your data'))
    st <- as.character(unique(mydata()$SUBNATIONAL1_NAME[!is.na(mydata()$COUNTRY_CODE)]))
    selectInput('mylocst',label = h3('Select State'), choices = c('All',st[order(st)]))
  })
  
  output$myloc_dist <- renderUI({
    validate(need(input$file1, ''))
    ifelse(input$mylocst == 'All', loc <- unique(mydata()$County),loc <- unique(mydata()$County[mydata()$SUBNATIONAL1_NAME == input$mylocst & !is.na(mydata()$COUNTRY_CODE)]))
    selectInput('mylocdt',label = h3('Select District'), choices = c('All',loc[order(loc)]))
  })
  
  rctmyloc_table <- reactive({
    validate(need(input$file1, 'Please upload your data'))
    my_locs <- mydata() %>% filter(!CATEGORY %in% c('spuh','slash'))
    if(input$mylocst != 'All'){
      if(input$mylocdt == 'All') my_locs <- my_locs %>% filter(SUBNATIONAL1_NAME == input$mylocst)
    }
    if(input$mylocdt != 'All') my_locs <- my_locs %>% filter(County == input$mylocdt)
    my_locs <- my_locs  %>% group_by(Location) %>% mutate(Richness = n_distinct(COMMON.NAME)) %>% group_by(Submission.ID) %>% slice(1) %>% group_by(Location) %>% summarise('Lists (Complete)' = paste0(n_distinct(Submission.ID),' (', sum(All.Obs.Reported),')'), Species = min(Richness), 'Duration (Mins)' = sum(Duration..Min.), Latitude1 = min(Latitude), Longitude1 = min(Longitude) )
    my_locs
  })
  
  
  output$myloc_table <- renderDataTable({
    DT::datatable(rctmyloc_table()[,1:4],selection = 'single')
  })
  
  output$myloc_map <- renderLeaflet({
    validate(need(input$file1, 'Please upload your data'))
    my_locs <- rctmyloc_table()
    leaflet() %>% addTiles(urlTemplate = "http://a.tiles.mapbox.com/v3/openstreetmap.1b68f018/{z}/{x}/{y}.png",attribution = 'Mapbox <a href="http://mapbox.com/about/maps" target="_blank">Terms &amp; Feedback</a> ; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% addMarkers(data = my_locs, lng = my_locs$Longitude1,lat = my_locs$Latitude1,icon = googloc, popup = my_locs$Location, label = my_locs$Location)
  })
  
  observeEvent(input$myloc_table_rows_selected, {
    toggleModal(session, "modalloc", "open")
  })
  
  mylocdeets <- eventReactive(input$myloc_table_rows_selected,{
    rown <- data.frame(rctmyloc_table())
    rown <- rown[input$myloc_table_rows_selected,1]
    list1 <- mydata() %>% filter(Location == rown) %>% group_by(Link) %>% mutate('Species' = n_distinct(COMMON.NAME)) %>% slice(1) %>% ungroup() %>% select(Date,Time,Duration..Min.,Species,All.Obs.Reported,Link) %>% arrange(desc(Date))
    colnames(list1)[3] <- 'Duration (Mins)'
    colnames(list1)[5] <- 'Complete'
    list1$Complete[list1$Complete == 1] <- 'Yes'
    list1$Complete[list1$Complete == 0] <- 'No'
    DT::datatable(list1, options = list(searching = F, paging = F), escape = F)
  })
  
  output$mylocpopup <- renderUI({
    rown <- rctmyloc_table()[input$myloc_table_rows_selected,1]
    bsModal("modalloc", rown, "", size = "large",DT::renderDataTable(mylocdeets(), escape = F)
    )
  })
  
  
  
  observeEvent(input$splist_rows_selected, {
    toggleModal(session, "modaltrip", "open")
  })
  
  tripdeets <- eventReactive(input$splist_rows_selected,{
    rown <-  spcslist()[!duplicated(spcslist()[,1:2]),]
    rown <- rown[input$splist_rows_selected,1]
    spcslist1 <- spcslist() %>% filter(COMMON.NAME == rown) %>% select(Location,Date,Time,Species.Comments,Link)
    DT::datatable(spcslist1, options = list(searching = F, paging = F), escape = F)
  })
  
  output$sppopup <- renderUI({
    rown <-  spcslist()[!duplicated(spcslist()[,1:2]),]
    rown <- rown[input$splist_rows_selected,1]
    bsModal("modaltrip", rown, "", size = "large",DT::renderDataTable(tripdeets(), escape = F)
    )
  })
  
  mysplist <- reactive({
    myspc <- mydata() %>% group_by(COMMON.NAME,Date,Time,Location) %>% slice(1) %>% ungroup() %>%  select(COMMON.NAME,Scientific.Name,Taxonomic.Order,Longitude,Latitude,Submission.ID,Count,Location,Date,Time,Breeding.Code,Species.Comments,CATEGORY) %>% arrange(Taxonomic.Order)
    myspc <- data.frame(myspc)
    if(input$splash == 'Exclude') myspc <- myspc %>% filter(!CATEGORY %in% c('spuh','slash') )
    if(input$filterby == 'Location') myspc <- myspc %>% filter(Location == input$locfil)
    if(input$filterby == 'Date') {
      lowd <- min(input$datfil)
      higd <- max(input$datfil)
      myspc <- myspc %>% filter(Date >= lowd, Date <= higd)
    } 
    if(nrow(myspc) == 0) return(NULL)
    myspc <-  myspc[,1:2]
    colnames(myspc) <- c("English (India) name","Scientific Name")
    myspc <- myspc[!duplicated(myspc),]
    row.names(myspc) <- c(1:nrow(myspc))
    myspc
  })
  
  output$myspcs <- DT::renderDataTable({
    validate(need(input$file1, 'Please upload your data'))
    input$unsp
    DT::datatable(mysplist())
  })
  
  spcsrown <- reactive(input$myspcs_rows_selected)
  
  
  spcsmap <- reactive({
    spmap <- leaflet() %>% addTiles(urlTemplate = "http://a.tiles.mapbox.com/v3/openstreetmap.1b68f018/{z}/{x}/{y}.png",attribution = 'Mapbox <a href="http://mapbox.com/about/maps" target="_blank">Terms &amp; Feedback</a> ; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') 
    if(is.null(spcsrown()) == F){
      spcsel <- factor(mysplist()[spcsrown(),1], levels = mysplist()[spcsrown(),1])
      spcspoints <- mydata() %>% filter(COMMON.NAME %in% spcsel) %>% group_by(COMMON.NAME,Location) %>% slice(1)
      spcspoints$COMMON.NAME <- factor(spcspoints$COMMON.NAME, levels = mysplist()[spcsrown(),1])
      mypal <- colorFactor(brewer.pal(9,'Set1')[1:length(spcsel)], domain = spcsel)
      spmap <- spmap %>% addCircleMarkers(data = spcspoints, lng = jitter(spcspoints$Longitude, amount = 0.0005), lat = jitter(spcspoints$Latitude, amount = 0.0005), stroke = F, fillColor  = ~mypal(spcspoints$COMMON.NAME), popup = spcspoints$Location, label = spcspoints$COMMON.NAME,radius = 8, fillOpacity = .5) %>% addLegend('topright',pal = mypal, values = spcsel)
    }
    
    spmap
  })
  
  output$spcsobs <- renderLeaflet({
    spcsmap()
  })
  
  
  output$spcdl <- downloadHandler(
    filename = 'species_map.html',
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      mapshot(spcsmap(), url = file)
    }
  )
  
  rctmyspcsdet <- reactive({
    validate(need(input$file1, 'Please upload your data'))
    spcsel1 <- factor(mysplist()[spcsrown(),1], levels = mysplist()[spcsrown(),1])
    spcslist2 <- mydata() %>% filter(COMMON.NAME %in% spcsel1) %>% group_by(COMMON.NAME) %>% summarise('Lists' = n_distinct(Submission.ID),'Locations' = n_distinct(Location))
  })

    
  output$myspcsdet <- renderDataTable({
    spcslist2 <- rctmyspcsdet()
    colnames(spcslist2)[1] <- "English (India) name"
    DT::datatable(spcslist2, options = list(searching = F),selection = 'single')
  })
  
  observeEvent(input$myspcsdet_rows_selected, {
    toggleModal(session, "modalspcs", "open")
  })
  
  myspdeets <- eventReactive(input$myspcsdet_rows_selected,{
    rown <- data.frame(rctmyspcsdet())
    rown <- rown[input$myspcsdet_rows_selected,1]
    spcslist1 <- mydata() %>% filter(COMMON.NAME == rown) %>% select(Date,Time,Location,Species.Comments, Link) %>% arrange(Date)
    DT::datatable(spcslist1, options = list(searching = F, paging = F), escape = F)
  })
  
  output$mysppopup <- renderUI({
    rown <- rctmyspcsdet()[input$myspcsdet_rows_selected,1]
    bsModal("modalspcs", rown, "", size = "large",DT::renderDataTable(myspdeets(), escape = F)
    )
  })
  

  output$filterloc <- renderUI({
    selectizeInput('locfil','Select Location',choices = unique(mydata()$Location)[order(unique(mydata()$Location))], multiple = T)
  })
  
  output$filterdat <- renderUI({
    dateRangeInput('datfil','Select Dates')
  })
  
  observe({
    input$res
    updateSelectInput(session,'filterby','Filter by', choices = c('','Location','Date'), selected = NULL)
  })

}


# Run the application 
shinyApp(ui = ui, server = server)


