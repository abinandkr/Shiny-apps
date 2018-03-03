library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(lubridate)
library(stringr)
library(shiny)
library(DT)
library(leaflet)

# Define UI for application that draws a histogram
ui <- navbarPage('My Application',
                 tabPanel('Text Messages',
                          sidebarLayout(
                            sidebarPanel(
                              tags$head(tags$style(type="text/css", "
             #loadmessage {
                                                   position: fixed;
                                                   bottom: 0px;
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
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"
                              ),
                              
                              conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                               tags$div("Loading...",id="loadmessage")),
                              fileInput('textdat','Input Text Delivery File', accept = '.csv'),
                              conditionalPanel('output.textfileUploaded',
                              uiOutput('textdatewind'),
                              selectInput('sumcol','Summarise by', choices = c('Month','Week','Day')),
                              selectInput('calctyp','Calculate',choices = c('Numbers','Proportions')),
                              conditionalPanel('input.text_tabs == "Delivery Report" | input.text_tabs == "Delivery Lag"',
                              selectInput('disptyp','Display as', choices = c('Line Plot','Bar Plot','Table'))),
                              uiOutput('textselloc'),
                              checkboxInput('expstat','Expand Delivery Status')),
                              width = 2
                              ),
                          mainPanel(
                            tabsetPanel(id = 'text_tabs',
                              tabPanel('Delivery Report', 
                                       conditionalPanel(
                                         'input.disptyp == "Line Plot"',
                                         plotOutput('del_lineplot')),
                                       conditionalPanel(
                                         'input.disptyp == "Table"',
                                         dataTableOutput('del_table')),
                                       conditionalPanel(
                                         'input.disptyp == "Bar Plot"',
                                         plotOutput('del_barplot'))),                                       
                              tabPanel('Delivery Lag', 
                                       conditionalPanel(
                                         'input.disptyp == "Line Plot"',
                                         plotOutput('lag_lineplot')),
                                       conditionalPanel(
                                         'input.disptyp == "Table"',
                                         dataTableOutput('lag_table')),
                                       conditionalPanel(
                                         'input.disptyp == "Bar Plot"',
                                         plotOutput('lag_barplot'))),
                              tabPanel('Location', dataTableOutput('loc_table'))
                            )
                            )
                          )
                          ),
                 tabPanel('Call Logs',
                          sidebarLayout(
                            sidebarPanel(
                              tags$head(tags$style(type="text/css", "
             #loadmessage {
                                                   position: fixed;
                                                   bottom: 0px;
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
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"
                              ),
                              
                              conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                               tags$div("Loading...",id="loadmessage")),
                              fileInput('calldat','Upload Voice Call Data', accept = '.csv'),
                              conditionalPanel('output.callfileUploaded',
                                               uiOutput('calldatewind'),
                                               sliderInput('mindur','Filter minimum duration(s)',min = 0,max = 60,value = 0),
                                               selectInput('callsumcol','Summarise by', choices = c('Month','Week','Day')),
                                               selectInput('callcalctyp','Calculate',choices = c('Numbers','Proportions')),
                                               selectInput('calldisptyp','Display as', choices = c('Line Plot','Bar Plot','Table')),
                                               checkboxInput('expcall','Expand Dial Status')),
                              width = 2
                            ),
                            mainPanel(
                              conditionalPanel(
                                'input.calldisptyp == "Line Plot"',
                                plotOutput('ans_lineplot')),
                              conditionalPanel(
                                'input.calldisptyp == "Table"',
                                dataTableOutput('ans_table')),
                              conditionalPanel(
                                'input.calldisptyp == "Bar Plot"',
                                plotOutput('ans_barplot'))
                            )
                          )
                          ),
                 tabPanel('Conflict',
                          sidebarLayout(
                            sidebarPanel(
                              tags$head(tags$style(type="text/css", "
             #loadmessage {
                                                   position: fixed;
                                                   bottom: 0px;
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
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"
                              ),
                              
                              conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                               tags$div("Loading...",id="loadmessage")),
                              fileInput('conflictdat','Upload Conflict Data'),
                              conditionalPanel('output.conflictfileUploaded',
                                               uiOutput('confdatewind'),
                                               conditionalPanel('input.conf_tabs == "Map"',
                                                                selectInput('mapcolby','Colour by',choices = c('Year','Category'))),
                                               conditionalPanel('input.conf_tabs == "Graphs"',
                                                                selectInput('confsum','Summarise by',choices = c('Year','Month')),
                                                                selectInput('confcalc','Calculate',choices = c('Numbers','Cumulative')))
                                               ),
                            width = 2
                            ),
                            mainPanel(
                              tabsetPanel(id = 'conf_tabs',
                                          tabPanel('Map',
                                            leafletOutput('confmap',height = '600px')
                                          ),
                                          tabPanel('Graphs',
                                                   plotOutput('confplot'))
                              )
                            )
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
options(shiny.maxRequestSize=300*1024^2)   
  
  output$textfileUploaded <- reactive({
    return(!is.null(input$textdat))
  })
  
  outputOptions(output, 'textfileUploaded', suspendWhenHidden=FALSE)
  
  text_dat <- reactive({
    validate(need(input$textdat, 'Please upload your data'))
    dat <- read.csv(input$textdat$datapath)
    dat$location <- str_extract(dat$MESSAGE.TEXT, '(?<=Elephants are in\\s)\\w+')
    locs <- unique(dat$location)
    locs <- locs[!is.na(locs)]
    locmatch <- lapply(locs, function(x) agrep(x, locs, max.distance = .1))
    locdat <- data.frame(loc1 = locs,loc2 = locs, stringsAsFactors = F)
    
    for(i in 1:length(locmatch)){
      if (length(locmatch[[i]]) == 1){
        next
      } else{
        for(j in locmatch[[i]]){
          if(length(intersect(locmatch[[i]],locmatch[[j]])) > 1){
            locdat$loc2[i] <- locdat$loc1[min(intersect(locmatch[[i]],locmatch[[j]]))]
          }
        }
      }
    }
    
    require('plyr')
    dat$location <- mapvalues(dat$location,locdat$loc1,locdat$loc2)
    detach("package:plyr", unload=TRUE)
    dat$DATE <- as.Date(dat$DATE, '%d-%B-%y')
    dat$MONTH <- factor(month(dat$DATE,label = T,abbr = T),levels = month.abb)
    colnames(dat)[colnames(dat) == 'MONTH'] <- 'Month'
    dat$Week <- format(dat$DATE, '%W')
    dat$Day <- dat$DATE - as.Date(paste(year(dat$DATE),'01-01',sep ='-'))
    dat$del_und <- ifelse(dat$STATUS == 'Delivered','Delivered','Undelivered')
    dat$Lag <- cut(dat$LATENCY.MINS.,c(0,30,60,120,Inf),include.lowest = T,labels = c('< 0.5 hrs',' 0.5 to 1 hrs','1 to 2 hrs','>2 hrs'))
    dat
  })

  output$textdatewind <- renderUI({
    validate(need(input$textdat, 'Please upload your data'))
    dateRangeInput('datewind','Select Date Range',start = min(text_dat()$DATE),end = max(text_dat()$DATE),min = min(text_dat()$DATE),max = max(text_dat()$DATE))
  })
  
  output$textselloc <- renderUI({
    validate(need(input$textdat, 'Please upload your data'))
    selectInput('selloc','Location Filter', choices = sort(unique(text_dat()$location)), multiple = T)
  })
     
  del_dat <- reactive({
    validate(need(input$textdat, 'Please upload your data'))
    statcol <- ifelse(input$expstat == TRUE,'STATUS','del_und' )
    ifelse(!is.null(input$selloc),ddat <- text_dat() %>% filter(location %in% input$selloc), ddat <- text_dat())
    
    ddat <- ddat %>% filter(between(DATE,input$datewind[1],input$datewind[2])) %>%
      group_by_('YEAR',input$sumcol,statcol) %>% 
      summarise('Messages' = n()) %>% 
      group_by_('YEAR',input$sumcol) %>% 
      mutate('Proportion of Messages(%)' = round(Messages/sum(Messages)*100,2))
    
    colnames(ddat)[colnames(ddat) == statcol] <- 'STATUS'
  
    ddat1 <- expand.grid('YEAR' = unique(ddat$YEAR),x = unique(data.frame(ddat)[,input$sumcol]) , 'STATUS' = unique(ddat$STATUS), stringsAsFactors = F)
    colnames(ddat1)[colnames(ddat1) == 'x'] <- input$sumcol
    ddat1 <- left_join(ddat1,ddat) 
    ddat1[is.na(ddat1)] <- 0
    ddat1 <- ddat1 %>% group_by_('YEAR',input$sumcol) %>% filter(sum(Messages) > 0)
    ddat1
  })
  
  calctypserv <- reactive({ifelse(input$calctyp == 'Numbers','Messages','`Proportion of Messages(%)`')
  })
  
  output$del_lineplot <- renderPlot({
    if(is.null(input$textdat)) return(NULL)
    ggplot(data = del_dat(),aes_string(x = input$sumcol, y = calctypserv(), color = 'STATUS', group = 'STATUS')) + 
      facet_grid(.~YEAR)+
      geom_line(lwd = 1.5) + 
      geom_point(size = 3) + 
      theme_bw()
  })
  
  output$del_barplot <- renderPlot({
    validate(need(input$textdat, 'Please upload your data'))
    ggplot(data = del_dat(),aes_string(x = input$sumcol, y = calctypserv())) + 
      geom_bar(stat = 'identity',aes(fill = STATUS), position = 'dodge') + 
      facet_grid(.~YEAR)+
      theme_bw()
  })
  
  output$del_table <- renderDataTable({
    validate(need(input$textdat, 'Please upload your data'))
    DT::datatable(del_dat() %>% arrange_('YEAR',input$sumcol))
  })
  
  lag_dat <- reactive({
    validate(need(input$textdat, 'Please upload your data'))
    ldat <- text_dat() %>% filter(between(DATE,input$datewind[1],input$datewind[2]), STATUS == 'Delivered') %>%
      group_by_('YEAR',input$sumcol,'Lag') %>% 
      summarise('Messages' = n()) %>% 
      group_by_('YEAR',input$sumcol) %>% 
      mutate('Proportion of Messages(%)' = round(Messages/sum(Messages)*100,2))
    
    ldat1 <- expand.grid('YEAR' = unique(ldat$YEAR),x = unique(data.frame(ldat)[,input$sumcol]) , 'Lag' = unique(ldat$Lag), stringsAsFactors = F)
    colnames(ldat1)[colnames(ldat1) == 'x'] <- input$sumcol
    ldat1 <- left_join(ldat1,ldat) 
    ldat1[is.na(ldat1)] <- 0
    ldat1 <- ldat1 %>% group_by_('YEAR',input$sumcol) %>% filter(sum(Messages) > 0)
    ldat1
  })
  
  calctypserv <- reactive({ifelse(input$calctyp == 'Numbers','Messages','`Proportion of Messages(%)`')
  })
  
  output$lag_lineplot <- renderPlot({
    validate(need(input$textdat, 'Please upload your data'))
    ggplot(data = lag_dat(),aes_string(x = input$sumcol, y = calctypserv(), color = 'Lag', group = 'Lag')) + 
      facet_grid(.~YEAR)+
      geom_line(lwd = 1.5) + 
      geom_point(size = 3) + 
      theme_bw()
  })
  
  output$lag_barplot <- renderPlot({
    validate(need(input$textdat, 'Please upload your data'))
    ggplot(data = lag_dat(),aes_string(x = input$sumcol, y = calctypserv())) + 
      geom_bar(stat = 'identity',aes(fill = Lag), position = 'dodge') + 
      facet_grid(.~YEAR)+
      theme_bw()
  })
  
  output$lag_table <- renderDataTable({
    validate(need(input$textdat, 'Please upload your data'))
    DT::datatable(lag_dat() %>% arrange_('YEAR',input$sumcol))
  })
  
  output$loc_table <- renderDataTable({
    validate(need(input$textdat, 'Please upload your data'))
    statcol <- ifelse(input$expstat == TRUE,'STATUS','del_und' )
    
    ifelse(!is.null(input$selloc),ldat <- text_dat() %>% filter(location %in% input$selloc), ldat <- text_dat())
    
    ldat <- ldat %>% filter(between(DATE,input$datewind[1],input$datewind[2])) %>%
      group_by_('YEAR',input$sumcol,'location',statcol) %>% 
      summarise('Messages' = n()) %>% 
      group_by_('YEAR',input$sumcol,'location') %>% 
      mutate('Proportion of Messages(%)' = round(Messages/sum(Messages)*100,2))
    
    colnames(ldat)[colnames(ldat) == statcol] <- 'STATUS'
    
    ldat1 <- expand.grid('location' = unique(ldat$location),'YEAR' = unique(ldat$YEAR),x = unique(data.frame(ldat)[,input$sumcol]) , 'STATUS' = unique(ldat$STATUS), stringsAsFactors = F)
    colnames(ldat1)[colnames(ldat1) == 'x'] <- input$sumcol
    ldat1 <- left_join(ldat1,ldat) 
    ldat1[is.na(ldat1)] <- 0
    ldat1 <- ldat1 %>% group_by_('YEAR',input$sumcol,'location') %>% filter(sum(Messages) > 0)
    ldat1$location[ldat1$location == 0] <- 'Could not extract location'
    ldat1$location[is.na(ldat1$location)] <- 'Could not extract location'
    ldat1$location = factor(ldat1$location)
    ldat1$STATUS <- factor(ldat1$STATUS)
    DT::datatable(ldat1,filter = 'top', options = list(autoWidth = T))
  })
  
  output$callfileUploaded <- reactive({
    return(!is.null(input$calldat))
  })
  
  outputOptions(output, 'callfileUploaded', suspendWhenHidden=FALSE)
  
  call_dat <- reactive({
    validate(need(input$calldat, 'Please upload your data'))
    cdat <- read.csv(input$calldat$datapath,stringsAsFactors = F)
    cdat$Date <- as.Date(cdat$Date,'%d-%b-%y')
    cdat$DIAL.STATUS <- tolower(cdat$DIAL.STATUS)
    cdat$DIAL.STATUS <- paste(toupper(substr(cdat$DIAL.STATUS, 1, 1)), substr(cdat$DIAL.STATUS, 2, nchar(cdat$DIAL.STATUS)), sep="")
    cdat$Year <- year(cdat$Date)
    cdat$Month <- factor(month(cdat$Date,label = T,abbr = T),levels = month.abb)
    cdat$Week <- format(cdat$Date, '%W')
    cdat$Day <- cdat$Date - as.Date(paste(year(cdat$Date),'01-01',sep ='-'))
    cdat
  })
  
  output$calldatewind <- renderUI({
    validate(need(input$calldat, 'Please upload your data'))
    dateRangeInput('datewindc','Select Date Range',start = min(call_dat()$Date),end = max(call_dat()$Date),min = min(call_dat()$Date),max = max(call_dat()$Date))
  })
  
  ans_dat <- reactive({
    validate(need(input$calldat, 'Please upload your data'))
    statcol <- ifelse(input$expcall == TRUE,'DIAL.STATUS','ans_unans')
    cdat <- call_dat()
    cdat$DIAL.STATUS[as.numeric(cdat$callDuration) > 0 & as.numeric(cdat$callDuration) < input$mindur] <- '< Min Duration'
    cdat$ans_unans <- ifelse(cdat$DIAL.STATUS == 'Answered','Answered','Unanswered')
    cldat <- cdat %>% filter(between(Date,input$datewindc[1],input$datewindc[2])) %>%
      group_by_('Year',input$callsumcol,statcol) %>% 
      summarise('Calls' = n()) %>% 
      group_by_('Year',input$callsumcol) %>% 
      mutate('Proportion of Calls(%)' = round(Calls/sum(Calls)*100,2))
    colnames(cldat)[colnames(cldat) == statcol] <- 'Dial Status'
    cldat1 <- expand.grid('Year' = unique(cldat$Year),x = unique(data.frame(cldat)[,input$callsumcol]) , 'Dial Status' = unique(cldat$`Dial Status`), stringsAsFactors = F)
    colnames(cldat1)[colnames(cldat1) == 'x'] <- input$callsumcol
    cldat1 <- left_join(cldat1,cldat) 
    cldat1[is.na(cldat1)] <- 0
    cldat1 <- cldat1 %>% group_by_('Year',input$callsumcol) %>% filter(sum(Calls) > 0)
    cldat1
  })
  
  callcalctypserv <- reactive({ifelse(input$callcalctyp == 'Numbers','Calls','`Proportion of Calls(%)`')
  })
  
  output$ans_lineplot <- renderPlot({
    if(is.null(input$calldat)) return(NULL)
    ggplot(data = ans_dat(),aes_string(x = input$callsumcol, y = callcalctypserv(), color = '`Dial Status`', group = '`Dial Status`')) + 
      facet_grid(.~Year)+
      geom_line(lwd = 1.5) + 
      geom_point(size = 3) + 
      theme_bw()
  })
  
  output$ans_barplot <- renderPlot({
    validate(need(input$calldat, 'Please upload your data'))
    ggplot(data = ans_dat(),aes_string(x = input$callsumcol, y = callcalctypserv())) + 
      geom_bar(stat = 'identity',aes(fill = `Dial Status`), position = 'dodge') + 
      theme_bw()
  })
  
  output$ans_table <- renderDataTable({
    validate(need(input$calldat, 'Please upload your data'))
    DT::datatable(ans_dat() %>% arrange_('Year',input$callsumcol))
  })
   
  output$conflictfileUploaded <- reactive({
    return(!is.null(input$conflictdat))
  })
  
  outputOptions(output, 'conflictfileUploaded', suspendWhenHidden=FALSE)
  
  conflict_dat <- reactive({
    validate(need(input$conflictdat, 'Please upload your data'))
    conflictdat <- read.csv(input$conflictdat$datapath,stringsAsFactors = F)
    conflictdat$DATE <- as.Date(conflictdat$DATE,'%d-%m-%Y')
    conflictdat$Year <- year(conflictdat$DATE)
    conflictdat$Month <- month(conflictdat$DATE, label = T, abbr = T)
    conflictdat <- conflictdat %>% select(-c(Item.description,Quantity,Measure,Action, Company))
    conflictdat <- conflictdat[!duplicated(conflictdat),]
    pl1 <- unique(conflictdat$Place.category)
    require('plyr')
    conflictdat$Place.category <- mapvalues(conflictdat$Place.category, pl1, c('Ration Shop', "Residence",'Noon-meal Centre','Others','Ration Shop','Noon-meal Centre'))
    detach("package:plyr")
    conflictdat$Place.category <- factor(conflictdat$Place.category, levels = c('Residence','Ration Shop','Noon-meal Centre','Others'))
    conflictdat
  })
  
  output$confdatewind <- renderUI({
    validate(need(input$conflictdat, 'Please upload your data'))
    dateRangeInput('confdatewind','Filter by Date',start = min(conflict_dat()$DATE),end = max(conflict_dat()$DATE),min = min(conflict_dat()$DATE),max = max(conflict_dat()$DATE))
  })
  
  output$confmap <- renderLeaflet({
    conflictdat <- conflict_dat() %>% filter(between(DATE,input$confdatewind[1],input$confdatewind[2]))
    factpal1 <- colorFactor(palette = 'Set1',conflictdat$Year)
    factpal2 <- colorFactor(palette = 'Set1',conflictdat$Place.category)
    
    valmap <- leaflet() %>% addTiles(urlTemplate = "http://a.tiles.mapbox.com/v3/openstreetmap.1b68f018/{z}/{x}/{y}.png",attribution = 'Mapbox <a href="http://mapbox.com/about/maps" target="_blank">Terms &amp; Feedback</a> ; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>')

    if(input$mapcolby == 'Year'){
      valmap <- valmap %>% addCircleMarkers(data = conflictdat, lng = jitter(conflictdat$Longitude, factor = .000001),lat = jitter(conflictdat$Latitude, factor = .000001),radius = 5,fillOpacity = 1,stroke = F, popup = paste('<b>Date : </b>',conflictdat$DATE,'<br/>','<b>Herd : </b>',conflictdat$Herd,'<br/>','<b>Place : </b>',conflictdat$Site, sep = ''), color = ~factpal1(conflictdat$Year)) %>%
      addLegend('bottomright',pal = factpal1, values = conflictdat$Year)
    } else{
      valmap <- valmap %>% addCircleMarkers(data = conflictdat, lng = jitter(conflictdat$Longitude, factor = .000001),lat = jitter(conflictdat$Latitude, factor = .000001),radius = 5,fillOpacity = 1,stroke = F, popup = paste('<b>Date : </b>',conflictdat$DATE,'<br/>','<b>Herd : </b>',conflictdat$Herd,'<br/>','<b>Place : </b>',conflictdat$Site, sep = ''), color = ~factpal2(conflictdat$Place.category)) %>%
        addLegend('bottomright',pal = factpal2, values = conflictdat$Place.category)
    }
    valmap
  })
  
  output$confplot <- renderPlot({
    condat <- conflict_dat() %>%filter(between(DATE,input$confdatewind[1],input$confdatewind[2])) %>% group_by_('Year',input$confsum,'Place.category') %>% summarise(Incidents = n()) 
    tot <- conflict_dat() %>%filter(between(DATE,input$confdatewind[1],input$confdatewind[2])) %>% group_by_('Year',input$confsum) %>% summarise(Incidents = n()) %>% mutate(Place.category = 'Total') 
    condat <- bind_rows(condat,tot)
    if(input$confsum == 'Month'){
      condat <- left_join(expand.grid(Year = unique(condat$Year),Month = month.abb,Place.category = unique(condat$Place.category)),condat)
      condat$Incidents[is.na(condat$Incidents)] <- 0
      condat <- condat %>% group_by(Year,Place.category) %>% mutate(`Cumulative Incidents` = cumsum(Incidents)) %>% group_by(Year,Month) %>% filter(sum(Incidents) > 0)
    }else{
      condat <- left_join(expand.grid(Year = unique(condat$Year),Place.category = unique(condat$Place.category)),condat)
      condat$Incidents[is.na(condat$Incidents)] <- 0
      condat <- condat %>% group_by(Place.category) %>% mutate(`Cumulative Incidents` = cumsum(Incidents))
    }
    ctyp <- ifelse(input$confcalc == 'Numbers','Incidents','`Cumulative Incidents`')
    condat$Place.category <- factor(condat$Place.category, levels = c('Residence','Ration Shop','Noon-meal Centre','Others','Total'))
    colnames(condat)[which(names(condat) == "Place.category")] <- 'Place Category'
    g <- ggplot() + geom_line(data = condat, aes_string(x = input$confsum, y = ctyp,colour = '`Place Category`',group = '`Place Category`'), size = 1.5) + theme_bw()
    ifelse(input$confsum == 'Year',print(g), print(g + facet_wrap(~Year)))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

