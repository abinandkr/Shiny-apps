library(shiny)
library(dplyr)
library(leaflet)
library(stringr)
library(rgdal)

dat <- read.csv('mig_dat_new.csv')
k1 <- readOGR('.','Distrits with eBird code')


ui <- shinyUI(fluidPage(
   
   titlePanel("Migration"),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("month",
                     "Select Month:", min = as.Date("2016-01-01"),max =as.Date("2016-12-01"),value=as.Date("2016-01-01"),timeFormat="%b", step = 31, ticks = F),
         selectInput('species','Select Species',levels(dat$COMMON.NAME)),
         uiOutput('box1')
    
      ),
      
      mainPanel(
         leafletOutput("distPlot", height = 800)
      )
   )
))

server <- shinyServer(function(input, output) {
  
   output$box1 <- renderUI({
     print(paste(str_replace(input$species, fixed(' '), '_'),'jpg', sep ='.'))
     img(src= paste(str_replace(input$species, fixed(' '), '_'),'jpg', sep ='.'), align = "bottom")
   })
  
   output$distPlot <- renderLeaflet({
     dat <- dat %>% filter(month == as.numeric(strsplit(as.character(input$month),split  = '-')[[1]][2]), COMMON.NAME == input$species)
     k2 <- k1[k1@data$district10 %in% dat$SUBNATIONAL2_CODE,]
     leaflet() %>% addTiles() %>% setMaxBounds(68.7,6,97.25,37.6)%>% fitBounds(68.7,6,97.25,37.6) %>% addPolygons(data = k2, stroke = T, color = 'Orange',weight = 2, fillColor = 'Orange', fillOpacity = 1)
   })
})


# Run the application 
shinyApp(ui = ui, server = server)
