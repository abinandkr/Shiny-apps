library(shiny)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(sp)
library(rgdal)
library(gridExtra)
library(grid)
library(gridBase)

load("all_rel_col.Rdata")

#all <- all %>% select(SAMPLING.EVENT.IDENTIFIER,group.id,ALL.SPECIES.REPORTED,LATITUDE,LONGITUDE,SUBNATIONAL1_CODE,SUBNATIONAL2_CODE,DURATION.MINUTES,upload.month,FULL.NAME,SPECIES.NAME,APPROVED,REVIEWED,CATEGORY) 

#save(all,file = 'all_rel_col.Rdata')


india.red <- readOGR('.','IndiaStates_2011')

st_codes <- read.csv('state_codes.csv', stringsAsFactors = F)

st_choices <- as.list(st_codes$SUBNATIONAL1_CODE)
names(st_choices) <- st_codes$state

#############################


# Define UI for application that draws a histogram
ui <- dashboardPage( title ='eBird Monthly Report'
   ,
   dashboardHeader(title = "eBird Monthly"),
   
   # Sidebar with a slider input for number of bins 
   dashboardSidebar(
         selectInput('state','Select State', choices = st_choices),
         selectInput('month','Select Month', choices = sort(unique(all$upload.month)), selected = sort(unique(all$upload.month), decreasing = T)[2])
      ),
      
      # Show a plot of the generated distribution
      dashboardBody(
        fluidRow(column(12,plotOutput('comp_plot', height = '900px', width = '1100px'), align = 'center')
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  region.list <- reactive({all %>% filter(upload.month == input$month)})
  
  region.peeps <- reactive({region.list() %>% group_by(SAMPLING.EVENT.IDENTIFIER) %>% slice(1) %>% ungroup() %>% group_by(SUBNATIONAL1_CODE,FULL.NAME) %>% 
    summarise(no.lists = n_distinct(SAMPLING.EVENT.IDENTIFIER),
              no.hrs = round(sum(DURATION.MINUTES, na.rm=TRUE)/60, 1)) %>% arrange(desc(no.lists))})
  
  region.stats <- reactive({region.list() %>% group_by(SUBNATIONAL1_CODE) %>% summarise(total.recs = n(), 
                                                                            total.lists = n_distinct(SAMPLING.EVENT.IDENTIFIER),
                                                                            total.peeps = n_distinct(FULL.NAME),
                                                                            total.sp = n_distinct(SPECIES.NAME[APPROVED == 1 & !CATEGORY %in% c('spuh','slash')]))
    })
  
  region.spp <- reactive({region.list() %>% filter(ALL.SPECIES.REPORTED == 1) %>% group_by(SUBNATIONAL1_CODE) %>% mutate(visits = n_distinct(group.id)) %>% group_by(SUBNATIONAL1_CODE,SPECIES.NAME) %>%
    summarise(perc.lists = round(100 * n_distinct(group.id)/max(visits), 0)) %>%
    arrange(desc(perc.lists)) %>% slice(1:10)})
  
  
  
  
  
   comp_plot_f <- function(){
     
     region.list.prev <- all %>% filter(upload.month < input$month, SUBNATIONAL1_CODE == input$state) %>% group_by(group.id) %>% select(LONGITUDE,LATITUDE) %>% slice(1)
     
     region.list.loc <- region.list() %>% filter(SUBNATIONAL1_CODE == input$state) %>% group_by(group.id) %>% select(LONGITUDE,LATITUDE) %>% slice(1)
     
     validate(
       need(is.na(region.list.loc) == F, "No eBirding happened in the selected state on this month")
     )
     
     disp.month = paste(month.name[as.numeric(strsplit(input$month, split = '-')[[1]][2])],strsplit(input$month, split = '-')[[1]][1], sep = ' ')
     
     stplot <- ggplot() +
       geom_polygon(data = fortify(india.red[india.red$ST_NM == st_codes$state[st_codes$SUBNATIONAL1_CODE == input$state],]), aes(x=long, y=lat, group=group), fill = rgb(171/255, 215/255, 230/255), col = 'black') +
       geom_point(data = region.list.prev, aes(jitter(LONGITUDE),jitter(LATITUDE), color = 'Earlier months', fill = 'Earlier months'), shape = 21, alpha = 0.1, size = 3)+
       geom_point(data = region.list.loc, aes(jitter(LONGITUDE),jitter(LATITUDE), color = 'This month', fill = 'This month'), shape = 21, alpha = 0.4, size = 3)+
       scale_color_manual(name="", values = c(rgb(0.5, 0.5, 0.5, 0.1),"black")) +
       scale_fill_manual(name="", values=c(rgb(0.5, 0.5, 0.5, 0.1),rgb(199/255, 0, 0, 53/255)))+
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
             plot.margin=unit(c(0,0,0,0)+2, "cm"),
             panel.border = element_blank(),
             panel.background = element_blank(),
             legend.text = element_text(size = 15),
             legend.position = 'bottom')+
       coord_equal()
     
     

     pp <- region.peeps() %>% filter(SUBNATIONAL1_CODE == input$state) %>% slice(1:15) %>% ungroup %>% select(-SUBNATIONAL1_CODE)
     colnames(pp) <- c('Name','Lists','Hours')
     pp$Name <- strtrim(pp$Name,30)
       
     region.spp.st <- region.spp() %>% filter(SUBNATIONAL1_CODE == input$state) %>% arrange(perc.lists)
     region.spp.st$Species <- do.call("rbind", strsplit(region.spp.st$SPECIES.NAME, " | ", fixed=TRUE))[,1]
     
     
     cumlt <- all %>% filter(upload.month <= input$month, SUBNATIONAL1_CODE == input$state) %>% group_by(upload.month) %>% summarise(nlists = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% mutate(cumlists = cumsum(nlists)) %>% filter(upload.month > '2013-12', upload.month <= input$month)
     

     plot.new()
     grid.newpage()
     vpstat <- viewport(x= 0, y =1, height = .1, width = 1, just = c('left','top'))
     vp0 <- viewport(x = 0, y = .9, height = .1,width = 0.48, just = c('left','top'))
     vp1 <- viewport(x = 0, y = .83, height = .83,width = 0.48, just = c('left','top'))
     vp2 <- viewport(x = 0.55, y = .43, height = 0.4,width = 0.4, just = c('left','bottom'))
     vp3 <- viewport(x = 0.68, y = 0.4, height = 0.4,width = 0.29, just = c('right','top'))
     vp4 <- viewport(x = 0.68, y = 0.4, height = 0.4,width = 0.30, just = c('left','top'))
     
     
     
     pushViewport(vpstat)
     grid.text(region.stats()$total.lists[region.stats()$SUBNATIONAL1_CODE == input$state], x = unit(.125, 'npc'), y = unit(.5,'npc'), gp=gpar(fontsize = 35, col = 'red4', fontface = 'bold'))
     grid.text('Lists', x = unit(.125, 'npc'), y = unit(.2,'npc'), gp=gpar(fontsize = 20, col = 'black'))
     grid.text(region.stats()$total.recs[region.stats()$SUBNATIONAL1_CODE == input$state], x = unit(.375, 'npc'), y = unit(.5,'npc'), gp=gpar(fontsize = 35, col = 'red4', fontface = 'bold'))
     grid.text('Observations', x = unit(.375, 'npc'), y = unit(.2,'npc'), gp=gpar(fontsize = 20, col = 'black'))
     grid.text(region.stats()$total.peeps[region.stats()$SUBNATIONAL1_CODE == input$state], x = unit(.625, 'npc'), y = unit(.5,'npc'), gp=gpar(fontsize = 35, col = 'red4', fontface = 'bold'))
     grid.text('eBirders', x = unit(.625, 'npc'), y = unit(.2,'npc'), gp=gpar(fontsize = 20, col = 'black'))
     grid.text(region.stats()$total.sp[region.stats()$SUBNATIONAL1_CODE == input$state], x = unit(.875, 'npc'), y = unit(.5,'npc'), gp=gpar(fontsize = 35, col = 'red4', fontface = 'bold'))
     grid.text('Species', x = unit(.875, 'npc'), y = unit(.2,'npc'), gp=gpar(fontsize = 20, col = 'black'))
     upViewport()
     pushViewport(vp1)
     par(new = NA, oma = c(0,0,0,0), xpd = T, mar = c(2,0,0,0))
     print(stplot, newpage = F)
     upViewport()
     pushViewport(vp0)
     grid.text(st_codes$state[st_codes$SUBNATIONAL1_CODE == input$state][1],x = unit(.6,'npc'),y = unit(.4, "npc"), gp=gpar(fontsize = 35, col = 'red4', fontface = 'bold'))
     grid.text(paste('eBirding in',disp.month, sep = ' '),x = unit(.6,'npc') ,y = unit(0, "npc"), gp=gpar(fontsize = 25))
     upViewport()
     pushViewport(vp2)
     par(new = TRUE, xpd = T, oma = c(1,1,1,1), mar = c(3,3,3,3), fig = gridFIG())
     plot((cumlt$cumlists/1000)~as.Date(paste(cumlt$upload.month,'01',sep = '-')), col="red4", typ = 'l', ylab = 'No of Lists x 1000', xlab = NA, bty = 'n', lwd = 3, xlim = c(as.Date('2014-01-01'),as.Date('2018-01-01')), ylim = c(0,round(max(cumlt$cumlists)/1000*1.1,0)), mgp = c(2,.5,0), cex.lab = 1.3, font.lab = 2)
     grid.text('Lists in State',y=unit(.9, "npc"), gp=gpar(fontsize = 18, fontface = 'bold', col = 'red4'))
     upViewport()
     pushViewport(vp3)
     par(new = TRUE, oma = c(0,0,0,0), xpd = T, mar = c(1,3,1,1))
     grid.draw(tableGrob(pp,rows = NULL, theme = ttheme_minimal(core = list(fg_params = list(hjust=1, x=1)),
                                                    colhead = list(fg_params = list(hjust=1, x=1, 
                                                                                    fontface="bold"))
     )))
     grid.text('Top Birders', x = unit(0,'npc') + unit(2,'lines'), y = unit(.35,'npc'),rot = 90, gp=gpar(fontsize = 18, fontface = 'bold', col = 'red4'))
     upViewport()
     pushViewport(vp4)
     par(new = TRUE, xpd = T, fig = gridFIG(), mar = c(3,0,0,0)+.5, oma = c(0,0,0,2))
     region.temp <- with(region.spp.st, barplot(perc.lists, horiz=TRUE, xlab="Percentage of complete lists", col="lightblue", mgp = c(1,.5,-.5), cex.lab = 1.2, font.lab = 2))
     text(0, region.temp[,1], labels = region.spp.st$Species, pos=4, cex=0.8)
     grid.text('Most Common Species', x = unit(1,'npc')- unit(1.7,'lines'), y = unit(.48,'npc'),rot = 90, gp=gpar(fontsize = 18, fontface = 'bold', col = 'red4'))
     upViewport()
   }
   
   output$comp_plot <- renderPlot({
     comp_plot_f()
   })
 
}

# Run the application 
shinyApp(ui = ui, server = server)