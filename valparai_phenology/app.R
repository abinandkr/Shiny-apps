library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:/Users/Abinand Reddy/Desktop/Thesis/NCF/LTDW workshop")
dat <- read.csv("Tree Phenology_Monthly Data_FINAL.csv")
dat$Date <- as.Date(dat$Date,'%d-%B-%Y')
dat <- dat %>% filter(!is.na(Date))
dat$yrmo <- format(dat$Date, '%Y-%m')
dat1 <- dat %>% select(TreeNo,Trail,Species,Leaf_Flush,Flower_Buds,Flower_Open,Fruit_Unripe,Fruit_Ripe,yrmo)
dat1 <- gather(dat1, 'phenophase', 'score', c(4:8))
dat1$TreeNo <- as.character(dat1$TreeNo)
dat1$score[dat1$score == ''] <- NA
dat1$score[dat1$score == 'X'] <- NA
dat1$score[dat1$score == 'dead'] <- NA
dat1$score <- as.numeric(dat1$score)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Valparai Phenology"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("species","Select Species",choices = unique(dat1$Species), selected = unique(dat1$Species)[1]),
         sliderInput('score','Select min score', min = 1, max = 4, step = 1,value = 1)
         ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dat2 <- reactive({
    dat2 <- dat1 %>% group_by(yrmo,Species,phenophase) %>% summarise(nind = n(), nind_p = sum(score >= input$score, na.rm = T), prop = round(nind_p/nind*100,2))
  })
  
   output$distPlot <- renderPlot({
     dat3 <- dat2() %>% filter(Species == input$species)
     
     dat3$Date <- as.Date(paste(dat3$yrmo,'01',sep = '-'))
     
     g <- ggplot() + geom_line(data = dat3, aes(y = prop, x = Date)) + facet_grid(phenophase~.) + theme_bw()
     
     g
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

