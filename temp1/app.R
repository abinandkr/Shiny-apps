rm(list = ls())
library(DT)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinydashboard)

# This function will create the buttons for the datatable, they will be unique
shinyInput <- function(FUN, len, id, ...) {inputs <- character(len)
for (i in seq_len(len)) {
  inputs[i] <- as.character(FUN(paste0(id, i), ...))}
inputs
}

ui <- dashboardPage(
  dashboardHeader(title = "Simple App"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Menu Item 1", tabName = "one", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "one",h2("Datatable Modal Popup"),
              DT::dataTableOutput('my_table'),uiOutput("popup")
      )
    )
  )
)


server <- function(input, output, session) {
  my_data <- reactive({
    testdata <- mtcars
    as.data.frame(cbind(View = shinyInput(actionButton, nrow(testdata),'button_', label = "View", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ),testdata))
  })  
  output$my_table <- DT::renderDataTable(my_data(),selection = 'single',options = list(searching = FALSE,pageLength = 10),server = FALSE, escape = FALSE,rownames= FALSE)
  
  # Here I created a reactive to save which row was clicked which can be stored for further analysis
  SelectedRow <- eventReactive(input$select_button,{
    as.numeric(strsplit(input$select_button, "_")[[1]][2])
  })
  
  # This is needed so that the button is clicked once for modal to show, a bug reported here
  # https://github.com/ebailey78/shinyBS/issues/57
  observeEvent(input$select_button, {
    toggleModal(session, "modalExample", "open")
  })
  
  DataRow <- eventReactive(input$select_button,{
    my_data()[SelectedRow(),2:ncol(my_data())]
  })
  
  
  
  output$popup <- renderUI({
    bsModal("modalExample", paste0("Data for Row Number: ",SelectedRow()), "", size = "large",
            column(12,                   
                   DT::renderDataTable(DataRow())
                   
            )
    )
  })
  
}

shinyApp(ui, server)