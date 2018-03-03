library(shiny)

shinyUI(fluidPage(
  singleton(tags$head(HTML(
    '
    <script type="text/javascript">
    $(document).ready(function() {
    // disable download at startup. data_file is the id of the downloadButton
    $("#data_file").attr("disabled", "true").attr("onclick", "return false;");
    
    Shiny.addCustomMessageHandler("download_ready", function(message) {
    $("#data_file").removeAttr("disabled").removeAttr("onclick").html(
    "<i class=\\"fa fa-download\\"></i>Download " + message.fileSize + " ");
    });
    })
    </script>
    '
  ))),
  tabsetPanel(
    tabPanel('Data download example',
             actionButton("start_proc", h5("Click to start processing data")),
             hr(),
             
             downloadButton("data_file"),
             helpText("Download will be available once the processing is completed.")
    )
  )
  ))
Server UI.R

library(shiny)

get_a_pdf_plot <- function(my_i){
  pdf(paste("plot_", my_i,'.pdf', sep=""))
  plot(1:my_i*5, 1:my_i*5,
       xlim = c(1, my_i*5),
       ylim = c(1, my_i*5),
       main = paste("1:", my_i, sep = ""))
  dev.off()
}


shinyServer(function(input, output, session) {
  
  observe({
    if (input$start_proc > 0) {
      Sys.sleep(2)
      session$sendCustomMessage("download_ready", list(fileSize= "Ready"))
    }
  })
  
  output$data_file <- downloadHandler(
    filename = 'pdfs.zip',
    content = function(fname) {
      fs <- c()
      tmpdir <- tempdir()
      setwd(tempdir())
      print (tempdir())
      
      for (i in c(1,2,3,4,5)) {
        path <- paste("plot_", i, ".pdf", sep="")
        fs <- c(fs, path)
        get_a_pdf_plot(i)
      }
      print (fs)
      zip(zipfile="pdfs.zip", files=fs)
    }
  )
})