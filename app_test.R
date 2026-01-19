packages.list <- c("shiny", "shinyTime", "tidyverse", "DT", "magrittr", "reactable")
packages.to.install <- packages.list[!(packages.list %in% installed.packages()[,"Package"])]
if(length(packages.to.install)) install.packages(packages.to.install)

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(magrittr)
library(reactable)

#("staff_input.R")

# Create UI

# Application title

ui <- fluidPage(

div(style="margin:10px", img(src="arc_logo.png", height=65, width=200),
titlePanel("Blood donation time input")),

# Sidebar with a slider input for number of bins 
            selectInput("donation_center", "Select a donation center",
                                              choices=c("rockville_md",
                                                        "None"="None"), selected="None"),
            mainPanel(
              DTOutput("wait_time"))

)

server <- function(input, output) {

  center_csv <- reactive({
    req(input$donation_center)
    validate(
      need(input$donation_center != "None",
           "Please select a donation center.")
    )
    
    path <- file.path(".", paste0("arc_donation_time_tracker_", input$donation_center, ".csv"))
    
    validate(
      need(file.exists(path),
           paste("File not found:", basename(path)))
    )
    
    read.csv(path, stringsAsFactors = FALSE)
  })
  
  read_table <- function(path) {
    if (file.exists("./", paste0("arc_donation_time_tracker_", input$donation_center, ".csv"))) {
      read.csv(path, stringsAsFactors = FALSE)
    } 
  }
  

  output$wait_time <- renderDT({
    datatable(center_csv(), rownames = FALSE)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
