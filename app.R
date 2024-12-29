packages.list <- c("shiny", "shinyTime", "tidyverse", "DT", "magrittr", "reactable")
packages.to.install <- packages.list[!(packages.list %in% installed.packages()[,"Package"])]
if(length(packages.to.install)) install.packages(packages.to.install)

library(shiny)
library(shinyTime)
library(tidyverse)
library(DT)
library(magrittr)
library(reactable)

source("staff_input.R")

# Create UI

# Application title

ui <- fluidPage(

div(style="margin:10px", img(src="arc_logo.png", height=65, width=200),
titlePanel("Blood donation wait time")),

# Sidebar with a slider input for number of bins 
              selectInput("machine_name", "Choose a machine",
                                              choices=c("Whole blood 1"="WB1",
                                                        "Whole blood 2"="WB2",
                                                        "Whole blood 3"="WB3",
                                                        "Platelet 1"="Platelet1",
                                                        "Platelet 2"="Platelet2",
                                                        "Power red 1"="PR1",
                                                        "Power red 2"="PR2",
                                                        "Power red 3"="PR3",
                                                        "None"="None"),selected="None"),
              fluidRow(div(style="margin:15px", 
                           timeInput("start_time", "Start time", second=FALSE),
                           actionButton(inputId = "submit",label = "Submit start time"))),        
                       
              mainPanel(
                reactableOutput("wait_time")),

            actionButton(inputId = "reset",label = "Clear start and finish times")
              
)


server <- function(input, output) {
  
  dwt <- reactiveVal(donation_wait_time)
  observeEvent(input$submit,
               {dwt(dwt() %>% 
                    mutate(Start.time=case_when(donation_wait_time$Machine.name==input$machine_name ~ strftime(input$start_time, "%H:%M"), TRUE ~ Start.time)))
                    donation_wait_time <<- dwt()
                    write.csv(donation_wait_time,file="./arc_donation_time_tracker.csv",quote=FALSE,row.names=FALSE)})
  observeEvent(input$submit,
               {dwt(dwt() %>% 
                    mutate(Finish.time=case_when(donation_wait_time$Donation.type=="Platelet" ~ substr(strptime(donation_wait_time$Start.time, format="%H:%M")+7200,12,20), TRUE ~ Finish.time))
                    %>% mutate(Finish.time=case_when(donation_wait_time$Donation.type=="WB" ~ substr(strptime(donation_wait_time$Start.time, format="%H:%M")+1800,12,20), TRUE ~ Finish.time))
                    %>% mutate(Finish.time=case_when(donation_wait_time$Donation.type=="PR" ~ substr(strptime(donation_wait_time$Start.time, format="%H:%M")+3600,12,20), TRUE ~ Finish.time)))
                   donation_wait_time <<- dwt()
                   write.csv(donation_wait_time,file="./arc_donation_time_tracker.csv",quote=FALSE,row.names=FALSE)}) 
  observeEvent(input$reset,
               {dwt(dwt() %>% 
                    mutate(Start.time="") %>%
                    mutate(Finish.time=""))
                    donation_wait_time <<- dwt()
                    write.csv(donation_wait_time,file="./arc_donation_time_tracker.csv",quote=FALSE,row.names=FALSE)}) 
  
  output$wait_time <- renderReactable({
    reactable(
      dwt()
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
