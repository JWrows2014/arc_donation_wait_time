packages.list <- c("shiny", "shinyTime", "tidyverse", "DT", "magrittr", "reactable")
packages.to.install <- packages.list[!(packages.list %in% installed.packages()[,"Package"])]
if(length(packages.to.install)) install.packages(packages.to.install)

library(shiny)
library(shinyTime)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(magrittr)
library(reactable)

#source("staff_input.R")

donation_wait_time <- tibble::tibble(
  Machine.name  = c("WB1","WB2","WB3","Platelet1","Platelet2","PR1","PR2","PR3"),
  Donation.type = c("WB","WB","WB","Platelet","Platelet","PR","PR","PR"),
  Start.time    = as.POSIXct(NA),
  Finish.time   = as.POSIXct(NA)
)

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
                           shinyWidgets::timeInput("start_time", "Start time"
                                    ),
                           actionButton(inputId = "submit",label = "Submit start time"))),        
                       
              mainPanel(
                #reactableOutput("wait_time")
                DTOutput("wait_time")), #1/17/2025

            actionButton(inputId = "reset",label = "Clear start and finish times")
              
)


server <- function(input, output) {
  started <- reactive({
    req(started)
    as.POSIXct(paste(Sys.Date(), input$start_time), tz = "")
  })
  dwt <- reactiveVal(donation_wait_time)
  observeEvent(input$submit,
               {st <- started()
                 dwt(dwt() %>% 
                    mutate(Start.time=if_else(Machine.name==input$machine_name, started(), Start.time),
                    Finish.time = case_when(
                             Machine.name == input$machine_name & Donation.type == "Platelet" ~ st + 7200,
                             Machine.name == input$machine_name & Donation.type == "WB"       ~ st + 1800,
                             Machine.name == input$machine_name & Donation.type == "PR"       ~ st + 3600,
                             TRUE ~ Finish.time)
                    #Finish.time=case_when(Donation.type=="Platelet" ~ started()+7200, TRUE ~ Finish.time))
                    #%>% mutate(Finish.time=case_when(Donation.type=="WB" ~ started()+1800, TRUE ~ Finish.time))
                    #%>% mutate(Finish.time=case_when(Donation.type=="PR" ~ started()+3600, TRUE ~ Finish.time))
                    #donation_wait_time <- dwt()
                    #write.csv(donation_wait_time,file="./arc_donation_time_tracker.csv",quote=FALSE,row.names=FALSE)
                 ))})
  #observeEvent(input$submit,
               #{dwt(dwt() %>% 
                    #mutate(Finish.time=case_when(Donation.type=="Platelet" ~ strftime(strptime(Start.time, format="%r")+7200, format="%r"), TRUE ~ Finish.time))
                    #%>% mutate(Finish.time=case_when(Donation.type=="WB" ~ strftime(strptime(Start.time, format="%r")+1800, format="%r"), TRUE ~ Finish.time))
                    #%>% mutate(Finish.time=case_when(Donation.type=="PR" ~ strftime(strptime(Start.time, format="%r")+3600, format="%r"), TRUE ~ Finish.time)))
                   #donation_wait_time <- dwt()
                   #write.csv(donation_wait_time,file="./arc_donation_time_tracker.csv",quote=FALSE,row.names=FALSE)
                 #}) 
  observeEvent(input$reset,
                  {dwt(dwt() %>% 
                    mutate(Start.time=as.POSIXct(NA)) %>%
                    mutate(Finish.time=as.POSIXct(NA)))
                    #donation_wait_time <- dwt()
                    #write.csv(donation_wait_time,file="./arc_donation_time_tracker.csv",quote=FALSE,row.names=FALSE)
                 }) 
  
  #output$wait_time <- renderReactable({
    #reactable(
      #dwt()
  output$wait_time <- renderDT({
    datatable(
      dwt() %>%
        mutate(
          Start.time  = if_else(is.na(Start.time), "", format(Start.time, "%I:%M %p")),
          Finish.time = if_else(is.na(Finish.time), "", format(Finish.time, "%I:%M %p"))
        ),
      rownames = FALSE
    )
  })
    #)


  
}

# Run the application 
shinyApp(ui = ui, server = server)
