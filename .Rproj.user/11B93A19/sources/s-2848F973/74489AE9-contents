

library(shinydashboard)
library(readr)
library(magrittr)
library(lazytrade)
library(lubridate)
library(dplyr)
library(ggplot2)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
    navbarPage("EA MANAGEMENT",
        tabPanel("DATA",
                 sidebarLayout(
                     sidebarPanel(fluidRow(
                                            selectInput(inputId = "Terminal", label = "Select the terminal Number",choices = 1:5),
                                  column(width = 12,fluidRow(
                                    column(4,actionButton(inputId = "Refresh", label = "Refresh")),
                                    column(8,helpText("Refresh the symbol and magic number selection")))),
                                  column(width = 12,fluidRow( 
                                    column(6, selectInput(inputId = "MagicNum", label = "Select Magic Number", choices = 1:10)),
                                    column(6, selectInput(inputId = "Symbol", label = "Select the symbol",choices = 1:10)))),
                                  column(width = 12,fluidRow(
                                    column(6,dateInput(inputId = "From", label = "From", value = Sys.Date()-30)),
                                    column(6,dateInput(inputId = "To", label = "To", value = Sys.Date())))),
                                 radioButtons(inputId = "Time",label = "Select the type of time", choices = c("Entry Time" , "Exit Time"),selected = "Exit Time")
                     )),
                     mainPanel(
                         tabsetPanel(type = "pills",
                                     tabPanel("Console",verbatimTextOutput("print")),
                                     tabPanel("Data",tableOutput("data")))
                      )
                 ))
      

    )
)

# Define server logic required to draw a histogram




server <- function(input, output, session) {

  
  
    file_path <- reactive({
        Terminals <- data.frame(id = 1:4, TermPath = c("C:/Program Files (x86)/AM MT4 - Terminal 1/MQL4/Files/",
                                                       "C:/Program Files (x86)/AM MT4 - Terminal 2/MQL4/Files/",
                                                       "C:/Program Files (x86)/AM MT4 - Terminal 3/MQL4/Files/",
                                                       "C:/Program Files (x86)/AM MT4 - Terminal 4/MQL4/Files/"),
                                stringsAsFactors = F)
        
        paste0(Terminals[input$Terminal,2],"OrdersResultsT",input$Terminal,".csv")
      # file_path <- paste0(Terminals[2,2],"OrdersResultsT",2,".csv")
    })
    

    
  DF_Stats <- reactive({ 
     DF_Stats <- read.csv(file_path(), col.names = c("MagicNumber","Ticket","EntryTime","ExitTime","Profit","Symbol","Type"))
    DF_Stats <- data.frame(MagicNumber = DF_Stats$MagicNumber,
                          Ticket = DF_Stats$Ticket,
                          EntryTime = as.character(as.POSIXct(DF_Stats$EntryTime, format = "%Y.%m.%d %H:%M:%S", tz = "Africa/Cairo")),
                          ExitTime = as.character(as.POSIXct(DF_Stats$ExitTime, format = "%Y.%m.%d %H:%M:%S", tz = "Africa/Cairo")),
                          Profit = DF_Stats$Profit,
                          Symbol = DF_Stats$Symbol,
                          Type = DF_Stats$Type)
 })
    
    magicNumber <- reactive({
       unique(DF_Stats()$MagicNumber)
    })
    
    symbol <- reactive({
        unique(DF_Stats()$Symbol)
    })
    output$print <- renderPrint({
         str(DF_Stats())
    })
    
    output$data <- renderTable({
        if(input$MagicNum == "All"){
        
        DF_Stats()%>%filter(EntryTime >= input$From, ExitTime <= input$To)
      }
     else{
        if(input$Time == "Entry Time"){
          DF_Stats()%>%filter(MagicNumber == input$MagicNum, EntryTime >= input$From, EntryTime <= input$To)
        }else{
          DF_Stats()%>%filter(MagicNumber == input$MagicNum, ExitTime >= input$From, ExitTime <= input$To)
        }
     }
     
    })
    
    observeEvent(input$Refresh,{
        updateSelectInput(session, inputId = "MagicNum", label = NULL, choices = c("All",magicNumber()), selected = NULL)
      updateSelectInput(session, inputId = "Symbol", label = NULL, choices = c("All",symbol()), selected = NULL)
    })
    
    observeEvent(input$MagicNum,{
      DF_Stats <- read.csv(file_path(), header = FALSE, sep = ",", col.names = c("MagicNumber","Ticket","EntryTime","ExitTime","Profit","Symbol","Type"))
      
       if(input$MagicNum == "All"){
          updateSelectInput(session, "Symbol",label = "Select the symbol",choices = c("All",symbol()),selected = "All")
      }else{
        pair <- DF_Stats%>%group_by(Symbol)%>%filter(MagicNumber == input$MagicNum)%>%select(Symbol)%>%unique()
        updateSelectInput(session, "Symbol",label = "Select the symbol",choices = as.character(pair),selected = pair)
      }
    })
   
    observeEvent(input$Symbol,{
      x <- input$Symbol
      DF_Stats <- read.csv(file_path(), header = FALSE, sep = ",", col.names = c("MagicNumber","Ticket","EntryTime","ExitTime","Profit","Symbol","Type"))
      
      if(input$Symbol == "All" || input$Symbol == 1)
      {
            updateSelectInput(session, "MagicNum",label = "Select Magic Number",choices = c("All",magicNumber()),selected = "All")
      }else
      {
            MN <- DF_Stats%>%group_by(MagicNumber)%>%filter(Symbol==input$Symbol)%>%select(MagicNumber)%>%unique()
            updateSelectInput(session, "MagicNum",label = "Select Magic Number",choices =  c(as.integer(unlist(MN))) ,selected = as.integer(unlist(MN)[1]))
      }
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
