

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
        tabPanel("RESULT",
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
                                  column(width = 12,fluidRow(
                                    column(6,radioButtons(inputId = "Time",label = "Select the type of time", choices = c("Entry Time" , "Exit Time"),selected = "Exit Time")),
                                    column(6,selectInput(inputId = "Sort", label = "Sort data by", choices = c("MagicNumber","Ticket","EntryTime", "ExitTime","Profit")))))
                     )),
                     mainPanel(
                         tabsetPanel(type = "pills",
                                     tabPanel("Console",verbatimTextOutput("console")),
                                     tabPanel("Data",
                                              tabsetPanel(
                                                tabPanel("Data",tableOutput("data")),
                                                tabPanel("Balance",tableOutput("balance")))),
                                     tabPanel("Graph",
                                              tabsetPanel(
                                                tabPanel("Profit",plotOutput("profitGraph")),
                                                tabPanel("Balance", plotOutput("balanceGraph")))),
                                     tabPanel( 
                                       "Report",br(),
                                                       column(width = 12,fluidRow(
                                         column(3,strong("Total Trades :", style = "text-decoration: underline;")),
                                         column(3,textOutput("totalTrade")))),
                                                      column(width = 12,fluidRow(
                                          column(3,strong("Final Balance :", style = "text-decoration: underline;")),
                                          column(3,textOutput("finalBalance")))),
                                                      column(width = 12, fluidRow(
                                          column(3,strong("Profit Factor :", style = "text-decoration: underline;")),
                                          column(3,textOutput("profitFactor")))),
                                       column(width = 12, fluidRow(
                                         column(3,strong("Maximum Profit :", style = "text-decoration: underline;")),
                                         column(3,textOutput("maxProfit")))),
                                       column(width = 12, fluidRow(
                                         column(3,strong("Minimum Profit :", style = "text-decoration: underline;")),
                                         column(3,textOutput("minProfit"))))
                                                      )))))))

########################################### END OF UI #################################################
#######################################################################################################
########################################## START SERVER ###############################################

server <- function(input, output, session) {

  
 #-----------DATA MANAGEMENT-------------- 
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


    
#-----------MANAGE SIDEBAR-------------    
    #Refresh data 
    observeEvent(input$Refresh,{
        updateSelectInput(session, inputId = "MagicNum", label = NULL, choices = c("All",magicNumber()), selected = NULL)
        updateSelectInput(session, inputId = "Symbol", label = NULL, choices = c("All",symbol()), selected = NULL)
    })
    observeEvent(input$Terminal,{
      updateSelectInput(session, inputId = "MagicNum", label = NULL, choices = c("All",magicNumber()), selected = NULL)
      updateSelectInput(session, inputId = "Symbol", label = NULL, choices = c("All",symbol()), selected = NULL)
    }) 
    
    #update Symbol choices
    observeEvent(input$MagicNum,{
      if(input$MagicNum == "All"){
          updateSelectInput(session, "Symbol",label = "Select the symbol",choices = c("All",symbol()),selected = "All")
      }else{
        pair <- DF_Stats()%>%group_by(Symbol)%>%filter(MagicNumber == input$MagicNum)%>%select(Symbol)%>%unique()
        updateSelectInput(session, "Symbol",label = "Select the symbol",choices = as.character(pair),selected = pair)
      }
    })
   #update Magic Number
    observeEvent(input$Symbol,{
      if(input$Symbol == "All" || input$Symbol == 1)
      {
            updateSelectInput(session, "MagicNum",label = "Select Magic Number",choices = c("All",magicNumber()),selected = "All")
      }else
      {
            MN <- DF_Stats()%>%group_by(MagicNumber)%>%filter(Symbol==input$Symbol)%>%select(MagicNumber)%>%unique()
            updateSelectInput(session, "MagicNum",label = "Select Magic Number",choices =  c(as.integer(unlist(MN))) ,selected = as.integer(unlist(MN)[1]))
      }
    })
    
    #---------DATA TAB---------------  
    Stats <- reactive({
      if(input$MagicNum == "All"){
        DF_Stats <- DF_Stats()%>%filter(EntryTime >= input$From, ExitTime <= input$To)
      }
      else{
        if(input$Time == "Entry Time"){
          DF_Stats <- DF_Stats()%>%filter(MagicNumber == input$MagicNum, EntryTime >= input$From, EntryTime <= input$To)
        }else{
          DF_Stats <- DF_Stats()%>%filter(MagicNumber == input$MagicNum, ExitTime >= input$From, ExitTime <= input$To)
        }
      }
    })
    
   output$data <- renderTable({
      Stats()
      switch(input$Sort,
           "MagicNumber" =  Stats()[order(Stats()$MagicNumber,decreasing = T),],
           "Ticket" =  Stats()[order(Stats()$Ticket,decreasing = T),],
           "EntryTime" =  Stats()[order(Stats()$EntryTime,decreasing = T),],
           "ExitTime" =  Stats()[order(Stats()$ExitTime,decreasing = T),],
           "Profit"=  Stats()[order(Stats()$Profit,decreasing = T),])
   })
    
    
    DF_Balance <- reactive({
      Balance <- c()
     
      for(i in  1:nrow(Stats())){
        if (i==1){
          Balance[i] <- Stats()$Profit[i]
        }
        else{
          Balance[i] <- Stats()$Profit[i]+ Balance[i-1]
        }
      }

      DF_Balance <- Stats() %>% subset(select = -c(MagicNumber,Ticket,EntryTime,Type))
      cbind(DF_Balance,Balance)
    })
    
    
    output$balance <- renderTable({
      if(nrow(Stats())>0){DF_Balance()}
      else{"NO DATA"}
    })
   
#----------GRAPH TAB-----------------
  output$profitGraph <- renderPlot({
    if(nrow(Stats())>0){
        graph <- ggplot(Stats(), aes(x=ExitTime, y=Profit)) +  geom_bar(stat = "identity" )
        graph + theme(axis.text.x = element_text(angle =  45))  + ggtitle(paste0(input$Symbol," PROFIT"))
    }
  })
    
  
  output$balanceGraph <- renderPlot({
  
    if(nrow(Stats())>0){
    graph <- ggplot(DF_Balance(), aes(x = ExitTime)) + 
             geom_line(aes(y = Balance), group = 1) +
             geom_line(aes(y = 0), group = 1,color = "red", size = 1)
    graph + theme(axis.text.x = element_text(angle =  45))  + ggtitle(paste0(input$Symbol," BALANCE"))
    }
  })  
  
  
  
  #-------REPORT TAB-----------------
    

  output$finalBalance <- renderText({
    if(nrow(Stats()>0)){round(DF_Balance()[nrow(DF_Balance()),4],2)}
    else{"NO DATA"}
  })
 
  output$profitFactor <- renderText({
      if(nrow(Stats()>0)){
         negProfit <- Stats()%>%filter(Profit<0)%>%select(Profit)%>%sum()
         posProfit <- Stats()%>%filter(Profit>0)%>%select(Profit)%>%sum()
         round(abs(posProfit/negProfit),2)
      }else{
        "NO DATA"
      }
  })
    
  output$maxProfit <- renderText({
    if(nrow(Stats())>0){max(Stats()$Profit)}
    else{"NO DATA"}
  })
  
  output$minProfit <- renderText({
      if(nrow(Stats())>0){min(Stats()$Profit)}
      else{"NO DATA"}
  })
  
  output$totalTrade <- renderText({
    nrow(Stats())
  })
 
  #max consecutive win/loss
  
    
#---------------END CODE------------------------------------------
    output$console <- renderPrint({
        nrow(Stats())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
