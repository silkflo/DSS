

library(shinydashboard)
library(readr)
library(magrittr)
library(lazytrade)
library(lubridate)
library(dplyr)
#library(ggplot2)
library(DT)
library(plotly)
library(randomcoloR)
#library(hrbrthemes)
#source("C:/DSS/Function/All_Path.R")

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
                                    column(6,selectInput(inputId = "Sort", label = "Sort data by", choices = c("MagicNumber","Ticket","EntryTime", "ExitTime","Profit","Symbol")))))
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
                                               tabPanel("Profit",plotlyOutput("profitGraph")),
                                                tabPanel("Balance", plotlyOutput("balanceGraph")))),
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
        Terminals <- data.frame(id = 1:5, TermPath = c("C:/DSS/_DATA/",
                                                       "C:/DSS/_DATA/",
                                                       "C:/DSS/_DATA/",
                                                       "C:/DSS/_DATA/",
                                                       "C:/DSS/_DATA/"),
                                stringsAsFactors = F)
        
        paste0(Terminals[input$Terminal,2],"OrdersResultsT",input$Terminal,".csv")
      # file_path <- paste0(Terminals[2,2],"OrdersResultsT",2,".csv")
    })
#-----------------------------------------
    DF_Stats <- reactive({ 
       DF_Stats <- read.csv(file_path(), col.names = c("MagicNumber","Ticket","EntryTime","ExitTime","Profit","Symbol","Type"))
   
              DF_Stats <- data.frame(MagicNumber = DF_Stats$MagicNumber,
                            Ticket = DF_Stats$Ticket,
                            EntryTime = as.POSIXct(DF_Stats$EntryTime, format = "%Y.%m.%d %H:%M:%S", tz = "Africa/Cairo"),
                            ExitTime = as.POSIXct(DF_Stats$ExitTime, format = "%Y.%m.%d %H:%M:%S", tz = "Africa/Cairo"),
                            Profit = DF_Stats$Profit,
                            Symbol = DF_Stats$Symbol,
                            Type = DF_Stats$Type)
    })
#---------------------------------------      
    magicNumber <- reactive({
      unique(DF_Stats()$MagicNumber)
    })
#---------------------------------------    
    symbol <- reactive({
       unique(DF_Stats()$Symbol)
    })
#---------------------------------------
    DF_Balance <- reactive({
      
      Balance <- c()
      DF_Balance <- Stats()
      #mutate(DF_Balance,ExitTime =  as.POSIXct(DF_Balance$ExitTime, format = "%Y-%m-%d %H:%M:%S", tz = "Africa/Cairo"))
      DF_Balance <- DF_Balance %>% subset(select = -c(MagicNumber,Ticket,EntryTime,Type)) 
      DF_Balance <- DF_Balance[order(DF_Balance$Symbol,decreasing = F),]
      #group_by(DF_Balance,Stats()$Symbol)
      #pair <- as.vector(unique(Stats()$Symbol))
      
                    for(i in  1:nrow(DF_Balance)){
                if (i==1){
                    Balance[i] <- DF_Balance$Profit[i]
                }else if(DF_Balance$Symbol[i] != DF_Balance$Symbol[i-1] && i>1){
                     Balance[i] <- DF_Balance$Profit[i]
                }else{
                  Balance[i] <- DF_Balance$Profit[i]+ Balance[i-1]
                }
              }
    
      DF_Balance <- DF_Balance %>%  mutate(Balance) 
     # DF_Balance[order(DF_Balance$Symbol,decreasing = F),]
      #  DF_Balance[order(DF_Balance$ExitTime,decreasing = F),]
      
      DF_Balance
    })
#---------------------------------------    
    
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
     Stats <-  data.frame(MagicNumber = Stats()$MagicNumber,
                          Ticket = Stats()$Ticket,
                          EntryTime = as.character(Stats()$EntryTime),
                          ExitTime = as.character(Stats()$ExitTime),
                          Profit = Stats()$Profit,
                          Symbol = Stats()$Symbol,
                          Type = Stats()$Type)
     
     Stats
      switch(input$Sort,
           "MagicNumber" =  Stats[order(Stats$MagicNumber,decreasing = T),],
           "Ticket" =  Stats[order(Stats$Ticket,decreasing = T),],
           "EntryTime" =  Stats[order(Stats$EntryTime,decreasing = T),],
           "ExitTime" =  Stats[order(Stats$ExitTime,decreasing = T),],
           "Profit"=  Stats[order(Stats$Profit,decreasing = T),],
           "Symbol"=  Stats[order(Stats$Symbol,decreasing = T),])
   })
    
    output$balance <- renderTable({
      if(nrow(Stats())>0){
       
         DF_Balance <- data.frame(ExitTime = as.character(DF_Balance()$ExitTime),
                   Profit = DF_Balance()$Profit,
                   Symbol = DF_Balance()$Symbol,
                   Balance = DF_Balance()$Balance)
       
        "ExitTime" =  DF_Balance[order(DF_Balance$ExitTime,decreasing = F),]
       }
      else{"NO DATA"}
    })
   
#----------GRAPH TAB-----------------
  output$profitGraph <- renderPlotly({
    if(nrow(Stats())>0){
      
      pair <- as.vector(unique(Stats()$Symbol))
      color <- c("red", "black", "blue","green","orange","purple", "pink","cornflowerblue", "darkgreen","indianred3","magenta","mediumpurple3", "midnightblue","orchid4","palegreen","skyblue","slateblue4", "tomato1")
      colorList <- vector("list",length(pair))
      for (i in 1 : length(pair)){
        Ps <- list(target = pair[i], value = list(marker =list(color = sample(color,1))))
        colorList[[i]] <- Ps
      }
      
      if(input$MagicNum == "All"){
        
       graph <-  plot_ly(
          type = 'scatter',
          x = Stats()$ExitTime,
          y = Stats()$Profit,
          text = paste("<br>Time: ", Stats()$ExitTime,
                       "<br>Profit: ", Stats()$Profit,
                       "<br>Symbol: ", Stats()$Symbol),
          hoverinfo = 'text',
          mode = 'markers',
          transforms = list(
            list(
              type = 'groupby',
              groups = Stats()$Symbol,
              styles = colorList)))
       }else
        {
            plot_ly(
            x = Stats()$ExitTime,
            y = Stats()$Profit,
            type = "scatter",
            mode = 'markers',
            marker = list(color = sample(color,1)),
            name = paste0(input$Symbol," PROFIT"))
          }
    }
  })
    
  
  output$balanceGraph <- renderPlotly({
  
    if(nrow(Stats())>0){

    #  graph <- plot_ly(
    #        DF_Balance(), x = ~DF_Balance()$ExitTime,
    #          y = ~DF_Balance()$Balance,
    #          type = 'scatter',
    #          mode = 'lines',
    #          name = paste0(input$Symbol," BALANCE"))
     
      pair <- as.vector(unique(DF_Balance()$Symbol))
      color <- c("red", "black", "blue","green","orange","purple", "pink","cornflowerblue", "darkgreen","indianred3","magenta","mediumpurple3", "midnightblue","orchid4","palegreen","skyblue","slateblue4", "tomato1")
      colorList <- vector("list",length(pair))
      
      for (i in 1 : length(pair)){
        Ps <- list(target = pair[i], value = list(line =list(color = sample(color,1))))
        colorList[[i]] <- Ps
      }
      
      graph <-  plot_ly(
      type = 'scatter',
      x = DF_Balance()$ExitTime,
      y = DF_Balance()$Balance,
      text = paste("<br>Time: ", DF_Balance()$ExitTime,
                   "<br>Balance: ", DF_Balance()$Balance,
                   "<br>Symbol: ", DF_Balance()$Symbol),
      hoverinfo = 'text',
      mode = 'lines',
      
      transforms = list(
        list(
          type = 'groupby',
          groups = DF_Balance()$Symbol,
          styles = colorList)))
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
       str(DF_Balance())
      print(DF_Balance())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
