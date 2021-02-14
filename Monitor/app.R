

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
                                    column(6,dateInput(inputId = "From", label = "From", value = Sys.Date()-7)),
                                    column(6,dateInput(inputId = "To", label = "To", value = Sys.Date())))),
                                  column(width = 12,fluidRow(
                               #     column(6,radioButtons(inputId = "Time",label = "Select the type of time", choices = c("Entry Time" , "Exit Time"),selected = "Exit Time")),
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
                                         column(12,tableOutput("result")))),
                                                      column(width = 12,fluidRow(
                                         column(3,strong("Total Trades :", style = "text-decoration: underline;")),
                                         column(3,textOutput("totalTrade")))),
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
   pair <- reactive({
      as.vector(unique(DF_Balance()$Symbol))
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
        DF_Stats <- DF_Stats()%>%filter(ExitTime >= input$From, ExitTime <= input$To)
      }
      else{
        
          DF_Stats <- DF_Stats()%>%filter(MagicNumber == input$MagicNum, ExitTime >= input$From, ExitTime <= input$To)
       
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
   
   DF_Balance_All <- reactive({
     DF_Balance <- Stats()
     DF_Balance <- DF_Balance %>% subset(select = -c(MagicNumber,Ticket,EntryTime,Type)) 
     DF_Balance <- DF_Balance[order(DF_Balance$ExitTime,decreasing = F),]
     
     Balance <- c()
     for(i in  1:nrow(DF_Balance)){
       if (i==1){
         Balance[i] <- DF_Balance$Profit[i]
         
       }else{
         Balance[i] <- DF_Balance$Profit[i]+ Balance[i-1]
       }
     }
     
     DF_Balance <- DF_Balance %>%  mutate(Balance) 
     
     
   })
   
   
   
    output$balance <- renderTable({
      if(nrow(Stats())>0){
       
        if(input$MagicNum != "All"){
         DF_Balance <- data.frame(ExitTime = as.character(DF_Balance()$ExitTime),
                   Profit = DF_Balance()$Profit,
                   Symbol = DF_Balance()$Symbol,
                   Balance = DF_Balance()$Balance)
       
        "ExitTime" =  DF_Balance[order(DF_Balance$ExitTime,decreasing = F),]
        }else{
          
          DF_Balance <- data.frame(ExitTime = as.character(DF_Balance_All()$ExitTime),
                                   Profit = DF_Balance_All()$Profit,
                                   Symbol = DF_Balance_All()$Symbol,
                                   Balance = DF_Balance_All()$Balance)
        }
        
        }
      else{"NO DATA"}
    })
   
#----------GRAPH TAB-----------------
  output$profitGraph <- renderPlotly({
    if(nrow(Stats())>0){
      
     
      color <- c("red", "black", "blue","green","orange","purple", "pink","cornflowerblue", "darkgreen","indianred3","magenta","mediumpurple3", "midnightblue","orchid4","palegreen","skyblue","slateblue4", "tomato1")
      colorList <- vector("list",length(pair()))
      for (i in 1 : length(pair())){
        Ps <- list(target = pair()[i], value = list(marker =list(color = sample(color,1))))
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

      #pair <- as.vector(unique(DF_Balance()$Symbol))
      color <- c("red", "black", "blue","green","orange","purple", "pink","cornflowerblue", "darkgreen","indianred3","magenta","mediumpurple3", "midnightblue","orchid4","palegreen","skyblue","slateblue4", "tomato1")
      colorList <- vector("list",length(pair()))
      
      for (i in 1 : length(pair())){
        Ps <- list(target = pair()[i], value = list(line =list(color = sample(color,1))))
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
    buyProfit <- reactive({

      if(input$MagicNum == "All"){
       
       allProfit <-  Stats()  %>%
          group_by(Type) %>% 
          filter(Type == 0) %>%
          summarise( Profit = sum(Profit)) %>%
          select(-c(Type)) %>%
          mutate(Symbol = "ALL PAIR")
     
        
        buyProfit <- Stats()  %>%
          group_by(Symbol, Type) %>% 
          filter(Type == 0) %>% 
          summarise( Profit = sum(Profit)) %>% 
          subset(select = c(Symbol,Profit)) %>%
          rbind(allProfit)
        
      } else{
        buyProfit <- Stats()  %>%
          group_by(Symbol, Type) %>% 
          summarise( Profit = sum(Profit)) %>% 
          filter(Type == 0) %>% 
          subset(select = c(Symbol,Profit))
      }
    })
  
  
  sellProfit <- reactive({
    
    if(input$MagicNum == "All"){
      
      allProfit <-  Stats()  %>%
        group_by(Type) %>% 
       filter(Type == 1) %>%
        summarise( Profit = sum(Profit)) %>%
        select(-c(Type)) %>%
        mutate(Symbol = "ALL PAIR")
      
      sellProfit <- Stats()  %>%
        group_by(Symbol, Type) %>% 
        filter(Type == 1) %>% 
        summarise( Profit = sum(Profit)) %>% 
        subset(select = c(Symbol,Profit)) %>%
        rbind(allProfit)
      
    } else{
      sellProfit <- Stats()  %>%
        group_by(Symbol, Type) %>% 
        filter(Type == 1) %>% 
        summarise( Profit = sum(Profit)) %>% 
        subset(select = c(Symbol,Profit))
    }
  })
  
 buyTrade <- reactive({
   
   if(input$MagicNum == "All"){
     

     allTrade <-  Stats()  %>%
       group_by(Type) %>% 
       filter(Type == 0) %>%
       summarise( Buy_Trade = n()) %>%
       select(-c(Type)) %>%
       mutate(Symbol = "ALL PAIR")
     
     
     buyTrade <- Stats()  %>%
       group_by(Symbol, Type) %>% 
       filter(Type == 0) %>% 
       summarise( Buy_Trade = n()) %>% 
       subset(select = c(Symbol,Buy_Trade)) %>%
       rbind(allTrade)
     
   } else{
     buyTrade <- Stats()  %>%
       group_by(Symbol, Type) %>% 
       summarise(Buy_Trade = n()) %>% 
       filter(Type == 0) %>% 
       subset(select = c(Symbol,Buy_Trade))
   }
 })
 
 sellTrade <- reactive({
   
   if(input$MagicNum == "All"){
     
     
     allTrade <-  Stats()  %>%
       group_by(Type) %>% 
       filter(Type == 1) %>%
       summarise( Sell_Trade = n()) %>%
       select(-c(Type)) %>%
       mutate(Symbol = "ALL PAIR")
     
     
     sellTrade <- Stats()  %>%
       group_by(Symbol, Type) %>% 
       filter(Type == 1) %>% 
       summarise( Sell_Trade = n()) %>% 
       subset(select = c(Symbol,Sell_Trade)) %>%
       rbind(allTrade)
     
   } else{
     sellTrade <- Stats()  %>%
       group_by(Symbol, Type) %>% 
       filter(Type == 1) %>% 
       summarise(Sell_Trade = n()) %>% 
       subset(select = c(Symbol,Sell_Trade))
   }
 })
 
 
 
  

  output$result <- renderTable({
    if(nrow(Stats()>0)){
      
     DF_allPair <- DF_Balance_All()
     allPair <- round(DF_allPair[nrow(DF_allPair),4],2)
   
     final_Balance  <- vector("numeric", length(pair()))
     for (i in 1: length(pair())){
        pairBalance <-  DF_Balance() %>% filter(Symbol == pair()[i])
        final_Balance[i]  <- round(pairBalance[nrow(pairBalance),4],2)
      }
      
      Final_Balance <- data.frame(Symbol = pair(),
                                  Final_Balance = final_Balance) 
      
      if(input$MagicNum == "All"){
           All_Pair <- c("ALL PAIR", allPair)
           
          Final_Balance <- rbind(Final_Balance,All_Pair)  
          Final_Balance <- left_join(x = Final_Balance,y = buyProfit(), by = "Symbol")
           Final_Balance <- left_join(x = Final_Balance,y = sellProfit(), by = "Symbol")
          Final_Balance <- left_join(x = Final_Balance, y = buyTrade(),by = "Symbol")
          Final_Balance <- left_join(x = Final_Balance, y = sellTrade(),by = "Symbol") 
          
      }else{
          Final_Balance <- left_join(x = Final_Balance,y = buyProfit(), by = "Symbol")
          Final_Balance <- left_join(x = Final_Balance,y = sellProfit(), by = "Symbol")
          Final_Balance <- left_join(x = Final_Balance, y = buyTrade(),by = "Symbol")
          Final_Balance <- left_join(x = Final_Balance, y = sellTrade(),by = "Symbol") 
      }
          FB <- as.data.frame(Final_Balance)
          FB[is.na(FB)] <- 0
          FB <- FB %>% cbind(FB$Buy_Trade + FB$Sell_Trade) %>%
            set_names(c("Symbol","Final_Balance","Buy_Profit","Sell_Profit","Buy_Trade","Sell_Trade", "Total_Trade"))
       }
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
      str(sellTrade())
      print(sellTrade())
   
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
