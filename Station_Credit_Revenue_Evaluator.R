#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(reshape2)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("H2 Station Credit Revenue Evaluator"),
   
   # Sidebar
   sidebarLayout(
      sidebarPanel(
         sliderInput("credit",
                     "Credit price ($)",
                     min = 1,
                     max = 200,
                     value = 100),
         sliderInput("HRI_CI",
                     "Non-EER adjusted CI of dispensed H2 (g CO2e/MJ)",
                     min = 0,
                     max = 150,
                     value = 100,
                     step = 5),
         sliderInput("year",
                     "Beginning year",
                     min = 2019,
                     max = 2026,
                     value = 2019,
                     step = 1),
         sliderInput("ut",
                     "Average % uptime in beginning year",
                     min = 50,
                     max = 100,
                     value = 90,
                     step = 1),
         sliderInput("ut_change",
                     "Annual change in uptime (absolute %)",
                     min = -3,
                     max = 3,
                     value = 0,
                     step = .5),
         sliderInput("df",
                     "Average % of HRI capacity dispensed in beginning year",
                     min = 0,
                     max = 100,
                     value = 50,
                     step = 5),
         sliderInput("df_change",
                     "Annual change in HRI capacity dispensed (absolute %)",
                     min = -5,
                     max = 5,
                     value = 0,
                     step = .5),
         sliderInput("cap",
                     "Station's daily HRI capacity (kg)",
                     min = 100,
                     max = 1200,
                     value = 400,
                     step = 50),
         sliderInput("deficits",
                     "Total LCFS program deficits in starting year (MMT)",
                     min = 1,
                     max = 10,
                     value = 2.5,
                     step = 0.5),
         sliderInput("def_change",
                     "Annual change in program deficits (%)",
                     min = -20,
                     max = 20,
                     value = 0,
                     step = 1)
      ),
      
      # plots and table output
      mainPanel(
         tableOutput("results"),
         plotOutput("income_quart"),
         plotOutput("income"),
         plotOutput("stations"),
         plotOutput("credits")
         
      )
   )
)

# Define server logic
server <- function(input, output) {
    isolate({    
           Vals <- reactiveValues(
             gas = tibble( year = 2019:2041, CI = c(94.17,92.92,91.66,90.41,89.15,87.89,86.64,85.38,84.13,82.87,81.62,
                                                    80.36,80.36,80.36,80.36,80.36,80.36,80.36,80.36,80.36,80.36,80.36,80.36)),
           EER = 2.5,
           Eh2 = 120,
           Q = c(90, 91, 92, 92),
           C = 0.000001
           
         )
      
    })
   
  # 
   
   output$credits <- renderPlot({
      #generate income per station from
     
     
     for (i in 1:15) {
       for (q in 1:4){
         t <- (i*4)-(4-q)
         year_off <- input$year - 2019  
         Vals$earnings$year[t] <- input$year + i - 1
         Vals$earnings$quarter[t] <- q
         
         #setting quarterly values for LCFS deficit, uptime, and HRI capacity dispensed
         if (t == 1) {
           Vals$deficits[t] <- input$deficits*1000000
           Vals$ut[t] <- input$ut/100
           Vals$df[t] <- input$df/100
         } 
         else {
           Vals$deficits[t] <- Vals$deficits[t-1] + Vals$deficits[t-1]*(input$def_change/400)
           Vals$ut[t] <- min(Vals$ut[t-1] + (input$ut_change/400),  1)
           Vals$df[t] <- min(Vals$df[t-1] + (input$df_change/400),  1)
         }
         
         
         Vals$earnings$credits[t] <- max((Vals$gas$CI[i+year_off]*Vals$EER-input$HRI_CI)*Vals$Eh2*(input$cap*Vals$ut[t]*Vals$Q[q]-Vals$df[t]*input$cap*Vals$Q[q])*Vals$C,0)
         Vals$dispensed$credits[t] <- input$cap * Vals$df[t] * Vals$Q[q] * Vals$Eh2 * Vals$EER * Vals$C * (Vals$gas$CI[i+year_off] - (input$HRI_CI/Vals$EER))
         
         
         
         
       }
     } 
     
     creditz <- tibble(year = Vals$earnings$year, quarter = Vals$earnings$quarter, credits_HRI = Vals$earnings$credits, credits_throughput = Vals$dispensed$credits)
     creditz <- melt(creditz, id = c("year", "quarter"))  
     
     ggplot( data = creditz, aes(x = year+quarter*.25, y = value, fill = variable))+
      geom_col(color = "black")+
      labs(title = "Quarterly Credits per Station", y = "Credits (Metric Tons)", x = "Year and Quarter")+
       theme_minimal()
     
   })
   
  
   output$income <- renderPlot({
     
     
     
     Vals$creditz <- tibble(year = Vals$earnings$year, quarter = Vals$earnings$quarter, credits = Vals$earnings$credits, quarterly_HRI = credits*input$credit, 
                       quarterly_throughput = Vals$dispensed$credits*input$credit, cumulative_HRI = cumsum(quarterly_HRI), cumulative_through = cumsum(quarterly_throughput))
     
     cumcreditz <- melt(Vals$creditz, id = c("year", "quarter", "credits", "quarterly_HRI", "quarterly_throughput"))
     
     ggplot()+
       geom_col(data = cumcreditz, aes(x = year+quarter*.25, y = value/1000, fill = variable), position = "stack", color = "black", size = .5)+
       labs(title = "Cumulative Revenue per Station for 15 years", y = "Thousands of Dollars", x = "Year and Quarter")+
       theme_minimal()
       
     
     
   })
   
   output$income_quart <- renderPlot({
     
     
     
     quartcreditz <- melt(Vals$creditz, id = c("year", "quarter", "credits", "cumulative_HRI", "cumulative_through"))
     
     ggplot()+
       geom_col(data = quartcreditz, aes(x = year+quarter*.25, y = value/1000, fill = variable), position = "stack", color = "black", size = .5)+ 
       labs(title = "Quarterly Revenue per Station for 15 years", y = "Thousands of Dollars", x = "Year and Quarter")+
       theme_minimal()
     
     
     
   })
   
   output$stations <- renderPlot({
     
     creditz <- tibble(year = Vals$earnings$year, quarter = Vals$earnings$quarter, credits = Vals$earnings$credits, stations = (Vals$deficits*.025)/Vals$earnings$credits)
     max_stations <- max(creditz$stations, na.rm = TRUE)
     glimpse(creditz)
     creditz <- melt(creditz, id = c("year", "quarter", "credits"))
     
     
     
     ggplot( data = creditz)+
       geom_step(aes(x = year+quarter*.25, y = value, color = variable), position = "dodge")+
       labs(title = "Number of Identical Stations Supported by 2.5% of Quarterly Deficit", y = "Stations", x = "Year and Quarter")+
       scale_y_continuous(limits = c(0, max_stations +50))+
       theme_minimal()
     
     
   })
   
   output$results <- renderTable({
     
     resultz <- tibble(Total_HRI_Revenue = max(Vals$creditz$cumulative_HRI), Total_Throughput_Revenue = max(Vals$creditz$cumulative_through))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

