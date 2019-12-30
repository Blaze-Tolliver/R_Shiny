

library(shiny)
library(maps)
library(mapproj)
library(ggplot2)
library(mapdata)
library(ggmap)
library(readxl)
library(stringr)
library(dplyr)
library(tibble)

survey <- read_excel("FCEVsurvey.xlsx", col_names = TRUE)  
vars <- colnames(survey)
vars <- vars[8:109]



ui <- fluidPage(
   
   # 
   titlePanel("CVRP Survey Responses by County"),
   
   #
   sidebarLayout(
      sidebarPanel(
       selectInput("question","Select a Survey Question", vars, multiple = FALSE),
       radioButtons("metric","Select a Statistic", choices = c("mean","median","max","min", "IQR")),
       sliderInput("n","minimum number of respondents per county", value = 0, min = 0, max = 15),
       width = 2
      ),
      
      
      mainPanel(
        splitLayout(
            plotOutput("map", width = "100%", height = "600px"),
            plotOutput("count")
        ) 
      )
   )
)


server <- function(input, output) {
   
      observeEvent(input$question, {
            
            
        
            Vals <- reactiveValues(
              survey = read_excel("FCEVsurvey.xlsx", col_names = TRUE, na = c("-99","-77",""))
            )    
        
            Vals$counties <- map_data("county") %>%
              subset(region == "california")
            
            Vals$survey$county <- tolower(Vals$survey$county)
            
            Vals$selected <- select(Vals$survey, Response_ID, category, model, utility, county, purchdate, 
                                    modelyear, input$question)
            Vals$selected <- group_by(Vals$selected, county)
            
            Vals$selected <- filter(Vals$selected, !is.na(eval(parse(text = input$question))) )
            Vals$summary <- summarise(Vals$selected, mean = mean(eval(parse(text = input$question)), na.rm = TRUE), 
                                      median = median(eval(parse(text = input$question)), na.rm = TRUE),
                                      max = max(eval(parse(text = input$question)), na.rm = TRUE),
                                      min = min(eval(parse(text = input$question)), na.rm = TRUE),
                                      IQR = IQR(eval(parse(text = input$question)), na.rm = TRUE),
                                      n = n())
           print.default(Vals$summary[1,])
                      
                      Vals$mapped_vars <- full_join(Vals$counties, Vals$summary, by = c("subregion" = "county")) %>%
                        group_by(subregion)
                      print.default(max(Vals$mapped_vars[[input$metric]], na.rm = TRUE))
                      diff <- max(Vals$mapped_vars[[input$metric]], na.rm = TRUE) - min(Vals$mapped_vars[[input$metric]], na.rm = TRUE)
                      print.default(diff)
                      if(diff != 0) {
                        Vals$num_range <- diff
                      } else {
                        Vals$num_range <- max(Vals$mapped_vars[[input$metric]], na.rm = TRUE)
                      } 
                      
                      count
                      #Vals$num_range <- 4
            #})
            
            isolate({
                  
                  Vals$ditch_the_axes <- theme(
                    axis.text = element_blank(),
                    axis.line = element_blank(),
                    axis.ticks = element_blank(),
                    panel.border = element_blank(),
                    panel.grid = element_blank(),
                    axis.title = element_blank()
                  )
                  
            })
  
            output$map <- renderPlot({
              
                  ggplot( data = Vals$mapped_vars , aes( x = long, y = lat, group = group)) +
                    coord_fixed(1.3) +
                    geom_polygon(fill = "grey", color = "white")+
                    geom_polygon ( data = filter(Vals$mapped_vars, n >= input$n), aes(fill = eval(parse(text = input$metric))), color = "white", stat = "identity")+
                    scale_fill_gradientn(name = "Values",colours = rev(terrain.colors(n = Vals$num_range +1))) +
                    Vals$ditch_the_axes+
                    labs( title = "CVRP Results by County")+
                    theme_void()
                    
            })
            
            output$count <- renderPlot({
              
                  ggplot( data = filter(Vals$summary, n >= input$n) , aes( x = county, y = eval(parse(text = input$metric))
                                                                           , fill = eval(parse(text = input$metric)) )) +
                    geom_col()+
                    geom_text(aes(label = n), nudge_y = -0.1)+
                    coord_flip()+
                    theme_minimal()+
                    scale_fill_gradientn(name = "Values", colours = rev(terrain.colors(n = Vals$num_range +1)))+
                    labs( y = "Bar Length = Metric Value, Bar Label = Number of Responses", x="")+
                    theme(legend.position = "none")
                
            })
    
            
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

