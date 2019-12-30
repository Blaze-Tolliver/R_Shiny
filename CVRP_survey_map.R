
library(reshape2)
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
library(tidyr)

survey <- read_excel("FCEVsurvey.xlsx", col_names = TRUE)  
vars <- colnames(survey)
vars <- vars[8:109]
vars <- str_replace_all(vars, "[abcdefghijklm]","")
vars <- tibble(cols = vars)
vars <- distinct(vars, cols)


ui <- fluidPage(
   
   # Application title
   titlePanel(""),
   
   # Sidebar with inputs
   sidebarLayout(
      sidebarPanel(
       selectInput("question","Select a Survey Question", vars, multiple = FALSE),
       radioButtons("metric","Select a Statistic", choices = c("mean","median","max","min", "IQR","count")),
       sliderInput("n","minimum number of respondents per county", value = 0, min = 0, max = 15),
       uiOutput("sub_questions"),
       uiOutput("bins"),
       width = 2
      ),
      
      # main panel with map and plots
      mainPanel(
        splitLayout(
            plotOutput("map", width = "100%", height = "600px"),
            plotOutput("count", width = "100%", height = "600px")
        ),
        plotOutput("facet", "count", width = "100%", height = "600px")
        
      )
   )
)

# Define server logic
server <- function(input, output) {
   
    isolate({
     
      
        
      Vals <- reactiveValues(
        survey = read_excel("FCEVsurvey.xlsx", col_names = TRUE)
      )    
      Vals$counties <- map_data("county") %>%
        subset(region == "california")
      
      Vals$survey$county <- tolower(Vals$survey$county)
      
      Vals$survey <- gather(Vals$survey, key = variable, value = value, -Response_ID, -category, -model, -utility, -county, -purchdate, -modelyear)
      
      Vals$ditch_the_axes <- theme(
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank()
      )
     
    })
      
  
    
    observeEvent(input$question, {
            
            
            Vals$survey <- mutate(Vals$survey, question = variable) 
            Vals$survey$question <- str_replace_all(Vals$survey$question, "[abcdefghijklm]","")
            Vals$selected <- filter(Vals$survey, question == input$question)
            
            Vals$num_questions <- distinct(Vals$selected, variable)
            Vals$sub_questions <- Vals$num_questions$variable
            Vals$num_questions <- length(Vals$sub_questions)
            
            Vals$selected <- group_by(Vals$selected, county)
            Vals$selected$num_values <- as.numeric(Vals$selected$value)
            
            if (Vals$num_questions == 1) {
                      # summary statistic calculation
                      
                      Vals$sub_question <- Vals$selected
                      Vals$summary <- summarise(filter(Vals$sub_question, num_values != -77 & num_values != -88 & num_values != -99), mean = mean(num_values, na.rm = TRUE), 
                                                median = median(num_values, na.rm = TRUE),
                                                max = max(num_values, na.rm = TRUE),
                                                min = min(num_values, na.rm = TRUE),
                                                IQR = IQR(num_values, na.rm = TRUE),
                                                count = n())
                     
                      Vals$counts <- Vals$selected 
                      Vals$counts <- ungroup(Vals$counts)
                      Vals$counts <- group_by(Vals$counts, variable)
                      Vals$counts <- filter(Vals$counts, !is.na(num_values))
                      
                      count <- tibble( county = Vals$summary$county, count = Vals$summary$count)
                      Vals$counts <- left_join(Vals$counts, count, by = "county")
                      
                      Vals$unique <- distinct(ungroup(Vals$counts), num_values)
                      glimpse(Vals$unique)
                      
                      #join summary statistics to map of counties          
                      Vals$mapped_vars <- full_join(Vals$counties, Vals$summary, by = c("subregion" = "county")) %>%
                        group_by(subregion)
                      
                      diff <- max(Vals$mapped_vars[[input$metric]], na.rm = TRUE) - min(Vals$mapped_vars[[input$metric]], na.rm = TRUE)
                      
                      if(diff != 0) {
                        Vals$num_range <- diff
                      } else {
                        Vals$num_range <- max(Vals$mapped_vars[[input$metric]], na.rm = TRUE)
                      } 
            } else {
                      

                      output$sub_questions <- renderUI({
                        selectInput("sub_question","Choose a sub-question to summarise", choices = Vals$sub_questions , multiple = FALSE)
                      })
                      
                      observeEvent(input$sub_question, {
                        Vals$sub_question <- filter(Vals$selected, variable == input$sub_question)
                        Vals$summary <- summarise(filter(Vals$sub_question, num_values != -77 & num_values != -88 & num_values != -99), mean = mean(num_values, na.rm = TRUE), 
                                                  median = median(num_values, na.rm = TRUE),
                                                  max = max(num_values, na.rm = TRUE),
                                                  min = min(num_values, na.rm = TRUE),
                                                  IQR = IQR(num_values, na.rm = TRUE),
                                                  count = n())
                        glimpse(Vals$summary)
                        
                        Vals$counts <- Vals$selected 
                        Vals$counts <- ungroup(Vals$counts)
                        Vals$counts <- group_by(Vals$counts, variable)
                        Vals$counts <- filter(Vals$counts, !is.na(num_values))
                        
                        count <- tibble( county = Vals$summary$county, count = Vals$summary$count)
                        Vals$counts <- left_join(Vals$counts, count, by = "county")
                        
                        Vals$unique <- distinct(ungroup(Vals$counts), num_values)
                        glimpse(Vals$unique)
                        
                        #join summary statistics to map of counties          
                        Vals$mapped_vars <- full_join(Vals$counties, Vals$summary, by = c("subregion" = "county")) %>%
                          group_by(subregion)
                        
                        diff <- max(Vals$mapped_vars[[input$metric]], na.rm = TRUE) - min(Vals$mapped_vars[[input$metric]], na.rm = TRUE)
                        
                        if(diff != 0) {
                          Vals$num_range <- diff
                        } else {
                          Vals$num_range <- max(Vals$mapped_vars[[input$metric]], na.rm = TRUE)
                        } 
                      })
                       
              
            }
            

  
            output$map <- renderPlot({
              
                  ggplot( data = Vals$mapped_vars , aes( x = long, y = lat, group = group)) +
                   coord_fixed(1.3) +
                    geom_polygon(fill = "grey", color = "white")+
                    geom_polygon ( data = filter(Vals$mapped_vars, count >= input$n), aes(fill = eval(parse(text = input$metric))), color = "white", stat = "identity")+
                    scale_fill_gradientn(name = "Values",colours = rev(terrain.colors(n = Vals$num_range +1))) +
                    Vals$ditch_the_axes+
                    labs( title = "CVRP Results by County for Selected Question or Sub-Question")+
                    theme_void()

            })
            
            output$count <- renderPlot({
              
                  ggplot( data = filter(Vals$summary, count >= input$n) , aes( x = county, y = eval(parse(text = input$metric))
                                                                           , fill = eval(parse(text = input$metric)) )) +
                    geom_col()+
                    geom_text(aes(label = count), nudge_y = -0.1)+
                    coord_flip()+
                    theme_minimal()+
                    scale_fill_gradientn(name = "Values", colours = rev(terrain.colors(n = Vals$num_range +1)))+
                    labs( y = "Bar Length = Statistic Value, Bar Label = Number of Responses", x="")+
                    theme(legend.position = "none")
                
            })
    
            output$facet <- renderPlot({
                    
                   
                  if (length(Vals$unique$num_values) < 10) {  
                    ggplot( aes( x = variable, fill = value), data = filter(Vals$counts, count >= input$n))+
                      geom_bar()+
                      labs( x = '', y = 'Count', title = "Bar Plot of Survey Response Counts")+
                      theme_minimal()+
                      scale_fill_discrete()+
                      facet_wrap(~county)+
                      coord_flip()
                  } else {
                    
                      isolate({
                          output$bins <- renderUI({
                            sliderInput("bin","histogram bins", value = 6, min = 1, max = 15)
                          })
                      })
                        #observeEvent(input$bin,{  
                                ggplot( aes(x = num_values, fill = variable), data = filter(Vals$counts, count >= input$n))+
                                  geom_histogram(bins = input$bin)+
                                  labs( x = 'Survey Value', y = 'Count', title = "Histogram of Survey Response Values")+
                                  theme_minimal()+
                                  facet_wrap(~county)+
                                  coord_flip()
                        #})    
                  }
            })
            
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

