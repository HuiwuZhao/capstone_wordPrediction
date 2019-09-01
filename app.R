#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building appllsications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("functions.R")

ui <- fluidPage(
        # Application title
        titlePanel("Data Science Capstone Project - Next Word Prediction App"),
        
        # User interface controls1
        sidebarLayout(
                sidebarPanel(
                        p("Input your word or text and press <ENTER> or click <Predict> to see the next word(s):"),	
                        textInput(inputId="text", label = ""),
                        submitButton("Predict"),
                        HTML('<script type="text/javascript"> 
        document.getElementById("text").focus();
        </script>')
                ),        
                
                mainPanel(
                        tabsetPanel(
                                
                                tabPanel("Result", 
                                         conditionalPanel(condition = "input.text != ''",
                                                          verbatimTextOutput("text"),
                                                          verbatimTextOutput("cleaned",placeholder = T), verbatimTextOutput("msg"),
                                                          selectInput("predicts","Word predictions:",choices=c(""))
                                         )
                                )                 
                                )
                        )
                )
 )
                
                


server <- function(input, output, session) {
        
        output$text <- renderText({
                paste("The Input text is:",input$text)
        })
        
        output$cleaned <- renderText({
                paste("The Cleansed Input text is:",Clean(input$text))
        })
        
        
        observe({
                iniTime <- Sys.time()
                
                textCleansed <- Clean(input$text)
                        
                        predictWords <- predict_next_word(textCleansed)
                        updateSelectInput(session = session, inputId = "predicts", choices = predictWords)
                        
                        endTime <- Sys.time()
                        output$msg <- renderText({
                                paste(msg, "\n", sprintf("- Total time processing = %6.3f msecs",1000*(endTime-iniTime)))
                        })
                        gc()
                #}  
        })
}

#Run the application
shinyApp(ui = ui, server = server)