#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

source("skey.R")

CSS <- "
#splash {
        position: absolute;
        background: #ffffff;
        z-index: 100;
        left: 0;
        right: 0;
        height: 100%;
        text-align: center;
        color: #cccccc;
}
"

# Define UI for application that draws a histogram
ui <- fluidPage(
        useShinyjs(),
        inlineCSS(CSS),
        
        div(
                id ="splash",
                tags$br(),
                h3("Loading skey...")
        ),
        
        mainPanel(
                tags$br(),
                h3("Predictive Text Input Model"),
                fluidRow(
                        column(10,
                               textInput(inputId = "inputText",
                                         width="100%",
                                         label = "",
                                         value = "how are you")
                        ),
                        column(2,
                               tags$br(),
                               actionButton(inputId="inputClear", 
                                            width="100%",
                                            label="Clear")
                        )
                ),
                fluidRow(
                        column(12,
                               span(verbatimTextOutput("predictions"), align="center")
                        )
                ),
                fluidRow(
                        column(3,
                               checkboxInput(inputId="inputSafe", 
                                             label="Profanity filter", 
                                             value=TRUE)
                        ),
                        column(3,
                               checkboxInput(inputId="inputFive", 
                                             label="Predict 5 words", 
                                             value=FALSE)
                        )
                )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

        load("ng.RData")
        
        observeEvent(input$inputClear, {
                updateTextInput(session, "inputText", label=NULL, value="")
        })

        output$predictions <- renderText({
                if (input$inputFive == TRUE) {
                        nw <- 5
                } else {
                        nw <- 1
                }
                paste(unlist(predictWords(input$inputText, ng, nw, input$inputSafe)), collapse=" | ")
        })

        hide(id="splash", anim=TRUE, animType="fade")
}

# Run the application 
shinyApp(ui = ui, server = server)

