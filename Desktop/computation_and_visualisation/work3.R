#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Name : Ananya Naresh
#NUID : 001315944

#Question1 - widget creation
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    actionButton("action", label = "Action"),
    hr(),
    fluidRow(column(2, verbatimTextOutput("value"))),

    checkboxInput("checkbox", label = "Choice A", value = TRUE),
    hr(),
    fluidRow(column(3, verbatimTextOutput("value2"))),
    
    checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
                       choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                       selected = 1),
    hr(),
    fluidRow(column(3, verbatimTextOutput("value3"))),
    
    dateInput("date", label = h3("Date input"), value = "2014-01-01"),
    hr(),
    fluidRow(column(3, verbatimTextOutput("value4"))),
    
    dateRangeInput("dates", label = h3("Date range")),
    hr(),
    fluidRow(column(4, verbatimTextOutput("value5"))),
    
    fileInput("file", label = h3("File input")),
    hr(),
    fluidRow(column(4, verbatimTextOutput("value6"))),
    
    numericInput("num", label = h3("Numeric input"), value = 1),
    hr(),
    fluidRow(column(3, verbatimTextOutput("value7"))),
    
    radioButtons("radio", label = h3("Radio buttons"),
                 choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                 selected = 1),
    hr(),
    fluidRow(column(3, verbatimTextOutput("value8"))),
    selectInput("select", label = h3("Select box"), 
                choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                selected = 1),
    hr(),
    fluidRow(column(3, verbatimTextOutput("value9"))),
    fluidRow(
        column(4,sliderInput("slider1", label = h3("Slider"), min = 0, max = 100, value = 50)),
    hr(),
    
         fluidRow(
            column(4, verbatimTextOutput("value10"))
        )
    ),
    fluidRow(
        column(4, sliderInput("slider2", label = h3("Slider Range"), min = 0, max = 100, value = c(40, 60))),
        
        hr(),
        
        fluidRow(
            column(4, verbatimTextOutput("value11"))
        )
    ),
    textInput("text", label = h3("Text input"), value = "Enter text..."),
    
    hr(),
    fluidRow(column(3, verbatimTextOutput("value12")))

)
    

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$value <- renderText({ paste0("The value : ",input$action)})    
    output$value2 <- renderText({ paste0("The selected value : ",input$checkbox)})
    output$value3 <- renderText({ paste0("The selected value : ",input$checkGroup)})
    output$value4 <- renderText({ paste0("The selected value : ",input$date)})
    output$value5 <- renderText({ paste0("The selected value : ",input$dates)})
    output$value6 <- renderPrint({
        str(input$file)
    })
    output$value7 <- renderText({ paste0("The entered value : ",input$num)})
    output$value8 <- renderText({ paste0("The selected value : ",input$radio)})
    output$value9 <- renderText({ paste0("The selected value :  ",input$select)})
    output$value10 <- renderText({ paste0("The selected value : ",input$slider1)})
    output$value11 <- renderText({ paste0("The selected value :  ",input$slider2)})
    output$value12 <- renderText({ paste0("The entered value :  ",input$text)})
}


# Run the application 
shinyApp(ui = ui, server = server)
