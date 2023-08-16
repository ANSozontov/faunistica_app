library(shiny)
runApp(shinyApp(
    ui = fluidPage(
        shinyjs::useShinyjs(),
        numericInput("test", "Test", 5),
        actionButton("submit0", "Hold"),
        actionButton("submit1", "Unhold")
    ),
    
    server = function(input, output, session) {
        shinyjs::disable("submit0")
        shinyjs::disable("test")
        
        observeEvent(input$submit0, {
            shinyjs::disable("test")
            shinyjs::disable("submit0")
            shinyjs::enable("submit1")
        })
        
        observeEvent(input$submit1, {
            shinyjs::enable("test")
            shinyjs::disable("submit1")
            shinyjs::enable("submit0")
        })
    }
))
