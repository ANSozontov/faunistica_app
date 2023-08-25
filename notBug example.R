library(shiny)
library(shinyjs)

ui <- fluidPage(
    shinyjs::useShinyjs(),
    htmlOutput("page1"),
    passwordInput("test1", label = "Test 1"), 
    shinyjs::disabled(numericInput("test2", "Test 2", 0)),
    actionButton("login", "Log in"),
    actionButton("logout", "Log out")
)

server <- function(input, output, session) {
    status <- reactiveVal("on start...")

    observeEvent(input$login, {
        disable("login")
        enable("logout")
        disable("test1")
        enable("test2")
        status("mode A")
    })
    
    observeEvent(input$logout, {
        enable("login")
        disable("logout")
        enable("test1")
        disable("test2")
        status("mode B")
    })

    output$page1 <- renderPrint(status())
}

shinyApp(ui = ui, server = server)