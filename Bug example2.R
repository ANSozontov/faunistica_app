library(shiny)
library(shinyjs)

ui <- fluidPage(
    shinyjs::useShinyjs(),
    uiOutput("page1")
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

    output$page1 <- renderUI(tagList(
        HTML(status()),
        passwordInput("test1", label = "Test 1"), 
        shinyjs::disabled(numericInput("test2", "Test 2", 0)),
        actionButton("login", "Log in"),
        actionButton("logout", "Log out")
    ))
}

shinyApp(ui = ui, server = server)