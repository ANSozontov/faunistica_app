# initial -----------------------------------------------------------------
library(shiny)
library(shinyalert)
library(tidyverse)

L <- readxl::read_xlsx("translation_app.navbar.as.ui.xlsx") %>% 
    unite("key", block, key) %>% 
    transpose(.names = .$key)

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
# Server logic ------------------------------------------------------------

    CL <- reactiveVal("ru")
    
    r <- function(X, current_language = CL()){ # translates text into current language
        sapply(X,function(s) L[[s]][[current_language]], USE.NAMES=FALSE)
    }
    
    output$test <- renderText(r("test_curretnt_language"))
    output$pg1 <- renderText(r("test_page1"))
    output$pg2 <- renderText(r("test_page2"))

    # language 
    observeEvent(input$change_language, {
        if(CL() == "ru") {
            updateActionButton(session, "change_language", label = "EN")
            CL("en")
        } else{
            updateActionButton(session, "change_language", label = "RU")
            CL("ru")
        }
    })

    output$p1 <- renderUI(tagList(
        br(),
        tags$img(src = "3253F523.jpg", width = 500),
        br(),
        tags$img(src = "3253F521.jpeg", width = 500)
        
    ))
    
    output$p2 <- renderUI(tagList(
                 br(),
                 tags$img(src = "dog.jpeg", width = 500),
                 br(),
                 tags$img(src = "dog.jpeg", width = 500)
        
    ))
    
    output$nvbr <- renderUI(tagList(
        navbarPage(
            title = tags$div(style="position: relative; margin-right: 90px", 
                             tags$img(src="logo_placeholder.svg", height = "70px"),
                             tags$p(style="position: relative; top: -70px; left: 90px; ", 
                                    "Faunistica 2.0")
            ),
            windowTitle = "Faunistica 2.0",
            position = "fixed-top",
            tabPanel(title = textOutput("pg1"), 
                    uiOutput("p1")
                     ),
            tabPanel(title = textOutput("pg2"), 
                    uiOutput("p2")
            )
    )))
}

# UI ----------------------------------------------------------------------
ui <- fluidPage(
    uiOutput("nvbr"),
    actionButton("change_language", "RU", 
                 # label = current_language(),
                 icon = icon("globe"),
                 style = "position: absolute; top: 8px; right: 5px; z-index:10000;")
)

# Run the application 
shinyApp(ui = ui, server = server,  
    options = list(launch.browser = FALSE)
)