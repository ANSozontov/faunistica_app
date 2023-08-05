# initial -----------------------------------------------------------------
library(shiny)
library(shinyalert)
# library(RPostgreSQL)
# library(RSQLite)
library(tidyverse)

L <- readxl::read_xlsx("translation.xlsx") %>% 
    unite("key", block, key) %>% 
    transpose(.names = .$key)

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
# Server logic ------------------------------------------------------------

    current_language <- reactiveVal("ru")
    
    r <- function(X){ # translates text into current language
        sapply(X,function(s) L[[s]][[current_language()]], USE.NAMES=FALSE)
    }
    
    output$test <- renderText(r("test_curretnt_language"))

    # language 
    observeEvent(input$change_language, {
        if(current_language() == "ru") {
            updateActionButton(session, "change_language", label = "EN")
            current_language("en")
        } else{
            updateActionButton(session, "change_language", label = "RU")
            current_language("ru")
        }
    })
    

    
    p1 <- 'renderUI(
        tabPanel("Главная", 
                 textOutput("test"),
                 tags$img(src = "dog.jpeg", width = 500),
                 tags$img(src = "dog.jpeg", width = 500)
        )
    )'
    p2 <- 'renderUI(
        tabPanel("Вторая",
                 textOutput("test"),
                 tags$img(src = "dog.jpeg", width = 500),
                 tags$img(src = "dog.jpeg", width = 500)
        )
    )'
    
    nvbr <- renderUI(
        navbarPage(
            title = tags$div(style="position: relative; margin-right: 90px", 
                             tags$img(src="logo_placeholder.svg", height = "70px"),
                             tags$p(style="position: relative; top: -70px; left: 90px; ", 
                                    "Faunistica 2.0")
            ),
            # windowTitle = "Faunistica 2.0",
            # position = "fixed-top", 
            eval(parse(text =p2)),
            eval(parse(text =p2))
        )
    )
}

# UI ----------------------------------------------------------------------
ui <- fluidPage(
    
    actionButton("change_language", "RU", 
                 # label = current_language(),
                 icon = icon("globe"),
                 style = "position: absolute; top: 8px; right: 5px; z-index:10000;"),
    
    textOutput("test")
    
)

# Run the application 
shinyApp(ui = ui, server = server,  
    options = list(launch.browser = FALSE)
)