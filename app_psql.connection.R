library(shiny)
library(RPostgreSQL)
library(tidyverse)

# UI ----------------------------------------------------------------------
ui <- fluidPage(
    titlePanel("Служба автоматизированного учёта чаепитий"),
        tabsetPanel(
            tabPanel("Статистика чаепитий",
            sidebarLayout(
                sidebarPanel(
                # HTML("<br>"),
                uiOutput("names_selector"),
                # selectInput("usr", "Чаёвник...", 
                #             choices = 1:2), ###
                HTML("<br>"),
                # HTML(h2("Показа"))
                numericInput("rowstoshow", "Строчек для показа: ", 
                             value = 1, min = 1, max = 50),
                HTML("<br>"),
                actionButton("refresh", "Обновить!")
            ),
            mainPanel(
                HTML("<br>"),
                uiOutput("little_title"),
                HTML("<br>"),
                tableOutput("res_table"),
                HTML("<br>"),
                sliderInput("bins", 
                            "Number of bins:",
                            min = 1,
                            max = 50,
                            value = 30),
                plotOutput("distPlot")
            )
            )
            ),
            tabPanel("Ввод новых чаепитий")
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
# Connect -----------------------------------------------------------------
    con_config <- list(database = "rdatabase", 
                       hostname = "localhost",
                       dsn_port = "5432", 
                       dsn_uid  = "ruser", 
                       mypass   = scan("/var/mypass", 
                            what = "", nlines = 1, quiet = TRUE))
    con <- dbConnect(dbDriver("PostgreSQL"), 
                     dbname = con_config$database,
                     host = con_config$hostname, 
                     port = con_config$dsn_port,
                     user = con_config$dsn_uid, 
                     password = con_config$mypass)

# Prepare table -----------------------------------------------------------
    testdf <- data.frame(name1 = c("Petir", "Mike", "Norman", "Borman", "Xi", "Xi"), 
               name2 = c("Mike", "Anna", "Borman", "Norman", "Petir", "Anna"), 
               dat = as.Date(c("02/12/22", "12/02/23", "01/10/21", 
                               "01/01/00", "26/07/89", "26/07/23"), 
                             format = "%d/%m/%y"), 
               proof = c("Jon", "John", "Johan", "Joe", "Ju", "Go"))
    
    users <- reactive({unique(c(testdf$name1, testdf$name2))})
    
    output$names_selector <- renderUI({
        selectInput("usr", "Чаёвник", choices = users()) 
    })
    
    output$little_title <- renderUI({paste0("Результаты поиска чаепитий для ", 
                                          input$usr,
                                          ":")})

# Dummy -------------------------------------------------------------------
    output$distPlot <- renderPlot({
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    
    
    
    observeEvent(input$refresh, {
        df <- reactive({
            testdf %>% 
                filter(name1 == input$usr | name2 == input$usr) %>% 
                transmute(
                    `С кем:` = case_when(name1 == input$usr ~ name2, TRUE ~ name1), 
                    `Когда:` = as.character(dat), 
                    `Подтверждает:` = proof)
        })
        
        output$res_table <- renderTable({df()})
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
