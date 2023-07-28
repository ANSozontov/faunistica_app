# initial -----------------------------------------------------------------
library(shiny)
library(RPostgreSQL)
library(tidyverse)
# testdf <- data.frame(name1 = c("Petir", "Mike", "Norman", "Borman", "Xi", "Xi"),
#            name2 = c("Anna", "Anna", "Borman", "Norman", "Petir", "Anna"),
#            dat = as.Date(c("02/12/22", "12/02/23", "01/10/21",
#                            "01/01/00", "26/07/89", "26/07/23"),
#                          format = "%d/%m/%y"),
#            proof = c("Jon", "John", "Johan", "Joe", "Ju", "Go"))


# connection --------------------------------------------------------------
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


# initial users -----------------------------------------------------------



# Observe DT changes ------------------------------------------------------


# UI ----------------------------------------------------------------------
ui <- fluidPage(
    titlePanel("Служба автоматизированного учёта чаепитий"),
        tabsetPanel(
            tabPanel("Статистика чаепитий",
            sidebarLayout(
                sidebarPanel(
                uiOutput("names_selector"),
                # selectInput("usr", "Чаёвник...", 
                #             choices = 1:2), ###
                HTML("<br>"),
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
                HTML("<br>")
            )
            )
            ),
            tabPanel("Ввод новых чаепитий", 
            sidebarLayout(
            sidebarPanel(
                HTML("<br>"), 
                textInput("i_name1", "Чаёвник1"),
                textInput("i_name2", "Чаёвник2"),
                dateInput("i_dat", "Дата чаепития", format = "yyyy/mm/dd"),
                textInput("i_proof", "Я видел это своими глазами и подтверждаю факт чаепития"),
                HTML("<br>"),
                actionButton("record", "Записать!")
            ), 
            mainPanel(
                tags$img(src = "dog.jpeg", width = 500)
            )
            )
            )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
# Connect -----------------------------------------------------------------


# Prepare table -----------------------------------------------------------

    
    users <- reactive({unique(c(
        DBI::dbGetQuery(con,"SELECT * FROM my_table")$name1, 
        DBI::dbGetQuery(con,"SELECT * FROM my_table")$name2))})
    
    output$names_selector <- renderUI({
        selectInput("usr", "Чаёвник", choices = users()) 
    })

    name_toshow <- eventReactive(input$refresh, {input$usr})
    observe({
        input$refresh
        output$little_title <- renderUI({paste0("Результат. Для ", 
                                                name_toshow(),
                                                " найдены следующие чаепития:")})
    })
    
    # observeEvent(input$refresh, {
        df <- eventReactive(
            input$refresh, {
            output$names_selector <- renderUI({
                selectInput("usr", "Чаёвник", choices = users()) 
            })
            DBI::dbGetQuery(con,"SELECT * FROM my_table") %>% 
                filter(name1 == input$usr | name2 == input$usr) %>% 
                transmute(
                    `С кем:` = case_when(name1 == input$usr ~ name2, TRUE ~ name1), 
                    `Когда:` = as.character(dat), 
                    `Подтверждает:` = proof)
        })
        
        output$res_table <- renderTable({slice(df(), 1:input$rowstoshow)})
        
        observeEvent(input$refresh, {
            updateNumericInput(session, inputId = "rowstoshow", 
                               max = nrow(df()), value = nrow(df()))
        })
        
        
        
        observeEvent(input$record, {
            i_last <- data.frame(name1 = input$i_name1, name2 = input$i_name2, 
                       dat = str_replace_all(as.character(input$i_dat), "-", "/"), 
                       proof = input$i_proof)
            i_succ <- DBI::dbWriteTable(con, "my_table", i_last, 
                                  append = TRUE, row.names = FALSE)
            if(i_succ){
                showNotification("Записано благополучно!", type = "message")
            } else {
                showNotification("Что-то не благополучно...", type = "error")
            }
            updateTextInput(session, inputId = "i_name1", value = "")
            updateTextInput(session, inputId = "i_name2", value = "")
            updateDateInput(session, inputId = "i_dat", value = NULL)
            updateTextInput(session, inputId = "i_proof", value = "")
        })

}
# Run the application 
shinyApp(ui = ui, server = server, 
    onStart = function() {
        onStop(function() {
            dbDisconnect(con)
        })
    }
)