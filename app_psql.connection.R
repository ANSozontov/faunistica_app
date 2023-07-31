# initial -----------------------------------------------------------------
library(shiny)
library(shinyalert)
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
users <- sort(unique(c(
    DBI::dbGetQuery(con,"SELECT * FROM my_table")$name1, 
    DBI::dbGetQuery(con,"SELECT * FROM my_table")$name2
    )))

# UI ----------------------------------------------------------------------
ui <- fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("Служба автоматизированного учёта чаепитий"),
        tabsetPanel(
            tabPanel("Статистика чаепитий",
            sidebarLayout(
                sidebarPanel(
                uiOutput("names_selector"),
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
                actionButton("record", "Записать!"),
                HTML("<br>"),
                HTML("<br>"),
                HTML("<h3>Авторизация</h3>"),
                passwordInput("pass", "Введите пароль"),
                actionButton("auth", "Авторизоваться"),
                actionButton("deauth", "Выйти из системы"),
                HTML("<br>"),
                HTML("<br>"),
                HTML(paste0("Зарегистрироваться и получить пароль можно у", 
                "<a href = https://t.me/faunistica_2_bot> телеграм-", 
                "бота</a> `faunistica_2_bot`."))
            ), 
            mainPanel(
                textOutput("currentstatus"),
                tags$img(src = "dog.jpeg", width = 500)
            )
            )
            )
        )
)

status <- "no"

server <- function(input, output, session) {
    shinyjs::disable("deauth")
    
    
    # output$currentstatus <- renderText(status())
    
    observeEvent(input$auth, {
        if(nchar(input$pass) < 1) {
            shinyalert::shinyalert(title = "Ошибка", text = "Введите пароль", type = "error")
        } else if(nchar(input$pass) < 4) {
            shinyalert::shinyalert(title = "Ошибка", text = "Пароль слишком короткий", type = "error")
        } else {
        md5pass = cli::hash_md5(toupper(input$pass))
        df <<- DBI::dbGetQuery(con, paste0("SELECT * FROM users WHERE hash = '", 
                                          md5pass, 
                                          "';"))
        if(nrow(df) != 1){
            shinyalert::shinyalert(title = "Ошибка", text = "Пароль неверный", type = "error")
        } else if(difftime(Sys.time(), df$hash_date, units = "mins") > 10){
            shinyalert::shinyalert(title = "Пароль устарел", 
                text = "Сгенерируйте пароль заново в телеграм-боте", type = "warning")
        } else {
            shinyjs::disable("auth")
            shinyjs::enable("deauth")
            shinyjs::disable("pass")
            # editable <<- "yes"
            status <<- "yes"
            shinyalert::shinyalert(title = "Вход в систему", 
                text = paste0("Вы успешно залогинились! ",
                    "\nРад приветствовать вас, ",
                    df$name[[1]], 
                    "! \n \nВозможность записи: ", 
                    status),
                type = "success")
        }
        }
    })
    observeEvent(input$deauth, {
        shinyjs::enable("auth")
        shinyjs::disable("deauth")
        shinyjs::enable("pass")
        status <<- "no"
        shinyalert::shinyalert(title = "Выход из системы", 
            text = paste0("Вы успешно вышли из своей учетной записи!", 
                          "\nДо новых встреч, ", 
                          df$name[[1]],
                          "! \n \nВозможность записи: ", 
                          status),
            type = "info")
    })

    output$names_selector <- renderUI({
        selectInput("usr", "Чаёвник", choices = users) 
    })

    name_toshow <- eventReactive(input$refresh, {input$usr})

    # How to react on `refresh` button
    df <- eventReactive(input$refresh, {
        users <<- sort(unique(c(
            DBI::dbGetQuery(con,"SELECT * FROM my_table")$name1, 
            DBI::dbGetQuery(con,"SELECT * FROM my_table")$name2
        )))
        
        output$little_title <- renderUI({paste0("Результат. Для ",
            name_toshow()," найдены следующие чаепития:")})
        
        updateSelectInput(session, "usr",
            choices = users, 
            selected = input$usr
        )

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
        
    # How to react on `record` button
    observeEvent(input$record, {
        if(status == "no") { 
            shinyalert::shinyalert(title = "Вы не авторизованы!", 
                text = "Войдите в систему чтобы вносить новые записи", type = "error")
        } else if(nchar(input$i_name1) == 0 | nchar(input$i_name2) == 0){
            shinyalert::shinyalert(title = "Некорректные данные", 
                text = "Имена не могут быть пустыми", type = "warning")
        } else if(nchar(input$i_name1) < 3 | nchar(input$i_name2) < 3){
            shinyalert::shinyalert(title = "Некорректные данные", 
                text = "Имена не могут быть слишком короткими", type = "warning")
        } else {
        i_last <- data.frame(name1 = input$i_name1, name2 = input$i_name2, 
                   dat = str_replace_all(as.character(input$i_dat), "-", "/"), 
                   # proof = input$i_proof) 
                   proof = dplyr::case_when(
                       input$i_proof == "" ~ df$name[[1]], 
                       TRUE ~ input$i_proof))
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
        }
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