# initial -----------------------------------------------------------------
library(shiny)
library(shinyalert)
library(RPostgreSQL)
library(RSQLite)
library(tidyverse)

my_table <- data.frame(name1 = c("Petir", "Mike", "Norman", "Borman", "Xi", "Xi"),
           name2 = c("Anna", "Anna", "Borman", "Norman", "Petir", "Anna"),
           dat = as.Date(c("02/12/22", "12/02/23", "01/10/21",
                           "01/01/00", "26/07/89", "26/07/23"),
                         format = "%d/%m/%y"),
           proof = c("Jon", "John", "Johan", "Joe", "Ju", "Go"))
users <-  data.frame(tlg_user_id = 99999999,
                    name = "windows.user",
                    hash = cli::hash_md5("pass"), 
                    hash_date = Sys.time())

# dbWriteTable(con, "users", users, overwrite = TRUE)
# dbWriteTable(con, "my_table", my_table, overwrite = TRUE)
con <- if(!str_detect(sessionInfo()$platform, "linux")){
    dbConnect(RSQLite::SQLite(), "win_db.sqlite")
} else {
    dbConnect(dbDriver("PostgreSQL"),
                 dbname = "rdatabase",
                 host = "localhost",
                 port = "5432",
                 user = "ruser",
                 password = scan("/var/mypass", what = ""))
}

dbGetQuery(con, "SELECT * FROM my_table")
dbGetQuery(con, "SELECT * FROM users")

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
# Server logic ------------------------------------------------------------
        # initial variables (for current session)
    shinyjs::disable("deauth")
    status <- reactiveVal("no")
    users <- reactiveVal({
        DBI::dbGetQuery(con,"SELECT * FROM my_table") %>% 
        select(name1, name2) %>% 
        as_vector() %>% 
        unique() %>% 
        sort()
    })
    
    output$names_selector <- renderUI({
        selectInput("usr", "Чаёвник", choices = users()) 
    })
    
# Log in ------------------------------------------------------------------
    observeEvent(input$auth, {
        if(nchar(input$pass) < 1) {
            shinyalert::shinyalert(title = "Ошибка", text = "Введите пароль", type = "error")
        } else if(nchar(input$pass) < 4) {
            shinyalert::shinyalert(title = "Ошибка", text = "Пароль слишком короткий", type = "error")
        } else {
            md5pass = cli::hash_md5(toupper(input$pass))
            current_user <- DBI::dbGetQuery(con, paste0("SELECT * FROM users WHERE hash = '", 
                                                         md5pass, 
                                                         "';"))
            if(nrow(current_user) != 1){
                shinyalert::shinyalert(title = "Ошибка", text = "Пароль неверный", type = "error")
            } else if(difftime(Sys.time(), current_user$hash_date, units = "mins") > 30){
                shinyalert::shinyalert(title = "Пароль устарел", 
                                       text = "Сгенерируйте пароль заново в телеграм-боте", type = "warning")
            } else {
                shinyjs::disable("auth")
                shinyjs::enable("deauth")
                shinyjs::disable("pass")
                status("yes")
                output$currentstatus <- renderText(status())
                shinyalert::shinyalert(title = "Вход в систему", 
                                       text = paste0("Вы успешно залогинились! ",
                                                     "\nРад приветствовать вас, ",
                                                     current_user$name[[1]], 
                                                     "! \n \nВозможность записи: ", 
                                                     status()),
                                       type = "success")
            }
        }
    })

# Log out -----------------------------------------------------------------
    observeEvent(input$deauth, {
        shinyjs::enable("auth")
        shinyjs::disable("deauth")
        shinyjs::enable("pass")
        status("no")
        output$currentstatus <- renderText(status())
        shinyalert::shinyalert(title = "Выход из системы", 
                               text = paste0("Вы успешно вышли из своей учетной записи!", 
                                             "\nДо новых встреч, ", 
                                             current_user$name[[1]],
                                             "! \n \nВозможность записи: ", 
                                             status()),
                               type = "info")
    })

# Record ------------------------------------------------------------------
    observeEvent(input$record, {
        if(status() == "no") { 
            shinyalert::shinyalert(
                title = "Вы не авторизованы!", 
                text = "Войдите в систему чтобы вносить новые записи", 
                type = "error")
        } else if(nchar(input$i_name1) == 0 | nchar(input$i_name2) == 0){
            shinyalert::shinyalert(
                title = "Некорректные данные", 
                text = "Имена не могут быть пустыми", 
                type = "warning")
        } else if(nchar(input$i_name1) > 15 | nchar(input$i_name2) > 15){
            shinyalert::shinyalert(
                title = "Некорректные данные", 
                text = "Таких длинных имён не бывает", 
                type = "warning")
        } else if(nchar(input$i_name1) < 3 | nchar(input$i_name2) < 3){
            shinyalert::shinyalert(
                title = "Некорректные данные", 
                text = "Имена не могут быть слишком короткими", 
                type = "warning")
        } else if(
            str_detect(input$i_name1, "[:digit:]") | 
            str_detect(input$i_name2, "[:digit:]")
            ){
            shinyalert::shinyalert(
                title = "Некорректные данные", 
                text = "Имён с цифрами не бывает", 
                type = "warning")
        } else if(
            str_detect(toupper(input$i_name1), stringr::regex("(.)\\1{2,}")) | 
            str_detect(toupper(input$i_name2), stringr::regex("(.)\\1{2,}"))
            ){
            shinyalert::shinyalert(
                title = "Некорректные данные", 
                text = "Пожалуйста, вводите существующие имена", 
                type = "warning")
        } else {
            i_last <- data.frame(
                name1 = input$i_name1, 
                name2 = input$i_name2, 
                dat = str_replace_all(as.character(input$i_dat), "-", "/"), 
                proof = dplyr::case_when(
                    input$i_proof == "" ~ current_user$name[[1]], 
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

# Refresh -----------------------------------------------------------------
    df <- eventReactive(input$refresh, {
        users(
            DBI::dbGetQuery(con,"SELECT * FROM my_table") %>% 
                    select(name1, name2) %>% 
                    as_vector() %>% 
                    unique() %>% 
                    sort()
        )
        
        output$little_title <- renderUI({
            paste0("Результат. Для ",
            name_toshow(),
            " найдены следующие чаепития:")})
        
        updateSelectInput(session, "usr",
                          choices = users(), 
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
    
    name_toshow <- eventReactive(input$refresh, {input$usr})
    
}

# UI ----------------------------------------------------------------------
ui <- fluidPage(
    shinyjs::useShinyjs(),
    tags$style(type="text/css", "body {padding-top: 70px;}"),
    tags$head(tags$link(rel="shortcut icon", 
                        href="icons8-favicon-96.png")),
    navbarPage(title = "Служба учёта чаепитий",
        position = "fixed-top",
        # icon = "icons8-favicon-96.png",
        tabPanel("Поиск состоявшихся",
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
        tabPanel("Ввод новых", 
            sidebarLayout(
            sidebarPanel(
                HTML(paste0("<h3>Авторизация</h3>", 
                    "<i>Перед вводом собственных данных необходимо авторизоваться. ", 
                    "Зарегистрироваться и получить пароль можно у", 
                    "<a href = https://t.me/faunistica_2_bot> телеграм-", 
                    "бота</a>.</i> <br> ")),
                passwordInput("pass", label = NULL, placeholder = "Введите пароль"),
                actionButton("auth", "Авторизоваться"),
                actionButton("deauth", "Выйти из системы"),
                HTML("<br> <br>"), 
                textInput("i_name1", "Чаёвник1"),
                textInput("i_name2", "Чаёвник2"),
                dateInput("i_dat", "Дата чаепития", format = "yyyy/mm/dd"),
                textInput("i_proof", placeholder = "Ваше имя (опционально)",
                    label = "Я видел это своими глазами и подтверждаю факт чаепития"),
                HTML("<br>"),
                actionButton("record", "Записать!"),
                HTML("<br>"),
                HTML("<br>")
            ), 
            mainPanel(
                textOutput("currentstatus"),
                tags$img(src = "dog.jpeg", width = 500)
            )
            )
        )
        # tabPanelBackground = "blue",
        
        # navbarMenu("More",
        #            tabPanel("Summary"),
        #            "----",
        #            "Section header",
        #            tabPanel("Table")
        # )
        )
)

# Run the application 
shinyApp(ui = ui, server = server, 
    onStart = function() {
        onStop(function() {
            rm(list = ls())
            dbDisconnect(con)
        })
    }
)