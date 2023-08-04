# initial -----------------------------------------------------------------
library(shiny)
library(shinyalert)
library(RPostgreSQL)
library(RSQLite)
library(tidyverse)

# my_table <- data.frame(name1 = c("Petir", "Mike", "Norman", "Borman", "Xi", "Xi"),
#            name2 = c("Anna", "Anna", "Borman", "Norman", "Petir", "Anna"),
#            dat = as.Date(c("02/12/22", "12/02/23", "01/10/21",
#                            "01/01/00", "26/07/89", "26/07/23"),
#                          format = "%d/%m/%y"),
#            proof = c("Jon", "John", "Johan", "Joe", "Ju", "Go"))
# users <-  data.frame(tlg_user_id = 99999999,
#                     name = "windows.user",
#                     hash = cli::hash_md5("pass"), 
#                     hash_date = Sys.time())

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
if(!str_detect(sessionInfo()$platform, "linux")){
    dbSendQuery(con, paste0(
        "UPDATE users SET hash_date = '", 
        Sys.time() |>
            capture.output() |>
            str_replace_all('\\[1] |"', "") |> 
            str_replace(" \\+", "+"),
        "' WHERE name = 'windows.user';")
    )
}
# dbGetQuery(con, "SELECT * FROM my_table")
# dbGetQuery(con, "SELECT * FROM users")

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
# Server logic ------------------------------------------------------------
        # initial variables (for current session)
    shinyjs::disable("deauth")
    status <- reactiveVal("no")
    current_language <- reactiveVal("RU")
    current_user <- reactiveVal(NULL)
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
    # languange 
    observeEvent(input$change_language, {
        if(current_language() == "RU") {
            updateActionButton(session, "change_language", label = "EN")
            current_language("EN")
        } else { 
            updateActionButton(session, "change_language", label = "RU")
            current_language("RU")
        }
    })
    
# Log in ------------------------------------------------------------------
    observeEvent(input$auth, {
        if(nchar(input$pass) < 1) {
            shinyalert::shinyalert(title = "Ошибка", text = "Введите пароль", type = "error")
        } else if(nchar(input$pass) < 4) {
            shinyalert::shinyalert(title = "Ошибка", text = "Пароль слишком короткий", type = "error")
        } else {
            md5pass = cli::hash_md5(toupper(input$pass))
            current_user(DBI::dbGetQuery(con, 
                paste0("SELECT * FROM users WHERE hash = '", md5pass, "';")))
            if(nrow(current_user()) != 1){
                shinyalert::shinyalert(title = "Ошибка", text = "Пароль неверный", type = "error")
            } else if(difftime(Sys.time(), current_user()$hash_date, units = "mins") > 30){
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
                                                     current_user()$name[[1]], 
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
                                             current_user()$name[[1]],
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
                    input$i_proof == "" ~ current_user()$name[[1]], 
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
    # tags$style(HTML("")),
    tags$head(tags$link(rel="shortcut icon", 
                        href="icons8-favicon-96.png")),
    actionButton("change_language", "RU", 
                 # label = current_language(),
                 icon = icon("globe"),
                 style = "position: absolute; top: 8px; right: 5px; z-index:10000;"),
    navbarPage(
        title = tags$div(style="position: relative; margin-right: 90px", 
                 tags$img(src="logo_placeholder.svg", height = "70px"),
                 tags$p(style="position: relative; top: -70px; left: 90px; ", 
                         "Faunistica 2.0")
        ),
        windowTitle = "Faunistica 2.0",
        position = "fixed-top",
        tabPanel("Главная", 
                 h2("О проекте в пяти предложениях:"), 
                 HTML(paste0("<p>Описание проекта для внешней аудитории. <br>", 
                    "4-5 предложений, где будет обоснование важности проекта с научной и социальной точки зрения. <br>", 
                    "Для каких задач нужны волонтеры и как они могут участвовать? <br> </p> <ul>")), 
                 HTML("<li>1. Научных публикаций тысячи и это число прирастает лавинообразно. </li>"),
                 HTML("<li>2. Поиск первичной информации в ручном режиме стал тормозить ученых((( </li>"),
                 HTML("<li>3. Результаты проекта повысят скорость и эффективность исследований окружающей среды. </li>"),
                 HTML("<li>4. Волонтеры будут оцифровывать научные статьи и структурировать информацию из них на онлайн-платформе. </li>"),
                 HTML("<li>5. Волонтеры смогут внести свой вклад в научный прогресс, получить доступ к эксклюзивным материалам и мероприятиям, а также (возможно) побороть свою арахнофобию.
                      </li></ul> <br>"),
                 h2("А теперь подробности"),
                 h3("Цель научного исследования"),
                 HTML(" <br> <ul><li>1. Поиск сведений о находках живых организмов - обязательный этап каждого исследования окружающей среды. </li>"),
                 HTML("<li>2. Традиционное решение этой задачи - тотальный просмотр всех научных публикаций - простое, но отнимает чрезвычайно много времени, сил и других ресурсов. </li>"),
                 HTML("<li>3. Необходим переход к использованию средств быстрого и эффективного поиск этих данных, без необходимости смотреть каждую статью вручную. </li>"),
                 HTML("<li>4. Такие средства есть в готовом виде, есть шаблонные решения с возможностью доработки под конкретные задачи, но…  Нет самих данных. </li>"),
                 HTML("<li>5. Цель проекта - разработать подходы и технические средства оцифровки литературных данных по биоразнообразию.  </li>"),
                 HTML("<li>6. Пауки Урала будут удобной модельной группой для разработки и оптимизации этих решений.</li> </ul>"),
                 h3("Планируемый результат исследования"),
                 HTML(" <br> <ul><li>1. Разработан инструмент оцифровки литературных источников, (веб-приложение, база данных, команда волонтеров).</li>"),
                 HTML("<li>2. Создана база данных с извлеченными из литературы сведениями (вот этот этап требует привлечения волонтеров)</li>"),
                 HTML("<li>3. Обеспечен доступ к полученным данным, к инструментам их анализа.</li> </ul>"),
                 h3("Роль волонтеров в научном проекте"),
                 HTML("Как результаты деятельности волонтёров помогут нашему исследованию?"),
                 HTML(" <br> <ul><li>Нам нужна помощь волонтеров в распознавании и структурировании сведений о находках пауков из предложенных научных статей: кого, где, когда и кто нашел. </li>"),
                 HTML("<li>Промежуточная задача - распознавание информации из научных статей и книг, наполнение ею базы данных. </li>"),
                 HTML("<li>Глобальная задача - организация свободного доступа ко всем литературным сведениям о находках живых организмов, предоставление к ней средств поиска.</li>")
                 ),
        
        navbarMenu("О проекте",
                   "----",
                   "Для волонтеров",
                   tabPanel("Польза для науки",
                            HTML("<br>"),
                            h3("Глобальная цель проекта, задачи"),
                            HTML("Цель Цель Цель Цель Цель Цель Цель Цель Цель Цель Цель <br>"),
                            HTML("Задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи"),
                            h3("Как результаты деятельности волонтёров помогают нашему исследованию"),
                            HTML("(какую промежуточную задачу исследования решают волонтёры)"),
                            tags$img(src = "dog.jpeg", width = 500)
                            
                            ),
                   tabPanel("Польза для вас",
                            HTML("<br>"),
                            h3("В чем профит для сообщества волонтеров и исследователей"),
                            HTML("<ul><li>Популяризация изучения биоразнообразия</li>"),
                            HTML("<li>Показать коллегам и населению, что изучение биоразнобразия это не только натурализм, но высокотехнологичная обработка больших данных.</li>"),
                            HTML("<li>Привлечение молодых исследователей в магистратуру и аспирантуру</li>"),
                            HTML("<li>Перспектива масштабирования до других регионов и групп живых организмов</li></ul> <br> "),
                            h3("В чем профит лично для вас"),
                            HTML("Активные участники проекта по оцифровке могут рассчитывать на: <ul>"),
                            HTML("<li>Возможность внести свой вклад в науку</li>"),
                            HTML("<li>Рейтинг, почетные звания, официальные благодарственные письма</li>"),
                            HTML("<li>Информацию о распространении и образе жизни тех, кого волонтер только что оцифровал </li>"),
                            HTML("<li>Оффлайн активности: обсуждения, экскурсии, лекции, фестивали, квесты, митапы</li>"),
                            HTML("<li>Онлайн чемпионаты, челенджи и марафоны: топ видов, топ авторов, топ точек и т.д.</li>"),
                            HTML("<li>Мерч: футболки, значки, нашивки</li>"),
                            HTML("<li>Возможность выполнить дипломную работу или диссертацию по этой теме и/или на этой базе данных </li>"),
                            HTML("<li>Соавторство в научных публикациях для наиболее активных и продуктивных участников, упоминание в благодарностях для среднеактивных</li><ul>"),
                            tags$img(src = "icons8-favicon-96.png", width = 500)
                   ),
                   tabPanel("Как нам помочь",
                            HTML("<br>"),
                            h4("Мы записали для вас несколько роликов, которые помогут вам лучше понять как устроен наш проект научного волонтерства и каким именно образом вы можете ему помочь."),
                            HTML("<br>"),
                            h4("Для чего и как изучают биоразнообразие"),
                            HTML('<iframe width="560" height="315" 
                                 src="https://www.youtube.com/embed/HgTwE_U6m0U" 
                                 frameborder="0" allow="accelerometer; autoplay; encrypted-media; 
                                 gyroscope; picture-in-picture" allowfullscreen></iframe>'), 
                            HTML("<br>"),
                            h4("Бинарная номенклатура - научное название для живых существ"),
                            HTML('<iframe width="560" height="315" 
                                 src="https://www.youtube.com/embed/pyc1cNDqwKE" 
                                 frameborder="0" allow="accelerometer; autoplay; encrypted-media; 
                                 gyroscope; picture-in-picture" allowfullscreen></iframe>'), 
                            HTML("<br>"),
                            h4("Научная этикетка и фаунистическая публикация"),
                            HTML('<iframe width="560" height="315" 
                                 src="https://www.youtube.com/embed/pyc1cNDqwKE" 
                                 frameborder="0" allow="accelerometer; autoplay; encrypted-media; 
                                 gyroscope; picture-in-picture" allowfullscreen></iframe>'), 
                            HTML("<br>"),
                            h4("Процедура ввода данных и интерфейс программы для этого"),
                            HTML('<iframe width="560" height="315" 
                                 src="https://www.youtube.com/embed/1DPo-STKFwg" 
                                 frameborder="0" allow="accelerometer; autoplay; encrypted-media; 
                                 gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                            h4("Полученные к сегодняшнему дню результаты"),
                            HTML('<iframe width="560" height="315" 
                                 src="https://www.youtube.com/embed/pyc1cNDqwKE" 
                                 frameborder="0" allow="accelerometer; autoplay; encrypted-media; 
                                 gyroscope; picture-in-picture" allowfullscreen></iframe>'), 
                            HTML("<br>"),
                            
                            
                   ),
                   tabPanel("Наш волонтерский проект",
                            HTML("<br>"),
                            h3("Схема проекта", align = "center"),
                            HTML('<center><img src="Схема_проекта.svg" width="68%"></center>'),
                            h3("Этапы реализации", align = "center"),
                            HTML("<b>Этап 1.</b> Разработка веб-приложения и  архитектуры базы данных<br> <br>"),
                            icon("arrows-down-to-line"), HTML("<i>Вы находитесь здесь</i><br>"),
                            HTML("<b>Этап 2.</b> Тестовый запуск проекта<br>"),
                            icon("arrows-up-to-line"), HTML("<i>Вы находитесь здесь</i><br> <br>"),
                            HTML("<b>Этап 3.</b> Внесение корректировок<br>"),
                            HTML("<b>Этап 4.</b> Запуск проекта в полную силу<br>"),
                            HTML("<b>Этап 5.</b> Внесение корректировок<br>"),
                            h3("Команда", align = "center"),
                            fluidRow(
                                column(width = 2, tags$img(src = "sozontov.jpg", width = "100%", `data-action`="zoom"), 
                                       ), 
                                column(width = 10, h4("Созонтов Артём"), HTML("34 года, к.б.н., <br>
Институт экологии растений и животных УрО РАН<br>
Уральский федеральный университет<br>
<br> 
<b>Руководитель</b><br>
Описание роли описание роли описание роли описание роли описание роли описание роли"))),
                            fluidRow(HTML("<br>")),
                            fluidRow(
                                column(width = 2, HTML('<img src="ivanova.jpg" data-action="zoom" width="100%">')
                                       # tags$img(src = "ivanova.jpg", width = "100%", `data-action`="zoom"), 
                                ), 
                                column(width = 10, h4("Иванова Наталья"), HTML("33 года, к.б.н., <br>
Институт математических проблем биологии – филиал Института прикладной математики им. М.В. Келдыша РАН<br>
<br> 
<b>Аудитор данных</b><br>
Описание роли описание роли описание роли описание роли описание роли описание роли"))),
                            fluidRow(HTML("<br>")),
                            fluidRow(
                                column(width = 2, tags$img(src = "sokolova.jpg", width = "100%"), 
                                ), 
                                column(width = 10, h4("Соколова Софья"), HTML("31 год, аспирант<br>
Южно-Уральский Федеральный научный центр Минералогии и геоэкологии УрО РАН, <br>
Институт экологии растений и животных УрО РАН<br>
<br> 
<b>Технический писатель</b><br>
Описание роли описание роли описание роли описание роли описание роли описание роли"))),
                            fluidRow(
                                column(width = 2, tags$img(src = "plakhina.jpg", width = "100%"), 
                                ), 
                                column(width = 10, h4("Плакхина Евгения"), HTML("37 лет, аспирант<br>
Пермский национальный исследовательский университет<br>
<br> 
<b>Библиограф</b><br>
Описание роли описание роли описание роли описание роли описание роли описание роли"))),
                            fluidRow(
                                column(width = 2, tags$img(src = "ustinova.jpg", width = "100%"), 
                                ), 
                                column(width = 10, h4("Устинова Анастасия"), HTML("23 года, аспирант<br>
Институт экологии растений и животных УрО РАН<br>
<br> 
<b>Коммуникатор</b><br>
Описание роли описание роли описание роли описание роли описание роли описание роли")))
                   ),
                   "----",
                   "Для специалистов",
                   tabPanel("Сотрудничество",
                            HTML("<br>"),
                            tags$img(src = "icons8-favicon-96.png", width = 500)
                            # uiOutput("names_selector")
                   ),
                   tabPanel("Наше веб-прилжоение",
                            HTML("<br>"),
                            tags$img(src = "dog.jpeg", width = 500)
                            # tableOutput("res_table")
                            ),
                   "----"
        ),
        tabPanel("Статистика",
            h3("Здесь пока только статистика по чаепитиям,", align = "center"), 
            h3("но скоро будет реальная наука!", align = "center"), 
            tabsetPanel(
                tabPanel("Общая",
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
                ))), 
                tabPanel("Персональная")
        )),
        tabPanel("Участвовать!", 
            HTML("<br>"),
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
                dateInput("i_dat", "Дата чаепития", format = "yyyy/mm/dd", weekstart = 1),
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
        
        
        )
)

# Run the application 
shinyApp(ui = ui, server = server,  
    options = list(launch.browser = FALSE),
    onStart = function() {
        onStop(function() {
            rm(list = ls())
            dbDisconnect(con)
        })
    }
)