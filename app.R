# initial -----------------------------------------------------------------
library(tidyverse)
library(DBI)
library(shiny)
library(shinyalert)
library(tippy)

# dbWriteTable(con, "users", users, overwrite = TRUE)
# dbWriteTable(con, "my_table", my_table, overwrite = TRUE)
con <- if(!str_detect(sessionInfo()$platform, "linux")){
    library(RSQLite)
    dbConnect(RSQLite::SQLite(), "win_db.sqlite")
} else {
    library(RPostgreSQL)
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

L <- readxl::read_xlsx("translation.xlsx") %>% 
    unite("key", block, key) %>% 
    transpose(.names = .$key)

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
# Server logic ------------------------------------------------------------
    # language 
    r <- function(X){ # translates text into current language
        txt <- sapply(X,function(s) L[[s]][[current_language()]], USE.NAMES=FALSE)
        # parse some code to html 
        if(substr(txt, 1, 4) == "ulli"){ 
            txt <- txt %>%
                stringr::str_replace_all("ulli_", "") %>% 
                stringr::str_replace_all("; ", "</li><li>") %>% 
                paste0("<ul><li>", ., "</li></ul>")
        }
        txt
    }
    
    current_language <- reactiveVal("ru")
    
    observeEvent(input$change_language, {
        if(current_language() == "ru") {
            updateActionButton(session, "change_language", label = "EN")
            showNotification("English language selected", type = "message")
            current_language("en")
            # } else if(current_language() == "en") { 
            #     updateActionButton(session, "change_language", label = "KZ")
            #     current_language("kz")
            # } else if(current_language() == "kz") { 
            #     updateActionButton(session, "change_language", label = "UA")
            #     current_language("ua")
        } else{
            updateActionButton(session, "change_language", label = "RU")
            showNotification("Выбран русский язык", type = "message")
            current_language("ru")
        }
    })
    
    # initial variables (for current session)
    status <- reactiveVal("no")
    
    holds <- reactiveVal(0)
    # output$test <- renderPrint(holds())
    
    popup.info <- reactive({
        tippy(HTML('<p style="font-size:24px;text-align:right">🛈</p>'), 
          tooltip = r("srv_hold"), 
          trigger = "mouseover",
          theme = "material")
    })
    
    # shinyjs::disable("deauth")
    # 
    # shinyjs::disable("unhold_adm")
    # shinyjs::disable("unhold_geo")
    # shinyjs::disable("unhold_ev")
    # shinyjs::disable("unhold_taxa")
    
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
                shinyalert::shinyalert(title = "Вход в систему", 
                                       text = paste0("Вы успешно залогинились! ",
                                                     "\nРад приветствовать вас, ",
                                                     current_user()$name[[1]], 
                                                     "! \n \nВозможность записи: ", 
                                                     status()),
                                       type = "success")
                shinyjs::disable("auth")
                shinyjs::enable("deauth")
                shinyjs::disable("pass")
            }
        }
    })

# Log out -----------------------------------------------------------------
    observeEvent(input$deauth, {
        shinyjs::enable("auth")
        shinyjs::disable("deauth")
        shinyjs::enable("pass")
        output$currentstatus <- renderText(status())
        shinyalert::shinyalert(title = "Выход из системы", 
                               text = paste0("Вы успешно вышли из своей учетной записи!", 
                                             "\nДо новых встреч, ", 
                                             current_user()$name[[1]],
                                             "! \n \nВозможность записи: ", 
                                             status()),
                               type = "info")
    })


# Check -------------------------------------------------------------------

    
# Record ------------------------------------------------------------------
    # observeEvent(input$record, {
    #     if(status() == "no") { 
    #         shinyalert::shinyalert(
    #             title = "Вы не авторизованы!", 
    #             text = "Войдите в систему чтобы вносить новые записи", 
    #             type = "error")
    #     } else if(nchar(input$i_name1) == 0 | nchar(input$i_name2) == 0){
    #         shinyalert::shinyalert(
    #             title = "Некорректные данные", 
    #             text = "Имена не могут быть пустыми", 
    #             type = "warning")
    #     } else if(nchar(input$i_name1) > 15 | nchar(input$i_name2) > 15){
    #         shinyalert::shinyalert(
    #             title = "Некорректные данные", 
    #             text = "Таких длинных имён не бывает", 
    #             type = "warning")
    #     } else if(nchar(input$i_name1) < 3 | nchar(input$i_name2) < 3){
    #         shinyalert::shinyalert(
    #             title = "Некорректные данные", 
    #             text = "Имена не могут быть слишком короткими", 
    #             type = "warning")
    #     } else if(
    #         str_detect(input$i_name1, "[:digit:]") | 
    #         str_detect(input$i_name2, "[:digit:]")
    #         ){
    #         shinyalert::shinyalert(
    #             title = "Некорректные данные", 
    #             text = "Имён с цифрами не бывает", 
    #             type = "warning")
    #     } else if(
    #         str_detect(toupper(input$i_name1), stringr::regex("(.)\\1{2,}")) | 
    #         str_detect(toupper(input$i_name2), stringr::regex("(.)\\1{2,}"))
    #         ){
    #         shinyalert::shinyalert(
    #             title = "Некорректные данные", 
    #             text = "Пожалуйста, вводите существующие имена", 
    #             type = "warning")
    #     } else {
    #         i_last <- data.frame(
    #             name1 = input$i_name1, 
    #             name2 = input$i_name2, 
    #             dat = str_replace_all(as.character(input$i_dat), "-", "/"), 
    #             proof = dplyr::case_when(
    #                 input$i_proof == "" ~ current_user()$name[[1]], 
    #                 TRUE ~ input$i_proof))
    #         i_succ <- DBI::dbWriteTable(con, "my_table", i_last, 
    #                                     append = TRUE, row.names = FALSE)
    #         if(i_succ){
    #             showNotification("Записано благополучно!", type = "message")
    #         } else {
    #             showNotification("Что-то не благополучно...", type = "error")
    #         }
    #         updateTextInput(session, inputId = "i_name1", value = "")
    #         updateTextInput(session, inputId = "i_name2", value = "")
    #         updateDateInput(session, inputId = "i_dat", value = NULL)
    #         updateTextInput(session, inputId = "i_proof", value = "")
    #     }
    # })


# Clear / drop ------------------------------------------------------------
    observeEvent(input$drop, { 
        if(holds()!=0) { 
            shinyalert::shinyalert(
                title = "Невозможно", 
                text = "Не могу сбросить введённые данные\nЧасть блоков зафиксирована", 
                type = "warning")
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

# PAGE home ----------------------------------------------------------------
    output$p_home <- renderUI(tagList(
        actionButton("change_language", "RU", 
                     icon = icon("globe"),
                     style = "position: absolute; top: 8px; right: 5px; z-index:10000;"),
        # h2("О проекте в пяти предложениях:"), 
        h2(r("home_brief")),
        HTML(paste0("<p>Описание проекта для внешней аудитории. <br>", 
                    "4-5 предложений, где будет обоснование важности проекта с научной и социальной точки зрения. <br>", 
                    "Для каких задач нужны волонтеры и как они могут участвовать? <br> </p>")), 
        HTML(r("home_summary")),
        h2(r("home_details")),
        h3(r("home_aim")),
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
    ))
    


# PAGE team ---------------------------------------------------------------
    output$p_team <- renderUI(tagList(
        h3(r("team_team"), align = "center"), 
        fluidRow(
            column(width = 2, tags$img(src = "sozontov.jpg", width = "100%", `data-action`="zoom")), 
            column(width = 10, 
                h4(r("team_sozontov1")), 
                HTML(r("team_sozontov2")), 
                HTML(paste0("<br><b>", r("team_sozontov3"), "</b><br>")),
                HTML(r("team_sozontov4"))
            )
        ),
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
    ))

# PAGE scientific profit ---------------------------------------------------
    output$p_sci.profit <- renderUI(tagList(
        HTML("<br>"),
        h3("Глобальная цель проекта, задачи"),
        HTML("Цель Цель Цель Цель Цель Цель Цель Цель Цель Цель Цель <br>"),
        HTML("Задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи"),
        h3("Как результаты деятельности волонтёров помогают нашему исследованию"),
        HTML("(какую промежуточную задачу исследования решают волонтёры)"),
        tags$img(src = "dog.jpeg", width = 500)
    ))

# PAGE your personal profit ------------------------------------------------
    output$p_your.profit <- renderUI(tagList(
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
    ))

# PAGE how to help ---------------------------------------------------------
    output$p_howtohelp <- renderUI(tagList(
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
        HTML("<br>")
    ))

# PAGE our project ---------------------------------------------------------
    output$p_our.project <- renderUI(tagList(
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
        HTML("<b>Этап 5.</b> Внесение корректировок<br>")
    ))
    

# PAGE cooperation ---------------------------------------------------------
    output$p_cooperation <- renderUI(tagList(
        HTML("<br>"),
        tags$img(src = "icons8-favicon-96.png", width = 500)
    ))

# PAGE our web application -------------------------------------------------
    output$p_web.app <- renderUI(tagList(
        HTML("<br>"),
        tags$img(src = "dog.jpeg", width = 500)
    ))

# PAGE statistics general --------------------------------------------------
    output$p_stats.general <- renderUI(tagList(
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
            ))
    ))

# PAGE statistics personal -------------------------------------------------
    output$p_stats.personal <- renderUI(tagList(
        h3("Здесь будет ваша персональная статистика"),
        h5("А может и не будет...")
    ))

# PAGE statistics ----------------------------------------------------------
    output$p_statistics <- renderUI(tagList(
        br(), 
        br(),
        renderPrint(paste0("hold blocks: ", holds())),
        br(), 
        renderPrint(paste0("status = ", status())),
        br(),
        h3("Здесь пока только статистика по чаепитиям,", align = "center"), 
        h3("но скоро будет реальная наука!", align = "center"), 
        tabsetPanel(
            tabPanel("Общая", uiOutput("p_stats.general")), 
            tabPanel("Персональная", uiOutput("p_stats.personal"))
        )
    ))

# PAGE input new data: auth ----------------------------------------------------
    output$i_auth <- renderUI(tagList(
        h3(r("i_auth.title"), align = "center"),
        br(),
        fluidRow(
            column(width = 3,
                HTML(text = case_when(status() == "no" ~ r("i_auth.no"), TRUE ~ r("i_auth.yes")))
            ),
            column(3, 
                HTML(paste0(
                    "<p>",
                    r("i_auth.text"), 
                    ' <a href = "https://t.me/faunistica_2_bot" target="_blank">', 
                    r("i_link.text"), 
                    ".</a></p>"))
                ),
            column(width = 3, passwordInput("pass", label = NULL, placeholder = r("i_passwd.fill"))), 
            column(width = 3, 
                column(width = 6, actionButton("auth", r("i_auth.in"), width = '80%', style="float:left")), # width = '100%'
                column(width = 6, actionButton("deauth", r("i_auth.out"), width = '80%', disabled = TRUE)) #  width = '100%'
            )
        )
     ))
    
# PAGE input new data: adm ----------------------------------------------------
    output$i_adm <- renderUI(tagList(
        h3(r("i_adm.title"), align = "center", style = "font-size: 2em"), 
        br(),
        fluidRow(
            column(width = 3, textInput("country",   r("i_adm0"))),
            column(width = 3, textInput("region",    r("i_adm1"))),
            column(width = 3, textInput("district",  r("i_adm2"))), 
            column(width = 3, textInput("loc", r("i_loc"), 
                                        placeholder = r("i_loc.fill"))) 
        ),
        fluidRow(
            column(width = 9), 
            column(width = 1, popup.info()),
            column(width = 1, actionButton("hold_adm", "",  icon = icon("lock"), style="float:right")), 
            column(width = 1, actionButton("unhold_adm", "", icon = icon("lock-open"), disabled = TRUE, style="float:left"))
        )
    ))
    
    observeEvent(input$hold_adm, {
        holds(holds()+1)
        shinyjs::enable("unhold_adm")
        shinyjs::disable("country")
        shinyjs::disable("region")
        shinyjs::disable("district")
        shinyjs::disable("loc")
        shinyjs::disable("hold_adm")
    })
    
    observeEvent(input$unhold_adm, {
        holds(holds()-1)
        shinyjs::disable("unhold_adm")
        shinyjs::enable("country")
        shinyjs::enable("region")
        shinyjs::enable("district")
        shinyjs::enable("loc")
        shinyjs::enable("hold_adm")
    })

# PAGE input new data: geo ------------------------------------------------
    output$i_geo <- renderUI(tagList(
        h3(r("i_geo.title"), align = "center", style = "font-size: 2em"), 
        br(),
        fluidRow(
            column(width = 3, textInput("place",   r("i_geo1"))), 
            column(width = 3, textInput("geo.rem",    r("i_geo2"))),
            column(width = 2, textInput("NN",  label = r("i_coords"), placeholder = r("i_geo.fill"))),
            column(width = 1, br(), h4("N")),
            column(width = 3)
        ),
        fluidRow(
            column(width = 6), 
            column(width = 2, textInput("EE", label = NULL, placeholder = r("i_geo.fill"))), 
            column(width = 1, h4("E")), 
            column(width = 1, popup.info() ),
            column(width = 1, actionButton("hold_geo", "",  icon = icon("lock"), style="float:right")), 
            column(width = 1, actionButton("unhold_geo", "", icon = icon("lock-open"), disabled = TRUE, style="float:left"))
        )
    ))
    
    observeEvent(input$hold_geo, {
        holds(holds()+1)
        shinyjs::disable("place")
        shinyjs::disable("geo.rem")
        shinyjs::disable("NN")
        shinyjs::disable("EE")
        shinyjs::disable("hold_geo")
        shinyjs::enable("unhold_geo")
    })
    
    observeEvent(input$unhold_geo, {
        holds(holds()-1)
        shinyjs::enable("place")
        shinyjs::enable("geo.rem")
        shinyjs::enable("NN")
        shinyjs::enable("EE")
        shinyjs::enable("hold_geo")
        shinyjs::disable("unhold_geo")
    })
    
# PAGE input new data: event ----------------------------------------------
    output$i_event <- renderUI(tagList(
        h3(r("i_ev.title"), align = "center", style = "font-size: 2em"), 
        br(),
        fluidRow(
            column(width = 3, textInput("habitat", r("i_ev.hab"))),
            column(width = 3, dateInput("dat", r("i_ev.date"), startview = "month")),
            column(width = 3, textInput(
                "effort", 
                r("i_ev.effort"), 
                placeholder = r("i_ev.effort_fill"))),
            column(width = 3, textInput("event_rem", r("i_ev.rem")))
        ),
        fluidRow(
            column(width = 9), 
            column(width = 1, popup.info() ),
            column(width = 1, actionButton("hold_ev", "",  icon = icon("lock"), style="float:right")), 
            column(width = 1, actionButton("unhold_ev", "", icon = icon("lock-open"), disabled = TRUE, style="float:left"))
        )
    ))
    
    observeEvent(input$hold_ev, {
        holds(holds()+1)
        shinyjs::disable("habitat")
        shinyjs::disable("dat")
        shinyjs::disable("effort")
        shinyjs::disable("event_rem")
        shinyjs::enable("unhold_ev")
        shinyjs::disable("hold_ev")
    })
    observeEvent(input$unhold_ev, {
        holds(holds()-1)
        shinyjs::enable("habitat")
        shinyjs::enable("dat")
        shinyjs::enable("effort")
        shinyjs::enable("event_rem")
        shinyjs::enable("hold_ev")
        shinyjs::disable("unhold_ev")
    })

# PAGE input new data: taxa -----------------------------------------------
    output$i_taxa <- renderUI(tagList(
        h3(r("i_taxa.title"), align = "center", style = "font-size: 2em"), 
        br(),
        fluidRow(
            column(width = 3, textInput("fam", r("i_taxa.fam"))),
            column(width = 3, textInput("gen", r("i_taxa.gen"))),
            column(width = 3, textInput("sp",  r("i_taxa.sp"))),
            column(width = 3, selectInput("taxa_nsp", r("i_taxa.nsp"), choices = c(`✘` = "no", `✔`= "yes"))),
        ),
        fluidRow(
            column(width = 6, textInput("tax.rem", "taxanomical remarks", width = '94%')), 
            column(width = 3, selectInput("sp.def", r("i_taxa.sp.def"), choices = c(`✔`= "yes", `✘` = "no"))), # "species defined"
            column(width = 1, br(), popup.info()),
            column(width = 1, br(), actionButton("hold_taxa", "",  icon = icon("lock"), style="float:right")), 
            column(width = 1, br(), actionButton("unhold_taxa", "", icon = icon("lock-open"), disabled = TRUE, style="float:left"))
        )
    ))
    
    observeEvent(input$sp.def, {
        if(input$sp.def == "no"){
            updateTextInput(session, "sp", value = NA)
            shinyjs::disable("sp")
        }
        if(input$sp.def == "yes"){
            shinyjs::enable("sp")
        }
    })
    
    observeEvent(input$hold_taxa, {
        holds(holds()+1)
        shinyjs::disable("fam")
        shinyjs::disable("gen")
        shinyjs::disable("sp")
        shinyjs::disable("tax.rem")
        shinyjs::disable("taxa_nsp")
        shinyjs::disable("sp.def")
        shinyjs::enable("unhold_taxa")
        shinyjs::disable("hold_taxa")
    })
    
    observeEvent(input$unhold_taxa, {
        holds(holds()-1)
        shinyjs::enable("fam")
        shinyjs::enable("gen")
        shinyjs::enable("sp")
        shinyjs::enable("tax.rem")
        shinyjs::enable("taxa_nsp")
        shinyjs::enable("sp.def")
        updateSelectInput(session, "sp.def", selected = "yes")
        shinyjs::enable("hold_taxa")
        shinyjs::disable("unhold_taxa")
    })
    
# PAGE input new data: amount ---------------------------------------------
    output$i_abu <- renderUI(tagList(
        h3(r("i_abu.title"), align = "center", style = "font-size: 2em"),
        HTML("<br>"), 
        fluidRow(
            column(width = 3, numericInput("mmm", r("i_abu.mmm"), value = 0, min = 0, max = 299)),
            column(width = 3, numericInput("fff", r("i_abu.fff"), value = 0, min = 0, max = 299)),
            column(width = 3, numericInput("jjj", r("i_abu.jjj"), value = 0, min = 0, max = 299)),
            column(width = 3, textAreaInput("ind_rem", r("i_abu.rem"))),
        )
    ))
    
    
# PAGE input new data - combine! ------------------------------------------
    output$p_input.data <- renderUI(tagList(
        br(),
        uiOutput("i_auth"),
        hr(),
        # br(),
        # uiOutput("i_publ"),
        # br(),
        uiOutput("i_adm"),
        hr(),
        uiOutput("i_geo"),
        hr(),
        uiOutput("i_event"),
        hr(),
        uiOutput("i_taxa"),
        hr(),
        uiOutput("i_abu"),
        hr(),
        fluidRow(
            column(width = 4),
            column(width = 2, actionButton("check", r("i_check"), width = "80%", style="float:left")),
            column(width = 2, actionButton("record", r("i_record"), width = "80%", style="float:left")),
            column(width = 2),
            column(width = 2, actionButton("drop", r("i_drop"), width = "80%", style="float:left"))
        ),
        br(),
        hr(),
        br()
    ))

    

# NAVBAR ------------------------------------------------------------------
    output$NAVBAR <- renderUI(tagList(
        navbarPage(
        title = tags$div(style="position: relative; margin-right: 90px", 
                         tags$img(src="logo_placeholder.svg", height = "70px"),
                         tags$p(style="position: relative; top: -70px; left: 90px; ", 
                                "Faunistica 2.0")
        ),
        windowTitle = "Faunistica 2.0",
        position = "fixed-top",
        tabPanel(title = r("nv_home"), uiOutput("p_home")), 
        navbarMenu(r("nv_about"), 
                   tabPanel(r("nv_team"), uiOutput("p_team")),
                   "----",
                   r("nv_for_volunteers"),
                   tabPanel(r("nv_profit_science"), uiOutput("p_sci.profit")), 
                   tabPanel(r("nv_profit_personal"),   uiOutput("p_your.profit")), 
                   tabPanel(r("nv_howtohelp"),   uiOutput("p_howtohelp")), 
                   tabPanel(r("nv_voluntary_project"), uiOutput("p_our.project")), 
                   "----",
                   r("nv_for_scientists"),
                   tabPanel(r("nv_cooperation"), uiOutput("p_cooperation")), 
                   tabPanel(r("nv_web_app"), uiOutput("p_web.app")), 
                   tabPanel(r("nv_scientific_project"), h4("to be filled soon...")),
                   "----"
        ),
        tabPanel(r("nv_statistics"), uiOutput("p_statistics") 
                 
        ),
        tabPanel(r("nv_participate"), uiOutput("p_input.data")) 
        )
    ))
    

# Initial actioins --------------------------------------------------------
    # shinyjs::disable("deauth", asis = TRUE)

    # shinyjs::disable("unhold_adm")
    # shinyjs::disable("unhold_geo")
    # shinyjs::disable("unhold_ev")
    # shinyjs::disable("unhold_taxa")
    
    
}

# UI ----------------------------------------------------------------------
ui <- fluidPage(
    shinyjs::useShinyjs(),
    tags$style(type="text/css", "body {padding-top: 70px;}"),
    tags$head(tags$link(rel="shortcut icon", 
                        href="icons8-favicon-96.png")),
    uiOutput("NAVBAR")
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